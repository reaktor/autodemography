library(plyr)
library(dplyr)
library(stringr)
library(reshape2)

library(pxweb)
library(pxR)
library(RCurl)
library(curl)


library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)

working.directory<-"."
setwd(working.directory)

# Haetan joitakin kunnittaisia ikätietoja
## 

get.geo <-function(data.name="tilastointialueet:kunta1000k_2016",name.ext=".shp") {
  data.file=paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],sep="")
  url.head <- "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName="
  url.tail <- "&outputFormat=SHAPE-ZIP"
  zip.file <- paste(tempdir(),"/","shape.zip",sep="")
  curl_download(paste(url.head,data.name, url.tail,sep=""), zip.file)
  unzip(zip.file,exdir=tempdir())  
  return(sp2df(readShapeSpatial(paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],name.ext,sep=""))))
}

geo<-list()

## nimet
## Duukkiksen redusoitu postinumerokattadata


kml.file="Data/Postinumerot 20150102.kml" 
layer.name <- rgdal::ogrListLayers(kml.file)
pnro.sp <- rgdal::readOGR(dsn = kml.file, layer = layer.name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)

geo$pono.duukkis <- sp2df(pnro.sp) %>% 
  rename(pono=pnro)

## Hateaan kuntarajakartat 2014-2016

geo$kunta <- c()
name.ext <-list() 
name.ext[["2014"]]<-"Polygon.shp"
name.ext[["2015"]]<-".shp"
name.ext[["2016"]]<-".shp"
for (y in c("2014","2015","2016")) 
  geo$kunta <- rbind(geo$kunta,
                     get.geo(paste("tilastointialueet:kunta1000k_",y,sep=""), name.ext=name.ext[[y]]) %>% 
                       mutate(nimi=iconv(nimi,from="latin1",to="UTF-8")) %>%
                       rename(kuntano=kunta,kunta=nimi) %>% 
                       select(id,long,lat,order,hole,piece,group,vuosi,kuntano,kunta))

# Kunta

kunnat<-read.csv(file="Data/kunnat2016.csv",fileEncoding="MAC",sep=";")

geo$kunta.vanhat2uudet<-merge(select(kunnat,kunta,kunta.old,kuntano.old),
      select(kunnat,kunta=kunta.old,kuntano=kuntano.old),
      by="kunta") %>% 
  select(kunta.old,kunta,kuntano.old,kuntano) %>% 
  mutate(kuntano.old=str_pad(kuntano.old, 3, side="left", pad="0"),
         kuntano=str_pad(kuntano, 3, side="left", pad="0"))

#### Postinumeroaluedatat ja tarkka postinumeroaluekartta

geo$paikannimet <- sp2df(get_mml(map.id="Yleiskartta-4500",data="KarttanimiPiste2000")) %>%
filter(paikkaTyyp %in% c(540,550,560)) %>% group_by(paikkaID) %>% arrange(kieliKoodi) %>% slice(1) %>% ungroup %>% select(-taivutus,-suuntaDXP,-suuntaDYI,-kirjTyyppi,-kirjVari,-kallistus,-harvennus,-kieliViral,-kieliEnem) 

demografia<-list()

demografia$kunta.vaesto<-rbind(read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/048_vaerak_tau_203.px"))) %>% 
                as.data.frame,
              read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/073_vaerak_tau_109_fi.px"))) %>% 
                as.data.frame %>%
                mutate(.,Tunnusluku=paste("Ikä",mapvalues(Ikä,c("Ikäluokat yhteensä","100 -"),c("yht","100+")),mapvalues(Sukupuoli,c("Sukupuolet yhteensä","Miehet","Naiset"),c("M+N","M","N")),sep=".")) %>% 
                select(-Ikä,-Sukupuoli)) %>%
  arrange(Vuosi,Alue,Tunnusluku) %>% 
  mutate(Vuosi=levels(Vuosi)[as.numeric(Vuosi)],
         Alue=as.character(Alue))

paavo<-rbind(get.geo("postialue:pno_tilasto_2016"),
             get.geo("postialue:pno_tilasto_2015"))

demografia$vars <- read.csv(file="paavo.koodit.txt",sep="\t",fileEncoding="MAC",stringsAsFactors = FALSE)  

demografia$pono.5<-group_by(paavo,posti_alue,vuosi) %>% 
slice(1) %>% 
ungroup %>% 
select(-id,-long,-lat,-order,-hole,-piece,-group,-namn) %>% 
  rename(pono=posti_alue, kuntano=kunta) %>%
mutate(pono=as.character(pono),
       kuntano=as.character(kuntano),
       nimi=iconv(nimi,from="latin1",to="UTF-8")) %>% 
mutate_if(is.numeric,function(x) ifelse(x==-1,NA,x))

geo$pono.statfi<-select(paavo,id,long,lat,order,hole,piece,group,nimi,euref_x,euref_y,vuosi,pono=posti_alue)

wmean<-function(x,y) return(weighted.mean(x,ifelse(is.na(y),0,y),na.rm=T))

paavo.aggr <- function(d,i)
  group_by(d$pono.5, vuosi, pono=str_sub(pono,1,i)) %>% 
  select(pono,vuosi,one_of(filter(d$vars,aggr=="sum")$koodi)) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  left_join(.,
            group_by(d$pono.5,vuosi,pono=str_sub(pono,1,i)) %>% 
              summarise(he_kika=wmean(he_kika,he_vakiy),
                        hr_ktu=wmean(hr_ktu,hr_tuy),
                        hr_mtu=wmean(hr_mtu,hr_tuy),
                        te_takk=wmean(te_takk,te_taly),
                        te_as_valj=wmean(te_as_valj,te_taly),
                        tr_ktu=wmean(tr_ktu,tr_kuty),
                        tr_mtu=wmean(tr_mtu,tr_kuty),
                        ra_as_kpa=wmean(ra_as_kpa,ra_asunn)),
            by=c("vuosi","pono"))

demografia$pono.2<-paavo.aggr(demografia,2)
demografia$pono.3<-paavo.aggr(demografia,3)


#### Lasketaan keskiarvot ja summat 2016 paavo datalle pono3 ja pono3

save(file="vaesto.RData", 
     demografia,
     geo)


