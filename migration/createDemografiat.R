library(here)
library(dplyr)
library(tidyr)
library(stringr)

library(pxweb)
library(RCurl)
library(curl)

library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)

directory <- here()
setwd(directory)

# Haetan joitakin kunnittaisia ikätietoja
## 

get.geo <-function(data.name="tilastointialueet:kunta4500k_2017",name.ext=".shp") {
  data.file=paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],sep="")
  url.head <- "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName="
  url.tail <- "&outputFormat=SHAPE-ZIP"
  zip.file <- paste(tempdir(),"/","shape.zip",sep="")
  curl_download(paste(url.head,data.name, url.tail,sep=""), zip.file)
  unzip(zip.file,exdir=tempdir())  
  return(sp2df(readShapeSpatial(paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],name.ext,sep=""))))
}

geo<-list()

## Duukkiksen redusoitu postinumerokattadata haettu erikseen

kml.file="Data/Postinumerot 20150102.kml" 
layer.name <- rgdal::ogrListLayers(kml.file)
pnro.sp <- rgdal::readOGR(dsn = kml.file, layer = layer.name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)

geo$pono.duukkis <- sp2df(pnro.sp) %>% 
  rename(pono=pnro)

## Hateaan kuntarajakartat 4500k 2014-2017

geo$kunta <- list()
name.ext <-list() 
name.ext[["2017"]]<-".shp"
for (y in c("2017")) 
  geo$kunta[[y]] <- rbind(geo$kunta,
                     get.geo(paste("tilastointialueet:kunta4500k_",y,sep=""), 
                             name.ext=name.ext[[y]]) %>% 
                       mutate(nimi=iconv(nimi, from="latin1", to="UTF-8")) %>%
                       rename(kuntano=kunta,kunta=nimi) %>% 
                       select(id,long,lat,order,hole,piece,group,vuosi,kuntano,kuntanimi=kunta) %>%
                       mutate(kuntanimi=map.vanhat.kuntanimet(kuntanimi)))

# Vanhat kunnat vuoden 2017 kuntiin; tehty käsin tilastokeskuksen datasta
kunnat<-read.csv(file="Data/kunnat2017.csv",fileEncoding="MAC",sep=";")

# Kuntien koodaus vanhasta uuteen
geo$kunta.vanhat2uudet<-merge(select(kunnat,kunta,kunta.old,kuntano.old),
      select(kunnat,kunta=kunta.old,kuntano=kuntano.old),
      by="kunta") %>% 
  select(kunta.old,kunta,kuntano.old,kuntano) %>% 
  mutate(kuntano.old=str_pad(kuntano.old, 3, side="left", pad="0"),
         kuntano=str_pad(kuntano, 3, side="left", pad="0"))

demografia<-list()

## Kuntadatoja Tilastokeskuksesta
v.2017 <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_avainluvut/2017/kuntien_avainluvut_2017_aikasarja.px",
                 dims = list("Alue 2017" = c('*'),
                             Tiedot = c('*'),
                             Vuosi = c('*')),
                 clean = TRUE) %>%
  transmute(Alue=as.character(`Alue 2017`), 
            Tiedot=as.character(Tiedot),
            Vuosi=as.numeric(as.character((Vuosi))),
            value=values) %>% 
  filter(!grepl("region|maakunta|seutukunta|Kainuu|Ålands landsbyg|Ålands skärgård|Uusimaa|Varsinais-Suomi|Satakunta|Kanta-Häme|Pirkanmaa|Päijät-Häme|Kymenlaakso|Etelä-Karjala|Etelä-Savo|Pohjois-Savo|Pohjois-Karjala|Keski-Suomi|Etelä-Pohjanmaa|Pohjanmaa|Keski-Pohjanmaa|Pohjois-Pohjanmaa|Lappi|Ahvenanmaa - Åland",
                                  Alue, ignore.case=FALSE)) %>%
  mutate(Alue=map.vanhat.kuntanimet(Alue))

kunta.tunnusluvut.2 <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/048_vaerak_tau_203.px",
                 dims = list(Alue = c('*'),
                             Vuosi = c('*'),
                             Tunnusluku = c('*')),
                 clean = TRUE) %>% 
  transmute(Alue=as.character(Alue), 
            Tiedot=as.character(Tunnusluku),
            Vuosi=as.numeric(as.character((Vuosi))),
            value=values) %>% 
  filter(!grepl("MANNER|AHVENANMAA|Suomi|vaalipiiri|hovioikeus| kunnat|Ahvenanmaa|FI|Åland|SHP|käräjäoikeus|poliisilaitos|ELY|AVI|reg|virasto|region|maakunta|seutukunta|Kainuu|Ålands landsbyg|Ålands skärgård|Uusimaa|Varsinais-Suomi|Satakunta|Kanta-Häme|Pirkanmaa|Päijät-Häme|Kymenlaakso|Etelä-Karjala|Etelä-Savo|Pohjois-Savo|Pohjois-Karjala|Keski-Suomi|Etelä-Pohjanmaa|Pohjanmaa|Keski-Pohjanmaa|Pohjois-Pohjanmaa|Lappi|Ahvenanmaa - Åland",
                Alue, ignore.case=FALSE)) %>% 
  mutate(Alue=map.vanhat.kuntanimet(Alue)) %>% 
  filter(Tiedot %in% c( 
    "Väestöllinen huoltosuhde",
    "Keski-ikä, molemmat sukupuolet", 
    "Ulkomaalaistaustaisten osuus, %",
    "Ev.lut kirkkoon kuuluvien osuus, %",
    "Muihin uskontokuntiin kuuluvien osuus, %",
    "Uskontokuntiin kuulumattomien osuus, %")
  )
  
demografia$kunta$tunnusluku <- rbind(v.2017, kunta.tunnusluvut.2) %>% 
  mutate(Tiedot=plyr::mapvalues(Tiedot, "Keski-ikä, molemmat sukupuolet", "Keski-ikä")) %>%
  spread(., Tiedot, value, fill=NA) %>% 
  rename(vuosi=Vuosi, kuntanimi=Alue)

## Kunnallisvaalit

vaalit<-list() 
vaali<-list()
vaalit[[1]]=list(vaali="kunta2012",osoite="http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vaa/kvaa/2012_07/920_kvaa_2012_tau_161_fi.px")
vaalit[[2]]=list(vaali="kunta2017",osoite="http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vaa/kvaa/2017_07/920_kvaa_2017_tau_161.px")

for (v in vaalit) {
  vaali[[v$vaali]] <- 
    get_pxweb_data(url = v$osoite,
                   dims = list(Alue = c('*'),
                               Sukupuoli = c('S'),
                               Puolue = c('*'),
                               "Puolueiden kannatus" = c('Sar3')), clean = TRUE) 
  
  names(vaali[[v$vaali]])<-gsub(" ","\\.",names(vaali[[v$vaali]]))
  
  vaali[[v$vaali]] <- filter(vaali[[v$vaali]], 
                             Puolueiden.kannatus=="Ääniä yhteensä" & Puolue!="Yhteensä" & Sukupuoli=="Kaikki ehdokkaat" & grepl("^[0-9][0-9][0-9]",Alue)) %>% mutate(kuntano=substr(Alue,1,3),kuntanimi=substr(Alue,5,100)) %>% 
    select(Puolue, value=values, kuntano, kuntanimi) %>% 
    spread(.,Puolue,value,fill=0) %>% 
    mutate(kuntano=map.kuntano(kuntano)) %>% 
    select(-kuntanimi) %>% 
    group_by(kuntano) %>% 
    summarise_all(funs(sum)) %>% 
    ungroup 

  
  vaali[[v$vaali]]$N<-rowSums(select(vaali[[v$vaali]],-kuntano))
}

demografia$kunta$vaali <- vaali

#
#### Postinumeroaluedatat ja tarkka postinumeroaluekartta

paavo<-bind_rows(get.geo("postialue:pno_tilasto_2018"), 
             get.geo("postialue:pno_tilasto_2017"),
             get.geo("postialue:pno_tilasto_2016"),
             get.geo("postialue:pno_tilasto_2015"))

# Paavo-datojen muutajanimiä jne. muodostettu käsin
paavo.vars <- read.csv(file="Data/paavo.koodit.txt", 
                       sep=";",
                       fileEncoding="MAC",
                       stringsAsFactors = FALSE)  

geo$pono.statfi.2017<-select(paavo,id,long,lat,order,hole,piece,group,nimi,euref_x,euref_y,vuosi,pono=posti_alue) %>%
  filter(vuosi==2017)

paavo.5<-group_by(paavo,posti_alue,vuosi) %>% 
slice(1) %>% 
ungroup %>% 
select(-id,-long,-lat,-order,-hole,-piece,-group,-namn) %>% 
  rename(pono=posti_alue, kuntano=kunta) %>%
mutate(pono=as.character(pono), pono.level=5,
       kuntano=as.character(kuntano),
       nimi=iconv(nimi,from="latin1",to="UTF-8")) %>% 
mutate_if(is.numeric,function(x) ifelse(x==-1,NA,x)) %>% select(-objectid)

# Painotus väkiluvun mukaan
wmean<-function(x,y) return(weighted.mean(x,ifelse(is.na(y),0,y),na.rm=T))

paavo.aggr <- function(d, i, vars=paavo.vars)
  group_by(d, vuosi, pono=str_sub(pono,1,i)) %>% 
  select(pono, vuosi, one_of(filter(vars, aggr=="sum")$koodi)) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  left_join(.,
            group_by(d, vuosi,pono=str_sub(pono,1,i)) %>% 
              summarise(he_kika=wmean(he_kika,he_vakiy),
                        hr_ktu=wmean(hr_ktu,hr_tuy),
                        hr_mtu=wmean(hr_mtu,hr_tuy),
                        te_takk=wmean(te_takk,te_taly),
                        te_as_valj=wmean(te_as_valj,te_taly),
                        tr_ktu=wmean(tr_ktu,tr_kuty),
                        tr_mtu=wmean(tr_mtu,tr_kuty),
                        ra_as_kpa=wmean(ra_as_kpa,ra_asunn),
                        euref_x=wmean(euref_x,pinta_ala),
                        euref_y=wmean(euref_y,pinta_ala),
                        kuntano=NA,
                        pono.level=i,
                        nimi=NA
                        ),
            by=c("vuosi","pono")) %>% 
  as.data.frame

paavo.aggr.kunta.pono <- function(d, i, vars=paavo.vars)
  group_by(d, vuosi, pono=str_sub(pono,1,i), kuntano) %>% 
  select(pono, vuosi, one_of(filter(vars, aggr=="sum")$koodi), kuntano) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  left_join(.,
            group_by(d, vuosi, pono=str_sub(pono,1,i), kuntano) %>% 
              summarise(he_kika=wmean(he_kika,he_vakiy),
                        hr_ktu=wmean(hr_ktu,hr_tuy),
                        hr_mtu=wmean(hr_mtu,hr_tuy),
                        te_takk=wmean(te_takk,te_taly),
                        te_as_valj=wmean(te_as_valj,te_taly),
                        tr_ktu=wmean(tr_ktu,tr_kuty),
                        tr_mtu=wmean(tr_mtu,tr_kuty),
                        ra_as_kpa=wmean(ra_as_kpa,ra_asunn),
                        euref_x=wmean(euref_x,pinta_ala),
                        euref_y=wmean(euref_y,pinta_ala),
                        pono.level=i*10+i,
                        nimi=NA
              ),
            by=c("vuosi","pono", "kuntano")) %>% 
  as.data.frame


### Lasketaan keskiarvot ja summat Paavo datalle pono3 ja pono3

demografia$postinumero$data<-bind_rows(paavo.5,paavo.aggr(paavo.5,3),
                             paavo.aggr(paavo.5,2),
                             paavo.aggr(paavo.5,1),
                             paavo.aggr.kunta.pono(paavo.5,3),
                             paavo.aggr.kunta.pono(paavo.5,2))

demografia$postinumero$vars<-paavo.vars

save(file="Data/demografia.2.RData", demografia)
save(file="Data/geografia.RData", geo)
