library(plyr)
library(dplyr)

library(pxweb)
library(pxR)
library(RCurl)
library(stringr)
library(reshape2)

library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)

working.directory<-"/Users/jhimberg/Projects/trafiOpenData"
setwd(working.directory)

# Tällä voisi hakea Paavo datat rajapintapalvelusta Haetaan Paavo-datat

hae.paavo <-function(versio=2016) get_pxweb_data(
  url = paste("http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/",versio,"/paavo_9_koko_",versio,".px",sep=""),
  dims = list(Postinumeroalue = c('*'), Tiedot = c('*')), clean = TRUE) %>% 
  filter(Postinumeroalue!="KOKO MAA") %>%
  mutate(tiedot=gsub(";|,|\\(|\\)","",Tiedot), 
         i=gsub(",.*$","",Tiedot),versio=versio)

paavo.data<-rbind(hae.paavo(2016),hae.paavo(2015))  

paavo<-mutate(paavo.data, 
                   kunta=gsub("^.*\\(","",Postinumeroalue), 
                   kunta=gsub(")$","",kunta),
                   pono=substring(Postinumeroalue,1,5),
                   pono.alue=gsub("^[0-9]{5}","",Postinumeroalue),
                   pono.alue=str_trim(sub("\\(.*\\)$","",pono.alue))) %>%
  select(pono,kunta,pono.alue,tieto=tiedot,value=values,versio)

pono<-filter(paavo,versio==2016) %>% 
  select(pono,nimi=pono.alue,kunta) %>% 
  mutate(num=as.numeric(pono),
         kunta=str_trim(kunta)) %>% 
  unique

# Haetan joitakin kunnittaisia ikätietoja

vaesto<-rbind(read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/048_vaerak_tau_203.px"))) %>% 
  as.data.frame %>% 
  filter(., Alue !="KOKO MAA"),
  read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/073_vaerak_tau_109_fi.px"))) %>% 
  as.data.frame %>%
  mutate(.,Tunnusluku=paste("Ikä",mapvalues(Ikä,c("Ikäluokat yhteensä","100 -"),c("yht","100+")),mapvalues(Sukupuoli,c("Sukupuolet yhteensä","Miehet","Naiset"),c("M+N","M","N")),sep=".")) %>% 
  select(-Ikä,-Sukupuoli)) %>%
  arrange(Vuosi,Alue,Tunnusluku) %>% 
  mutate(Vuosi=levels(Vuosi)[as.numeric(Vuosi)],
         Alue=as.character(Alue))

ikaluokka.map<-
  function(v) as.numeric(mapvalues(as.character(v), 
                                   c("0-4",  "5-9",  "10-14","15-19","20-24",
                                     "25-29","30-34","35-39","40-44","45-49",
                                     "50-54","55-59","60-64","65-69","70-74",
                                     "75-79","80-84","85-89","90-"),
                                   c(2,       7,      12,    17,      22,
                                     27,      32,     37,    42,      47,
                                     52,      57,     62,    67,      72,
                                     77,      82,     87,    91)))


###

kartta.kuntanimet <- sp2df(get_mml(map.id="Yleiskartta-4500",data="KarttanimiPiste2000"))
kartta.kunnat <- sp2df(get_mml(map.id="Yleiskartta-4500", data.id="HallintoAlue"))
# nimet:

kml.file="Data/Postinumerot 20150102.kml" 
layer.name <- rgdal::ogrListLayers(kml.file)
pnro.sp <- rgdal::readOGR(dsn = kml.file, layer = layer.name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)

pono.geo<-sp2df(pnro.sp) %>% 
  mutate(pono.3=substr(pnro,1,3),
         pono.2=substr(pnro,1,2),
         pono.1=substr(pnro,1,1))

## Muokataan kuntanimiä vastaamaan vuoden 2016 tilannetta

#kartta.kunta<-ungroup(kartta.kunnat) %>% mutate(
#  Kunta_ni1=as.character(iconv(Kunta_ni1,to="UTF-8")),
#  Kunta_ni2=as.character(iconv(Kunta_ni2,to="UTF-8")),
#  kunta=ifelse((Kieli_ni1=="Ruotsi" & Kieli_ni2!="N_A"),Kunta_ni2,Kunta_ni1)) %>%
#  select(id,long,lat,order,hole,piece,group,maakunta=Maaku_ni1,kunta.id=Kunta,kunta) %>%
#  mutate(kunta=mapvalues(kunta,
#                         c("Jalasjärvi","Hämeenkoski","Köyliö","Lavia","Maaninka","Nastola","Tarvasjoki","Pedersöre"),
#                         c("Kurikka","Hollola","Säkylä","Pori","Kuopio","Lahti","Lieto","Pedersören kunta")))

get.geo <-function(data.name="tilastointialueet:kunta1000k_2016") {
  temp <- tempdir()
  data.file=paste(temp,"/",str_split_fixed(data.name,pattern=":",n=2)[2],sep="")
  url.head <- "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName="
  url.tail <- "&outputFormat=SHAPE-ZIP"
  zip.file <- paste(temp,"/","shape.zip",sep="")
  curl_download(paste(url.head,data.name, url.tail,sep=""), zip.file)
  unzip(zip.file)  
  return(sp2df(readShapeSpatial(paste(temp,"/",str_split_fixed(data.name,pattern=":",n=2)[2],sep=""))))
  }

kunta.geo<-get.geo("tilastointialueet:kunta1000k_2016") %>%
  mutate(nimi=iconv(nimi,from="latin1",to="UTF-8")) %>%
  rename(kunta.nro=kunta,kunta=nimi) %>% 
  select(-namn,name)

####
paavo.geo<-rbind(get.geo("postialue:pno_tilasto_2016"),
             get.geo("postialue:pno_tilasto_2015"))
       
paavo.data<-group_by(paavo,posti_alue,vuosi) %>% 
slice(1) %>% 
ungroup %>% 
select(-id,-long,-lat,-order,-hole,-piece,-group,-namn) %>% 
mutate(nimi=iconv(nimi,from="latin1",to="UTF-8")) %>% 
mutate_if(is.numeric,function(x) ifelse(x==-1,NA,x)) %>% rename(pono=postialue)



#### Lasketaan keskiarvot ja summat 2016 paavo datalle pono3 ja pono3

paavo.muuttujat<-list()
paavo.muuttujat[["2016"]]<-filter(paavo,versio==2016) %>% .$tieto %>% unique
paavo.muuttujat[["2015"]]<-filter(paavo,versio==2015) %>% .$tieto %>% unique

summattavat<-c(
  "Postinumeroalueen pinta-ala",                                                                                                               
  "Asukkaat yhteensä 2014 HE",                                                                                                                 
  "Naiset 2014 HE",                                                                                                                            
  "Miehet 2014 HE",                                                                                                                            
  "0-2-vuotiaat 2014 HE",                                                                                                                      
  "3-6-vuotiaat 2014 HE",                                                                                                                      
  "7-12-vuotiaat 2014 HE",                                                                                                                     
  "13-15-vuotiaat 2014 HE",                                                                                                                    
  "16-17-vuotiaat 2014 HE",                                                                                                                    
  "18-19-vuotiaat 2014 HE",                                                                                                                    
  "20-24-vuotiaat 2014 HE",                                                                                                                    
  "25-29-vuotiaat 2014 HE",                                                                                                                    
  "30-34-vuotiaat 2014 HE",                                                                                                                    
  "35-39-vuotiaat 2014 HE",                                                                                                                    
  "40-44-vuotiaat 2014 HE",                                                                                                                    
  "45-49-vuotiaat 2014 HE",                                                                                                                    
  "50-54-vuotiaat 2014 HE",                                                                                                                    
  "55-59-vuotiaat 2014 HE",                                                                                                                    
  "60-64-vuotiaat 2014 HE",                                                                                                                    
  "65-69-vuotiaat 2014 HE",                                                                                                                    
  "70-74-vuotiaat 2014 HE",                                                                                                                    
  "75-79-vuotiaat 2014 HE",                                                                                                                    
  "80-84-vuotiaat 2014 HE",                                                                                                                    
  "85 vuotta täyttäneet 2014 HE",                                                                                                              
  "18 vuotta täyttäneet yhteensä 2013 KO",                                                                                                     
  "Perusasteen suorittaneet 2013 KO",                                                                                                          
  "Koulutetut yhteensä 2013 KO",                                                                                                               
  "Ylioppilastutkinnon suorittaneet 2013 KO",                                                                                                  
  "Ammatillisen tutkinnon suorittaneet 2013 KO",                                                                                               
  "Alemman korkeakoulututkinnon suorittaneet 2013 KO",                                                                                         
  "Ylemmän korkeakoulututkinnon suorittaneet 2013 KO",                                                                                         
  "18 vuotta täyttäneet yhteensä 2013 HR",                                                                                                     
  "Alimpaan tuloluokkaan kuuluvat asukkaat 2013 HR",                                                                                           
  "Keskimmäiseen tuloluokkaan kuuluvat asukkaat 2013 HR",                                                                                      
  "Ylimpään tuloluokkaan kuuluvat asukkaat 2013 HR",                                                                                           
  "Asukkaiden ostovoimakertymä 2013 HR",                                                                                                       
  "Taloudet yhteensä 2014 TE",                                                                                                                 
  "Nuorten yksinasuvien taloudet 2014 TE",                                                                                                     
  "Lapsettomat nuorten parien taloudet 2014 TE",                                                                                               
  "Lapsitaloudet 2014 TE",                                                                                                                     
  "Pienten lasten taloudet 2014 TE",                                                                                                           
  "Alle kouluikäisten lasten taloudet 2014 TE",                                                                                                
  "Kouluikäisten lasten taloudet 2014 TE",                                                                                                     
  "Teini-ikäisten lasten taloudet 2014 TE",                                                                                                    
  "Aikuisten taloudet 2014 TE",                                                                                                                
  "Eläkeläisten taloudet 2014 TE",                                                                                                             
  "Omistusasunnoissa asuvat taloudet 2014 TE",                                                                                                 
  "Vuokra-asunnoissa asuvat taloudet 2014 TE",                                                                                                 
  "Muissa asunnoissa asuvat taloudet 2014 TE",                                                                                                 
  "Taloudet yhteensä 2013 TR",                                                                                                                 
  "Alimpaan tuloluokkaan kuuluvat taloudet 2013 TR",                                                                                          
  "Keskimmäiseen tuloluokkaan kuuluvat taloudet 2013 TR",                                                                                      
  "Ylimpään tuloluokkaan kuuluvat taloudet 2013 TR",                                                                                           
  "Talouksien ostovoimakertymä 2013 TR",                                                                                                       
  "Kesämökit yhteensä 2014 RA",                                                                                                                
  "Rakennukset yhteensä 2014 RA",                                                                                                              
  "Muut rakennukset yhteensä 2014 RA",                                                                                                         
  "Asuinrakennukset yhteensä 2014 RA",                                                                                                         
  "Asunnot 2014 RA",                                                                                                                           
  "Pientaloasunnot 2014 RA",                                                                                                                   
  "Kerrostaloasunnot 2014 RA",                                                                                                                 
  "Työpaikat yhteensä 2013 TP",                                                                                                                
  "Alkutuotannon työpaikat 2013 TP",                                                                                                           
  "Jalostuksen työpaikat 2013 TP",                                                                                                             
  "Palveluiden työpaikat 2013 TP",                                                                                                             
  "A Maatalous metsätalous ja kalatalous 2013 TP",                                                                                             
  "B Kaivostoiminta ja louhinta 2013 TP",                                                                                                      
  "C Teollisuus 2013 TP",                                                                                                                      
  "D Sähkö- kaasu- ja lämpöhuolto jäähdytysliiketoiminta 2013 TP",                                                                             
  "E Vesihuolto viemäri- ja jätevesihuolto ja muu ympäristön puhtaanapito 2013 TP",                                                            
  "F Rakentaminen 2013 TP",                                                                                                                    
  "G Tukku- ja vähittäiskauppa moottoriajoneuvojen ja moottoripyörien korjaus 2013 TP",                                                        
  "H Kuljetus ja varastointi 2013 TP",                                                                                                         
  "I Majoitus- ja ravitsemistoiminta 2013 TP",                                                                                                 
  "J Informaatio ja viestintä 2013 TP",                                                                                                        
  "K Rahoitus- ja vakuutustoiminta 2013 TP",                                                                                                   
  "L Kiinteistöalan toiminta 2013 TP",                                                                                                         
  "M Ammatillinen tieteellinen ja tekninen toiminta 2013 TP",                                                                                  
  "N Hallinto- ja tukipalvelutoiminta 2013 TP",                                                                                                
  "O Julkinen hallinto ja maanpuolustus pakollinen sosiaalivakuutus 2013 TP",                                                                  
  "P Koulutus 2013 TP",                                                                                                                        
  "Q Terveys- ja sosiaalipalvelut 2013 TP",                                                                                                    
  "R Taiteet viihde ja virkistys 2013 TP",                                                                                                     
  "S Muu palvelutoiminta 2013 TP",                                                                                                             
  "T Kotitalouksien toiminta työnantajina kotitalouksien eriyttämätön toiminta tavaroiden ja palveluiden tuottamiseksi omaan käyttöön 2013 TP",
  "U Kansainvälisten organisaatioiden ja toimielinten toiminta 2013 TP",                                                                       
  "X Toimiala tuntematon 2013 TP",                                                                                                             
  "Asukkaat yhteensä 2013 PT",                                                                                                                 
  "Työvoima 2013 PT",                                                                                                                          
  "Työlliset 2013 PT",                                                                                                                         
  "Työttömät 2013 PT",                                                                                                                         
  "Työvoiman ulkopuolella olevat 2013 PT",                                                                                                     
  "Lapset 0-14 -vuotiaat 2013 PT",                                                                                                             
  "Opiskelijat 2013 PT",                                                                                                                       
  "Eläkeläiset 2013 PT",         
  "Muut 2013 PT")

keskiarvoistettavat <- c(
  "Postinumeroalueen pinta-ala",
  "X-koordinaatti metreinä",
  "Y-koordinaatti metreinä",
  "Asukkaiden keski-ikä 2014 HE",   
  "Asukkaiden keskitulot 2013 HR",                                                                                                             
  "Talouksien keskitulot 2013 TR",                                                                                                             
  "Talouksien keskikoko 2014 TE",                                                                                                              
  "Asumisväljyys 2014 TE",           
  "Asuntojen keskipinta-ala 2014 RA",
  "Asukkaiden mediaanitulot 2013 HR",
  "Talouksien mediaanitulot 2013 TR",
  "Asukkaat yhteensä 2014 HE",
  "18 vuotta täyttäneet yhteensä 2013 HR",                                                                                                               
  "Taloudet yhteensä 2013 TR",                                                                                                              
  "Taloudet yhteensä 2014 TE",
  "Asuinrakennukset yhteensä 2014 RA",
  "18 vuotta täyttäneet yhteensä 2013 HR")


paavo.summa.3<-filter(paavo, tieto %in% summattavat) %>% 
  group_by(tieto, pono.3=substring(pono,1,3)) %>% summarise_if(is.numeric,sum,na.rm=TRUE) %>%
  dcast(.,pono.3~tieto,value.var="value",fill=NA)

paavo.summa.2<-filter(paavo, tieto %in% summattavat) %>% 
  group_by(tieto, pono.2=substring(pono,1,2)) %>% summarise_if(is.numeric,sum,na.rm=TRUE) %>%
  dcast(.,pono.2~tieto,value.var="value",fill=NA)

w.mean<-function(a,b) {
  ix<-!is.na(a) & !is.na(b)
  return(sum(a[ix]*b[ix])/sum(b[ix]))
}

paavo.ka.3<-filter(paavo, tieto %in% keskiarvoistettavat) %>% 
  dcast(.,pono~tieto,value.var="value",fill=NA) %>% 
  group_by(pono.3=substring(pono,1,3)) %>%
  summarise(`X-koordinaatti metreinä`=w.mean(`X-koordinaatti metreinä`,`Postinumeroalueen pinta-ala`),
            `Y-koordinaatti metreinä`=w.mean(`Y-koordinaatti metreinä`,`Postinumeroalueen pinta-ala`),
            `Asukkaiden keski-ikä 2014 HE`=w.mean(`Asukkaiden keski-ikä 2014 HE`,`Asukkaat yhteensä 2014 HE`),
            `Asukkaiden keskitulot 2013 HR`=w.mean(`Asukkaiden keskitulot 2013 HR`,`18 vuotta täyttäneet yhteensä 2013 HR`),
            `Talouksien keskitulot 2013 TR`=w.mean(`Talouksien keskitulot 2013 TR`,`Taloudet yhteensä 2013 TR`),
            `Talouksien keskikoko 2014 TE`=w.mean(`Talouksien keskikoko 2014 TE`,`Taloudet yhteensä 2014 TE`),
            `Asumisväljyys 2014 TE`=w.mean(`Asumisväljyys 2014 TE`,`Taloudet yhteensä 2014 TE`),
            `Asuntojen keskipinta-ala 2014 RA`=w.mean(`Asuntojen keskipinta-ala 2014 RA`,`Asuinrakennukset yhteensä 2014 RA`),
            `Asukkaiden mediaanitulot 2013 HR`=w.mean(`Asukkaiden mediaanitulot 2013 HR`,`18 vuotta täyttäneet yhteensä 2013 HR`),
            `Talouksien mediaanitulot 2013 TR`=w.mean(`Talouksien mediaanitulot 2013 TR`,`Taloudet yhteensä 2013 TR`))

paavo.pono.3<-left_join(paavo.summa.3, paavo.ka.3,by="pono.3") %>% 
  select(pono.3, one_of(paavo.muuttujat$"2016"))

paavo.ka.2<-filter(paavo, tieto %in% keskiarvoistettavat) %>% 
  dcast(.,pono~tieto,value.var="value",fill=NA) %>% 
  group_by(pono.2=substring(pono,1,2)) %>%
  summarise(`X-koordinaatti metreinä`=w.mean(`X-koordinaatti metreinä`,`Postinumeroalueen pinta-ala`),
            `Y-koordinaatti metreinä`=w.mean(`Y-koordinaatti metreinä`,`Postinumeroalueen pinta-ala`),
            `Asukkaiden keski-ikä 2014 HE`=w.mean(`Asukkaiden keski-ikä 2014 HE`,`Asukkaat yhteensä 2014 HE`),
            `Asukkaiden keskitulot 2013 HR`=w.mean(`Asukkaiden keskitulot 2013 HR`,`18 vuotta täyttäneet yhteensä 2013 HR`),
            `Talouksien keskitulot 2013 TR`=w.mean(`Talouksien keskitulot 2013 TR`,`Taloudet yhteensä 2013 TR`),
            `Talouksien keskikoko 2014 TE`=w.mean(`Talouksien keskikoko 2014 TE`,`Taloudet yhteensä 2014 TE`),
            `Asumisväljyys 2014 TE`=w.mean(`Asumisväljyys 2014 TE`,`Taloudet yhteensä 2014 TE`),
            `Asuntojen keskipinta-ala 2014 RA`=w.mean(`Asuntojen keskipinta-ala 2014 RA`,`Asuinrakennukset yhteensä 2014 RA`),
            `Asukkaiden mediaanitulot 2013 HR`=w.mean(`Asukkaiden mediaanitulot 2013 HR`,`18 vuotta täyttäneet yhteensä 2013 HR`),
            `Talouksien mediaanitulot 2013 TR`=w.mean(`Talouksien mediaanitulot 2013 TR`,`Taloudet yhteensä 2013 TR`))

paavo.pono.2<-left_join(paavo.summa.2, paavo.ka.2,by="pono.2") %>% 
  select(pono.2, one_of(paavo.muuttujat$"2016"))

kartta.pono<-rename(kartta.pono,pono=pnro)

save(file="vaesto.RData", 
     paavo, 
     pono, 
     kunta.vaesto.ika, 
     kartta.kunta, 
     kartta.pono, 
     kartta.kuntanimet,
     paavo2016.5,
     paavo2016.3,
     paavo2016.2)



read.px(textConnection(getURL("read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/073_vaerak_tau_109_fi.px"))) %>% 