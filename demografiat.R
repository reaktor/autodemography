library(plyr)
library(dplyr)

library(pxweb)
library(pxR)
library(RCurl)
library(curl)
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

vaesto<-rbind(read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/048_vaerak_tau_203.px"))) %>% 
  as.data.frame,
  read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/073_vaerak_tau_109_fi.px"))) %>% 
  as.data.frame %>%
  mutate(.,Tunnusluku=paste("Ikä",mapvalues(Ikä,c("Ikäluokat yhteensä","100 -"),c("yht","100+")),mapvalues(Sukupuoli,c("Sukupuolet yhteensä","Miehet","Naiset"),c("M+N","M","N")),sep=".")) %>% 
  select(-Ikä,-Sukupuoli)) %>%
  arrange(Vuosi,Alue,Tunnusluku) %>% 
  mutate(Vuosi=levels(Vuosi)[as.numeric(Vuosi)],
         Alue=as.character(Alue))

## nimet
## Duukkiksen redusoitu postinumerokattadata

kml.file="Data/Postinumerot 20150102.kml" 
layer.name <- rgdal::ogrListLayers(kml.file)
pnro.sp <- rgdal::readOGR(dsn = kml.file, layer = layer.name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)

pono.geo<-sp2df(pnro.sp) %>% 
  mutate(pono.3=substr(pnro,1,3),
         pono.2=substr(pnro,1,2),
         pono.1=substr(pnro,1,1)) %>%
  rename(pono=pnro)

## Hateaan kuntarajakartat 2014-2016

kunta.geo <- c()
name.ext<-list() 
name.ext[["2014"]]<-"Polygon.shp"
name.ext[["2015"]]<-".shp"
name.ext[["2016"]]<-".shp"
for (y in c("2014","2015","2016")) 
  kunta.geo <- rbind(kunta.geo,
                     get.geo(paste("tilastointialueet:kunta1000k_",y,sep=""), name.ext=name.ext[[y]]) %>% 
                       mutate(nimi=iconv(nimi,from="latin1",to="UTF-8")) %>%
                       rename(kuntano=kunta,kunta=nimi) %>% 
                       select(id,long,lat,order,hole,piece,group,vuosi,kuntano,kunta))

## Muokataan kuntanimiä vastaamaan vuoden 2016 tilannetta
#                         c("Jalasjärvi","Hämeenkoski","Köyliö","Lavia","Maaninka","Nastola","Tarvasjoki","Pedersöre"),
#                         c("Kurikka","Hollola","Säkylä","Pori","Kuopio","Lahti","Lieto","Pedersören kunta")))

#### Postinumeroaluedatat ja tarkka postinumeroaluekartta

paavo.geo<-rbind(get.geo("postialue:pno_tilasto_2016"),
             get.geo("postialue:pno_tilasto_2015"))
       
paavo.data<-group_by(paavo.geo,posti_alue,vuosi) %>% 
slice(1) %>% 
ungroup %>% 
select(-id,-long,-lat,-order,-hole,-piece,-group,-namn) %>% 
mutate(nimi=iconv(nimi,from="latin1",to="UTF-8")) %>% 
mutate_if(is.numeric,function(x) ifelse(x==-1,NA,x)) %>% 
  rename(pono=posti_alue, kuntano=kunta) %>% 
  mutate(pono.3=substr(pnro,1,3),
         pono.2=substr(pnro,1,2),
         pono.1=substr(pnro,1,1)) %>% 

[1] "pono"       "nimi"       "euref_x"    "euref_y"    "pinta_ala"  "vuosi"      "kunta"     
[8] "he_vakiy"   "he_naiset"  "he_miehet"  "he_kika"    "he_0_2"     "he_3_6"     "he_7_12"   
[15] "he_13_15"   "he_16_17"   "he_18_19"   "he_20_24"   "he_25_29"   "he_30_34"   "he_35_39"  
[22] "he_40_44"   "he_45_49"   "he_50_54"   "he_55_59"   "he_60_64"   "he_65_69"   "he_70_74"  
[29] "he_75_79"   "he_80_84"   "he_85_"     "ko_ika18y"  "ko_perus"   "ko_koul"    "ko_yliop"  
[36] "ko_ammat"   "ko_al_kork" "ko_yl_kork" "hr_tuy"     "hr_ktu"     "hr_mtu"     "hr_pi_tul" 
[43] "hr_ke_tul"  "hr_hy_tul"  "hr_ovy"     "te_taly"    "te_takk"    "te_as_valj" "te_nuor"   
[50] "te_eil_np"  "te_laps"    "te_plap"    "te_aklap"   "te_klap"    "te_teini"   "te_aik"    
[57] "te_elak"    "te_omis_as" "te_vuok_as" "te_muu_as"  "tr_kuty"    "tr_ktu"     "tr_mtu"    
[64] "tr_pi_tul"  "tr_ke_tul"  "tr_hy_tul"  "tr_ovy"     "ra_ke"      "ra_raky"    "ra_muut"   
[71] "ra_asrak"   "ra_asunn"   "ra_as_kpa"  "ra_pt_as"   "ra_kt_as"   "tp_tyopy"   "tp_alku_a" 
[78] "tp_jalo_bf" "tp_palv_gu" "tp_a_maat"  "tp_b_kaiv"  "tp_c_teol"  "tp_d_ener"  "tp_e_vesi" 
[85] "tp_f_rake"  "tp_g_kaup"  "tp_h_kulj"  "tp_i_majo"  "tp_j_info"  "tp_k_raho"  "tp_l_kiin" 
[92] "tp_m_erik"  "tp_n_hall"  "tp_o_julk"  "tp_p_koul"  "tp_q_terv"  "tp_r_taid"  "tp_s_muup" 
[99] "tp_t_koti"  "tp_u_kans"  "tp_x_tunt"  "pt_vakiy"   "pt_tyovy"   "pt_tyoll"   "pt_tyott"  
[106] "pt_tyovu"   "pt_0_14"    "pt_opisk"   "pt_elakel"  "pt_muut" 




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

kartta.kuntanimet <- sp2df(get_mml(map.id="Yleiskartta-4500",data="KarttanimiPiste2000"))
kartta.kunnat <- sp2df(get_mml(map.id="Yleiskartta-4500", data.id="HallintoAlue"))

read.px(textConnection(getURL("read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vaerak/073_vaerak_tau_109_fi.px"))) %>% 