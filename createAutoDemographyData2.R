# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("~/Projects/autodemography/initTrafi.R")

# Ajetaan merkkien/mallien korjaustiedosto!! luodaan skriptillä createMallitMerkitkorjaus.R
# (Voidaan editoida käsin)
library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)
library(RColorBrewer)
library(lme4)
library(corrplot)
library(gganimate)
library(ggiraph)

##

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

#group_by(henkiloauto.historia %>% filter(ajoneuvonkaytto=="Yksityinen"), data, kuntanimi) %>% summarise(uusi=sum(rekisterivuodet<=0.25,na.rm=TRUE), N=n()) %>% ungroup %>% transmute(alue=kuntanimi, uusi=uusi/N, data) 
#kartta(filter(i, !is.ahvenanmaa(alue) & data=="4.02"), aluejako="kartogrammi.kuntanimi", color.map="BrBG")

henkiloauto <- tbl(trafi.db,"henkiloauto_uniqcombos") %>% 
  collect(n=Inf) %>% 
  mutate(merkki.orig=merkkiSelvakielinen, malli.orig=kaupallinenNimi) %>%
  fix.auto %>%
  fix.merkki.malli %>% 
  fix.kori %>% 
  fix.co2 %>%
  select(combo,  N.combo, kayttoonottoVuosi, ensirekVuosi, ajoneuvoluokka, ajoneuvoryhma, vari, ryhma, korityyppi, kori.orig, kori.est, kori, kayttovoima, polttoaine, omamassa, 
         iskutilavuus, suurinNettoteho, sahkohybridi, merkki.orig, malli.orig,
         mallimerkinta, merkki, malli, l.malli,  merkki.l.malli, Co2.orig, Co2.modelled, Co2)

# Laatu
#group_by(henkiloauto, kori.orig=toupper(kori.orig), kori.est) %>% summarise(N=sum(N.combo)) %>% spread(.,key=kori.orig,N) %>% View
#filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig<20)) %>% mutate(d=abs(Co2.orig-Co2.modelled)) %>% ggplot(aes(x=Co2.orig,y=d))+geom_smooth()
#filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig<20)) %>% mutate(d=abs(Co2.orig-Co2.modelled), D=abs(Co2.orig-Co2.modelled)/(Co2.modelled)) %>% .$D  %>% 
#quantile(.,c(.95,.99,1), na.rm=TRUE)
#mean(abs(henkiloauto$Co2.orig-henkiloauto$Co2.modelled),na.rm=TRUE)

henkiloauto.historia <-tbl(trafi.db,"henkiloauto_historia_diff") %>% 
  select(-km.diff) %>%
  collect(n=Inf) 

henkiloauto.historia <- left_join(henkiloauto.historia, 
                                  select(henkiloauto, 
                                         combo, 
                                         kayttoonottoVuosi, 
                                         ensirekVuosi, 
                                         vari, 
                                         ryhma, 
                                         kori, 
                                         polttoaine, 
                                         omamassa,
                                         suurinNettoteho, 
                                         merkki, 
                                         merkki.l.malli, 
                                         Co2), 
                                  by="combo") %>% 
  mutate(ryhma=plyr::mapvalues(ryhma, 
                               c("Henkilö","Maasto"), 
                               c("Normaali","Normaali"))) 

s<-auto.stat(filter(henkiloauto.historia, ryhma=="Normaali"))


henkiloauto.historia<-select(henkiloauto.historia, 
                             -katsastus.Q, 
                             -matkamittari.date.laatu, 
                             -ensirekisterointipvm, 
                             -kayttoonottopvm,
                             -ensirekVuosi) %>%

mutate(
          uusi=ifelse(kayttovuodet<=0.25,T,F),
          polttoaine=ifelse(polttoaine %in% c("Bensiini","Diesel","Sähkö"), polttoaine, "muupolttoaine"),
          merkki.orig=merkki,
          merkki=ifelse(merkki %in% s$merkki$merkki[1:80], merkki, "muu"), 
          malli=ifelse(merkki.l.malli %in% s$merkki.l.malli$merkki.l.malli[1:100], 
                       merkki.l.malli, ifelse(merkki != "muumerkki", merkki, "muu")),
          malli.kori=paste(malli,tolower(kori)),
          pono.2=str_sub(pono.3,1,2), 
          rekisteriero.v=kayttovuodet-rekisterivuodet)

#auto.stat(filter(henkiloauto.historia, uusi & ryhma=="Normaali"))


  trafi.freq <- function(h, attr="merkki", base=c("data", "pono.3", "ryhma", "ajoneuvonkaytto")) {
    r<-list()
    r$N<-count.normalized(h, attr=attr, base=base) %>% 
    select(., one_of(c(base, attr, "N", "sum.N"))) %>%
    spread_(., attr, "N", fill=0) 
    
    names(r$N)<-gsub("-| ","_",names(r$N))
    
    r$f <- r$N %>% mutate_at(., vars(setdiff(names(.), c(base,"sum.N"))), function(x) x/.$sum.N)
    
    return(r)
  }

  yksityiset<-filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi) & ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali")
  
  cont.data.kunta<-yksityiset %>% 
    group_by(kuntanimi, data) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                            Co2=mean(Co2, na.rm=TRUE),
                                            km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                            auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                            kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
    ungroup
  
  cont.data.pono3<-yksityiset %>% 
    group_by(pono.3, data) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                         Co2=mean(Co2, na.rm=TRUE),
                                         km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                         auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                         kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
    ungroup
  
  
  
  
  y<-trafi.freq(yksityiset)
  u<-trafi.freq(filter(yksityiset,uusi))
  w<-trafi.freq(yksityiset,"malli")
  
  
  u<-trafi.freq(mutate(yksityiset, uusi=plyr::mapvalues(uusi,c(TRUE,FALSE,NA), c("uusi","vanha","na"))), "uusi") 
  u<-u$N %>% mutate(pono.2=str_sub(pono.3,1,2))  %>%  group_by(.,pono.2,pono.3) %>% summarise_if(is.numeric,sum) 
  
kartta(select(cont.data.kunta, kW, data, alue=kuntanimi, kW) %>% filter(data=="4.02") %>% select(-data), aluejako="kuntanimi")
  

  
  x<-y$N %>% mutate(pono.2=str_sub(pono.3,1,2)) %>% select(.,-one_of(c("ryhma","ajoneuvonkaytto")))
  
  #xx<-w$N %>% mutate(pono.2=str_sub(pono.3,1,2)) %>% select(.,-one_of(c("ryhma","ajoneuvonkaytto")))
  #w<-y$N %>% mutate(pono.2=str_sub(pono.3,1,2)) %>% select(.,-one_of(c("ryhma","ajoneuvonkaytto"))) %>% group_by(pono.3,pono.2) %>% summarise_if(is.numeric,sum)
  
 
  p <- c()
  for (i in "4.10")
  p <-bind_rows(p, mutate(data=i, lme.binom.p2(filter(x, data==i & !is.na(pono.2)) %>% select(-data), "pono.2", "pono.3","sum.N")))
  
  
ggiraph(print(kartta(select(p,alue=pono.3,JAGUAR), aluejako="pono.3", map.colors="B")))



q <- lme.p2(filter(yksityiset, data=="4.10") %>% select(suurinNettoteho, pono.2, pono.3) %>% 
              filter(!is.na(suurinNettoteho) & !is.na(pono.2)  & !is.na(pono.3) ), "pono.2", "pono.3")

g <- filter(demografia$postinumero$suhteellinen, vuosi==2017 & aluejako=="pono.3") %>% 
  select(-euref_x, -euref_y, -vuosi, -pinta_ala, -matches("he_[0-9]"))

kartta(select(g, he_kika, alue), aluejako = "pono.3")

left_join(rename(g, pono.3=alue), q, by="pono.3") %>% ggplot(.,aes(y=suurinNettoteho,x=hr_ktu,size=he_vakiy,color=)) + geom_point()

left_join(rename(g,pono.3=alue),q,by="pono.3") %>% ggplot(.,aes(y=kayttovuodet,x=hr_ktu,size=he_vakiy,color=))+geom_point()


merkki.cor<-select_if(filter(p,data=="4.10"), is.numeric) %>% select_if(., function(x) var(x)>0) %>% cor
dist.cor<-sqrt(1-merkki.cor)

tsne(dist.cor, perplexity=30, k=2, max_iter=2000, whiten=TRUE) %>%
  data.frame(.) %>% transmute(x=X1,y=X2, merkki=rownames(dist.cor)) %>%
  ggplot(., aes(x=x,y=y,label=merkki)) + 
  geom_text(size=4)

