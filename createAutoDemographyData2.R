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

trafi.db<- src_sqlite(paste(working.directory, "/trafi.db",sep=""), create=FALSE)

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

henkiloauto.historia <-tbl(trafi.db, "henkiloauto_historia_diff") %>% 
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


henkiloauto.historia <- filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi))

s<-auto.stat(filter(henkiloauto.historia, ryhma=="Normaali" & kayttoonottoVuosi > 2006 & data=="4.10"))

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
          merkki=ifelse(merkki %in% s$merkki$merkki[1:45], merkki, "muu"), 
          malli=ifelse(merkki.l.malli %in% s$merkki.l.malli$merkki.l.malli[1:45], 
                       merkki.l.malli, ifelse(merkki != "muumerkki", merkki, "muu")),
          merkki.kori.polttoaine=paste0(merkki, tolower(kori), tolower(polttoaine), sep="."),
          pono.2=str_sub(pono.3,1,2), 
          rekisteriero.v=kayttovuodet-rekisterivuodet)

  trafi.freq <- function(h, attr="merkki", base=c("data", "pono.3", "ryhma", "ajoneuvonkaytto")) {
    r<-list()
    r$N<-count.normalized(h, attr=attr, base=base) %>% 
    select(., one_of(c(base, attr, "N", "sum.N"))) %>%
    spread_(., attr, "N", fill=0) 
    
    names(r$N)<-gsub("-| ","_",names(r$N))
    
    r$f <- r$N %>% mutate_at(., vars(setdiff(names(.), c(base,"sum.N"))), function(x) x/.$sum.N)
    
    return(r)
  }


  cont.data.kunta <- henkiloauto.historia %>% 
    group_by(kuntanimi, data, ryhma, ajoneuvonkaytto) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                            Co2=mean(Co2, na.rm=TRUE),
                                            km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                            auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                            kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
    ungroup
  
  cont.data.pono3 <- henkiloauto.historia %>% 
    group_by(pono.3, data, ryhma, ajoneuvonkaytto) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                         Co2=mean(Co2, na.rm=TRUE),
                                         km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                         auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                         kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
    ungroup
  
  cont.data.pono2 <- henkiloauto.historia %>% 
    group_by(pono.2, data, ryhma, ajoneuvonkaytto) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                         Co2=mean(Co2, na.rm=TRUE),
                                         km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                         auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                         kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
    ungroup
  
  
kartta(select(cont.data.pono3, km, data, alue=pono.3, ryhma, ajoneuvonkaytto) %>% 
           filter(data=="4.10" & ryhma=="Normaali" & ajoneuvonkaytto=="Yksityinen") %>% 
           select(-data,-ryhma,-ajoneuvonkaytto), aluejako="pono.3")
  
M <- trafi.freq(filter(henkiloauto.historia, ryhma == "Normaali" & ajoneuvonkaytto == "Yksityinen"), attr="merkki")
K <- trafi.freq(filter(henkiloauto.historia, ryhma == "Normaali" & ajoneuvonkaytto == "Yksityinen"), attr="kori")
P <- trafi.freq(filter(henkiloauto.historia, ryhma == "Normaali" & ajoneuvonkaytto == "Yksityinen"), attr="polttoaine")


M$N <- M$N %>% mutate(pono.2=str_sub(pono.3,1,2))
K$N <- K$N %>% mutate(pono.2=str_sub(pono.3,1,2))
P$N <- P$N %>% mutate(pono.2=str_sub(pono.3,1,2))


p <- c()
for (i in "4.10") 
  p <- bind_rows(p, mutate(data=i, lme.binom.p2(filter(P$N, data==i & !is.na(pono.2)) %>% select(-data, -ajoneuvonkaytto, -ryhma), "pono.2", "pono.3","sum.N")))
  
lda <- LDA(select(filter(M$N, data=="4.10"), -ajoneuvonkaytto, -ryhma, -data, -pono.2, -pono.3, -sum.N), k = 6, control=list(verbose=1),method="Gibbs")
Topics <- as.data.frame(posterior(lda)$topics)
names(Topics) <- paste0("T",names(Topics))
Topics <- bind_cols(select(filter(merkki$N, data=="4.10"), ajoneuvonkaytto, ryhma, data, pono.2, pono.3, sum.N), Topics)
Topics$max.topic<-topics(lda)

ggiraph(print(kartta(select(vaesto, alue, he_kika), aluejako="pono.3")))

vaesto <- filter(demografia$postinumero$suhteellinen, vuosi==2017 & aluejako=="pono.3") %>% 
  select(one_of(c("alue","naiset.osuus","he_kika","ko_al_kork", "ko_yl_kork", "ko_ammat","hr_ktu","hr_mtu","te_takk","te_as_valj","te_laps","te_aik","te_vuok_as","ra_ke","tp_alku_a","tp_jalo_bf","tp_palv_gu","pt_tyott","pt_0_14","pt_opisk")
))
  
autodata <-left_join(select(Topics,-pono.2,-data), rename(vaesto, pono.3=alue), by="pono.3") %>% left_join(p, by="pono.3")
  

ggiraph(print(kartta(select(p, alue=pono.3, Bensiini), aluejako="pono.3")))

#q <- lme.p2(filter(yksityiset, data=="4.10") %>% select(suurinNettoteho, pono.2, pono.3) %>% 
#              filter(!is.na(suurinNettoteho) & !is.na(pono.2)  & !is.na(pono.3) ), "pono.2", "pono.3")

merkki.cor<-select_if(filter(p,data=="4.10"), is.numeric) %>% select_if(., function(x) var(x)>0) %>% cor
dist.cor<-sqrt(1-merkki.cor)

tsne(dist.cor, perplexity=16, k=2, max_iter=3000, whiten=TRUE) %>%
  data.frame(.) %>% transmute(x=X1,y=X2, merkki=rownames(dist.cor)) %>%
  ggplot(., aes(x=x,y=y,label=merkki)) + 
  geom_text(size=4)

