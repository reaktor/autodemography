# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data


source(here::here("initTrafi.R"))

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

henkiloauto <- tbl(trafi.db,"henkiloauto_uniqcombos") %>% 
  collect(n=Inf) %>% 
  mutate(merkki.orig=merkkiSelvakielinen, 
         malli.orig=kaupallinenNimi) %>%
  fix.auto %>%
  fix.merkki.malli %>% 
  fix.kori %>% 
  fix.co2 %>%
  select(combo,  
         N.combo, 
         kayttoonottoVuosi, 
         kayttoonottopvm,
         ensirekVuosi, 
         ajoneuvoluokka, 
         ajoneuvoryhma, 
         vari, 
         ryhma, 
         korityyppi, 
         kori.orig, 
         kori.est, 
         kori, 
         kayttovoima, 
         polttoaine, 
         omamassa, 
         iskutilavuus, 
         suurinNettoteho, 
         sahkohybridi, 
         merkki.orig, 
         malli.orig,
         mallimerkinta, 
         merkki, 
         malli, 
         l.malli,  
         merkki.l.malli, 
         Co2.orig, 
         Co2.modelled, 
         Co2)

# Laatu

#group_by(henkiloauto, kori.orig=toupper(kori.orig), kori.est) %>% summarise(N=sum(N.combo)) %>% spread(.,key=kori.orig,N, fill=0) %>% View
#filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig<20)) %>% mutate(d=abs(Co2.orig-Co2.modelled)) %>% ggplot(aes(x=Co2.orig,y=d))+geom_smooth()
#filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig < 20)) %>% mutate(d=abs(Co2.orig-Co2.modelled), D=abs(Co2.orig-Co2.modelled)/(Co2.modelled)) %>% .$D %>%
#quantile(.,c(.95,.99,1), na.rm=TRUE)
#mean(abs(henkiloauto$Co2.orig-henkiloauto$Co2.modelled), na.rm=TRUE)

henkiloauto.historia <-tbl(trafi.db,"henkiloauto_historia_diff") %>% 
  collect(n=Inf) %>% 
  select(-kayttoonottopvm)

henkiloauto.historia <- left_join(henkiloauto.historia, 
                                  select(henkiloauto, 
                                         combo, 
                                         kayttoonottopvm,
                                         kayttoonottoVuosi, 
                                         ensirekVuosi,
                                         vari, 
                                         ryhma, 
                                         kori,
                                         kori.orig,
                                         polttoaine, 
                                         omamassa,
                                         suurinNettoteho, 
                                         merkki,
                                         malli,
                                         merkki.l.malli, 
                                         Co2,
                                         Co2.orig,
                                         Co2.modelled
                                         ), 
                                  by="combo") %>% 
  mutate(ryhma=plyr::mapvalues(ryhma, 
                               c("Henkilö","Maasto"), 
                               c("Normaali","Normaali"))) 


henkiloauto.historia <- filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi))

s <- auto.stat(filter(henkiloauto.historia, ryhma=="Normaali" & kayttoonottoVuosi > 2006 & data=="5.00"))

historia <- mutate(henkiloauto.historia,
          uusi=ifelse(kayttovuodet <= 0.25, T, F),
          polttoaine=ifelse(polttoaine %in% c("Bensiini","Diesel","Sähkö"), polttoaine, "muupolttoaine"),
          merkki.kaikki=merkki,
          malli.kaikki=malli,
          merkki=ifelse(merkki %in% s$merkki$merkki[1:45], merkki, "muumerkki"), 
          malli=ifelse(merkki.l.malli %in% s$merkki.l.malli$merkki.l.malli[1:200], 
                       merkki.l.malli, ifelse(merkki != "muumerkki", paste0(merkki,"_muu"), "muumerkki")),
          merkki.kori.polttoaine=paste(merkki, tolower(kori), tolower(polttoaine), sep="."),
          pono.2=str_sub(pono.3,1,2))

#rm(henkiloauto.historia)
#rm(henkiloauto)

## Jatkuvia arvoja
  
  ## Jatkuvia arvoja ####
  cont.group <- function(x) 
    group_by(x, alue, date, ryhma, ajoneuvonkaytto) %>% 
    summarise(uusi=mean(uusi, na.rm=TRUE),
              Co2=mean(Co2, na.rm=TRUE),
              km=mean(km.per.kayttovuosi, na.rm=TRUE),
              auton.ika=mean(kayttovuodet, na.rm=TRUE),
              Co2.kg.vuosi=mean(km.per.kayttovuosi*(Co2/1000), na.rm=TRUE),
              kW=mean(suurinNettoteho, na.rm=TRUE), 
              N=n(),
              total.Co2.kg=N*Co2.kg.vuosi,
              total.km=N*km) %>% 
    ungroup
  
  cont.data <- bind_rows(
    "kuntanimi" = historia %>% 
      rename(alue=kuntanimi) %>%
      cont.group, 
    
    "pono.3" = historia %>% 
      rename(alue=pono.3) %>%
      cont.group,
    
    "pono.2" = historia %>% 
      rename(alue=pono.2) %>%
      cont.group,
    
    .id="aluejako" )
  
  cont.all <- function(x) 
    group_by(x, alue, date) %>% 
    summarise(uusi=mean(uusi, na.rm=TRUE),
              Co2=mean(Co2, na.rm=TRUE),
              km=mean(km.per.kayttovuosi, na.rm=TRUE),
              auton.ika=mean(kayttovuodet, na.rm=TRUE),
              Co2.kg.vuosi=mean(km.per.kayttovuosi*(Co2/1000), na.rm=TRUE),
              kW=mean(suurinNettoteho, na.rm=TRUE), 
              N=n(),
              total.Co2.kg=N*Co2.kg.vuosi,
              total.km=N*km) %>% 
    ungroup
  
  cont.data.all <- bind_rows(
    "kuntanimi" = historia %>% 
      rename(alue=kuntanimi) %>%
      cont.all, 
    
    "pono.3" = historia %>% 
      rename(alue=pono.3) %>%
      cont.all,
    
    "pono.2" = historia %>% 
      rename(alue=pono.2) %>%
      cont.all,
    
    .id="aluejako" )

  # Frekvenssejä
  trafi.freq <-
    function(h,
             attr = "merkki",
             base = c("date", "pono.3", "ryhma", "ajoneuvonkaytto")) {
      r <- list()
      r$N <- count.normalized(h, attr = attr, base = base) %>%
        select(., one_of(c(base, attr, "N", "sum.N"))) %>%
        spread_(., attr, "N", fill = 0)
      
      names(r$N) <- gsub("-| ", "_", names(r$N))
      
      r$f <-
        r$N %>% mutate_at(., vars(setdiff(names(.), c(base, "sum.N"))), function(x)
          x / .$sum.N)
      
      return(r)
    }
  
  add.pono.2 <- function(x) {
    x$N <- x$N %>% mutate(pono.2 = str_sub(pono.3, 1, 2))
    x$f <- x$f %>% mutate(pono.2 = str_sub(pono.3, 1, 2))
    return(x)
  }
 
  add.pono.1 <- function(x) {
    x$N <- x$N %>% mutate(pono.1 = str_sub(pono.2, 1, 1))
    x$f <- x$f %>% mutate(pono.1 = str_sub(pono.2, 1, 1))
    return(x)
  }
  
  historia<-mutate(historia, pono.2=str_sub(pono.3,1,2))
  
  Merkki.3 <- trafi.freq(historia, attr = "merkki", c("date", "pono.3", "ryhma", "ajoneuvonkaytto"))  %>% add.pono.2
  Malli.3 <- trafi.freq(historia, attr = "malli", c("date", "pono.3", "ryhma", "ajoneuvonkaytto")) %>% add.pono.2
  Kori.3 <- trafi.freq(historia, attr = "kori", c("date", "pono.3", "ryhma", "ajoneuvonkaytto")) %>% add.pono.2
  Polttoaine.3 <- trafi.freq(historia, attr = "polttoaine", c("date", "pono.3", "ryhma", "ajoneuvonkaytto")) %>% add.pono.2
  Vari.3 <- trafi.freq(historia, attr = "vari", c("date", "pono.3", "ryhma", "ajoneuvonkaytto")) %>% add.pono.2
  
  Merkki.2 <- trafi.freq(historia, attr = "merkki", c("date", "pono.2", "ryhma", "ajoneuvonkaytto"))  %>% add.pono.1
  Malli.2 <- trafi.freq(historia, attr = "malli", c("date", "pono.2", "ryhma", "ajoneuvonkaytto")) %>% add.pono.1
  Kori.2 <- trafi.freq(historia, attr = "kori", c("date", "pono.2", "ryhma", "ajoneuvonkaytto")) %>% add.pono.1
  Polttoaine.2 <- trafi.freq(historia, attr = "polttoaine", c("date", "pono.2", "ryhma", "ajoneuvonkaytto")) %>% add.pono.1
  Vari.2 <- trafi.freq(historia, attr = "vari", c("date", "pono.2", "ryhma", "ajoneuvonkaytto")) %>% add.pono.1
  

save(historia, file=full.path("historia.RData"))

save(cont.data.all, cont.data, Merkki.2, Malli.2, Kori.2, Polttoaine.2, Vari.2, 
     Merkki.3, Malli.3, Kori.3, Polttoaine.3, Vari.3, file = full.path("autodata.2.RData"))
  
#  Naivi hierarkkinen binomitn  
  
  p.binom.2 <- function(N.data, dates = "2017-09-30") {
    p <- c()
    for (d in dates) {
      print(d)
      p <- bind_rows(p,
                     mutate(date = d,
                            lme.binom.p2(
                              filter(
                                N.data,
                                date == d &
                                  !is.na(pono.2) 
                              ) %>%
                                select(-date,-ajoneuvonkaytto,-ryhma), "pono.2", "pono.3", "sum.N")
                     ))
    }
    return(p)
  }
  
  #  Naivi hierarkinen binomitn kakkosalue
  
  p.binom.1 <- function(N.data, dates = "2017-09-30") {
    p <- c()
    for (d in dates) {
      print(d)
      p <- bind_rows(p,
                     mutate(date = d,
                            lme.binom.p2(
                              filter(
                                N.data,
                                date == d &
                                  !is.na(pono.2) 
                              ) %>%
                                select(-date,-ajoneuvonkaytto,-ryhma), "pono.1", "pono.2", "sum.N")
                     ))
    }
    return(p)
  }

  
  p.binom.k <- function(N.data, dates = "2017-09-30") {
    p <- c()
    for (d in dates) {
      print(d)
      p <- bind_rows(p,
                     mutate(date = d,
                            lme.binom.p3(
                              filter(
                                N.data,
                                date == d &
                                  !is.na(pono.2) & !is.na(kuntanimi)
                              ) %>%
                                select(-date,-ajoneuvonkaytto,-ryhma), "pono.2", "pono.3", "kuntanimi", "sum.N")
                     ))
    }
    return(p)
  }
  
  
dates=unique(historia$date)

Merkki.pono.3 <- p.binom.2(Merkki.3$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Malli.pono.3 <- p.binom.2(Malli.3$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Kori.pono.3 <- p.binom.2(Kori.3$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Polttoaine.pono.3 <- p.binom.2(Polttoaine.3$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Vari.pono.3 <- p.binom.2(Vari.3$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)

Merkki.pono.2 <- p.binom.1(Merkki.2$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Malli.pono.2 <- p.binom.1(Malli.2$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Kori.pono.2 <- p.binom.1(Kori.2$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Polttoaine.pono.2 <- p.binom.1(Polttoaine.2$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)
Vari.pono.2 <- p.binom.1(Vari.2$N %>% filter(ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), dates)

paavo.mukaan<-c("alue", "pinta_ala", "he_vakiy", "he_kika", "ko_al_kork", "ko_yl_kork", "ko_ammat", "hr_ktu", "hr_mtu", "te_takk", "te_laps", "te_aik")

vaesto.3 <- filter(demografia$postinumero$suhteellinen, vuosi==2017 & aluejako=="pono.3") %>% 
  select(one_of(paavo.mukaan))

vaesto.2 <- filter(demografia$postinumero$suhteellinen, vuosi==2017 & aluejako=="pono.2") %>%
  select(one_of(paavo.mukaan))

demografia$postinumero$vars %>% filter(koodi %in% paavo.mukaan) %>% transmute(koodi, nimi, 2017+paavo.vuosi.offset)

data.2 <- select(Merkki.pono.2, -pono.1) %>% 
  left_join(., select(Malli.pono.2, -pono.1), by=c("date","pono.2")) %>%
  left_join(., select(mutate(Kori.pono.2, Kori_muu = Muu + `<NA>`), -pono.1, -`<NA>`) %>% mutate, by=c("date","pono.2")) %>%
  left_join(., select(Polttoaine.pono.2, -pono.1), by=c("date","pono.2")) %>%
  left_join(., select(rename(Vari.pono.2, varipuuttuu = `<NA>`), -pono.1), by=c("date","pono.2")) %>% 
  left_join(., rename(vaesto.2, pono.2=alue), by="pono.2") %>% select(-Muu) %>%
  rename(Merkki_muu=muumerkki.x, Merkkimalli_muu=muumerkki.y)

data.3 <- select(Merkki.pono.3, -pono.2) %>% 
  left_join(., select(Malli.pono.3, -pono.2), by=c("date","pono.3")) %>%
  left_join(., select(mutate(Kori.pono.3, Kori_muu = Muu + `<NA>`), -pono.2, -`<NA>`) %>% mutate, by=c("date","pono.3")) %>%
  left_join(., select(Polttoaine.pono.3, -pono.2), by=c("date","pono.3")) %>%
  left_join(., select(rename(Vari.pono.3, varipuuttuu = `<NA>`), -pono.2), by=c("date","pono.3")) %>% 
  left_join(., rename(vaesto.3, pono.3=alue), by="pono.3") %>% select(-Muu) %>%
  rename(Merkki_muu=muumerkki.x, Merkkimalli_muu=muumerkki.y)


save(Kori.pono.2, Malli.pono.2, Vari.pono.2, Polttoaine.pono.2, Merkki.pono.3, 
     Kori.pono.3, Malli.pono.3, Vari.pono.3, Polttoaine.pono.3, Merkki.pono.2, 
     data.2, data.3, vaesto.2, vaesto.3, file=full.path("autostats.RData"))


write.table(select(Kori.2$N, -pono.1) %>% rename(tietopuuttuu=`<NA>`), 
            file=full.path("Kori_pono2_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Malli.2$N, -pono.1, -ends_with("_muu"), -starts_with("muu")),
            file=full.path("Malli_pono2_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Merkki.2$N, -pono.1), 
            file=full.path("Merkki_pono2_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Polttoaine.2$N, -pono.1), 
            file=full.path("Polttoaine_pono2_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Vari.2$N, -pono.1) %>% rename(tietopuuttuu=`<NA>`), 
            file=full.path("Vari_pono2_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")


write.table(select(Kori.3$N, -pono.2) %>% rename(tietopuuttuu=`<NA>`), 
            file=full.path("Kori_pono3_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Malli.3$N, -pono.2, -ends_with("_muu"), -starts_with("muu")), 
            file=full.path("Malli_pono3_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Merkki.3$N, -pono.2), 
            file=full.path("Merkki_pono3_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Polttoaine.3$N, -pono.2), 
            file=full.path("Polttoaine_pono3_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(Vari.3$N, -pono.2) %>% rename(tietopuuttuu=`<NA>`), 
            file=full.path("Vari_pono3_N.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(select(data.3,-ends_with("_muu"), -starts_with("muu")) , file=full.path("data3_yksityinen_normaali.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")
write.table(select(data.2, -ends_with("_muu"), -starts_with("muu")), file=full.path("data2_yksityinen_normaali.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(cont.data, file=full.path("jatkuvat.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")
write.table(cont.data.all, file=full.path("jatkuvat_kaikki.tsv"), sep="\t", col.names=TRUE, row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")

write.table(historia, file=full.path("trafi_henkiloautot.tsv"), sep="\t", col.names=TRUE, 
            row.names=FALSE, fileEncoding="UTF-8", dec=".", quote=FALSE, na="")


###

lda.merkki <- LDA(select(filter(Merkki.3$N, date=="2017-09-30" & ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali"), 
                         -ajoneuvonkaytto, -ryhma, -date, -starts_with("pono"), -sum.N), k = 6, control=list(verbose=1), method="Gibbs")

lda.malli <- LDA(select(filter(Malli.3$N, date == "2017-09-30" & ajoneuvonkaytto == "Yksityinen" & ryhma=="Normaali"), 
                        -ajoneuvonkaytto, -ryhma, -date, -starts_with("pono"), -sum.N), k = 5, control=list(verbose=1), method="Gibbs")


Topics <- as.data.frame(posterior(lda.malli)$topics)
names(Topics) <- paste0("T",names(Topics))
Topics <- bind_cols(select(filter(Malli.3$N, date=="2017-09-30" & ryhma=="Normaali" & ajoneuvonkaytto=="Yksityinen"), 
                           ajoneuvonkaytto, ryhma, date, pono.2, pono.3, sum.N), Topics)
Terms<-as.data.frame(posterior(lda.malli)$terms)

cont<-filter(cont.data, aluejako=="pono.3" & ryhma=="Normaali" & ajoneuvonkaytto=="Yksityinen") %>% 
  select(-aluejako, -ryhma, -ajoneuvonkaytto) %>% rename(pono.3=alue)

data <- left_join(data.3, Topics, by=c("pono.3","date")) %>% 
  left_join(., cont, by=c("pono.3","date")) 
 # mutate(pinta_ala=pinta_ala/1e6,
 #         asukastiheys=he_vakiy/pinta_ala, 
 #         autoja.per.asukas=N/he_vakiy, 
 #         km.per.asukas=total.km/he_vakiy,
 #        Co2.per.asukas=total.Co2.kg/he_vakiy)


kartta(select(filter(Merkki.pono.2, date %in% c("2015-09-30","2017-09-30")) %>% group_by(pono.2) %>% arrange(date) %>% 
                mutate_if(is.numeric, function(x) lead(x)-x), FORD, alue=pono.2), aluejako="pono.2")

ggiraph(print(kartta(select(filter(data.3, date=="2017-09-30"), alue=pono.3, VOLVO_S90), aluejako="pono.3")))


X<-left_join(select(Topics, -pono.2, -ryhma, -date,-ajoneuvonkaytto, alue=pono.3), vaesto.3, by="alue") %>% 
  left_join(., Merkki.3, by="pono.3")
                                                                                                                      
                                                                                                                    
cor(select_if(data.3,is.numeric),use="na.or.complete") %>% corrplot(.,order="hclust", hclust.method="ward.D2")

ggiraph(print(kartta(Topics %>% select(alue=pono.3, T1), aluejako="pono.3")))

ggiraph(print(kartta(select(vaesto.3, alue, he_kika), aluejako="pono.3")))


### Kartta   
alue.jako="pono.2"    

kartta(select(cont.data, km, date, alue, ryhma, ajoneuvonkaytto, aluejako) %>% 
         filter(aluejako == alue.jako, date=="2016-12-31" & ryhma=="Normaali" & ajoneuvonkaytto=="Yksityinen") %>% 
         select(-date, -ryhma, -ajoneuvonkaytto), aluejako=alue.jako)


autodata <-left_join(select(Topics,-pono.2,-data), rename(vaesto, pono.3=alue), by="pono.3") %>% left_join(p, by="pono.3")
  
ggiraph(print(kartta(select(filter(cont.data.all, date=="2015-06-30" & alue.jako="pono.3"), alue=pono.3, Valkoinen), aluejako="pono.3")))

filter(demografia$postinumero$data, vuosi==2017 & pono.level==5) %>% select(pono, kuntano, nimi) %>% mutate(pono.3=str_sub(pono,1,3), kuntanimi=map.kunta(kuntano)) %>% group_by(pono.3) %>% summarise(kuntanimi=paste(sort(unique(kuntanimi)),collapse=";"), nimi=paste(sort(unique(nimi)),collapse=";")) %>% View

#q <- lme.p2(filter(yksityiset, data=="4.10") %>% select(suurinNettoteho, pono.2, pono.3) %>% 
#              filter(!is.na(suurinNettoteho) & !is.na(pono.2)  & !is.na(pono.3) ), "pono.2", "pono.3")

#merkki.cor<-select_if(filter(p,data=="4.10"), is.numeric) %>% select_if(., function(x) var(x)>0) %>% cor
#dist.cor<-sqrt(1-merkki.cor)

d<-sqrt(1-cor(select_if(select(filter(Malli.pono.3, date=="2017-09-30"), -pono.2), is.numeric), use="pairwise.complete.obs"))

tsne(d, perplexity=30, k=2, max_iter=3000, whiten=TRUE) %>%
  data.frame(.) %>% transmute(x=X1,y=X2, merkki=rownames(d)) %>%
  ggplot(., aes(x=x,y=y,label=merkki)) + 
  geom_text(size=3)

