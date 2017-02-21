
fix.merkit.mallit <- function(henkiloautot, korjaustiedosto = "mallitmerkkikorjaus.csv")
{
korjaus <- read.csv(korjaustiedosto, quote="", fileEncoding = "UTF-8",sep="\t", stringsAsFactors = FALSE) %>%
  mutate_if(is.character,iconv,to="UTF-8") %>% 
  unique %>% 
  rename(mallimerkinta=malli.orig, 
         merkkiSelvakielinen=merkki.orig,
         kaupallinenNimi=k.malli.orig,
         malli=k.malli)

# Lisätään myös "lyhyt" automalli BMW:lle ja Mersulle

left_join(henkiloautot,
                 select(korjaus,-n), 
                 by=c("merkkiSelvakielinen","kaupallinenNimi","mallimerkinta")) %>% 
  mutate(l.malli=malli,
         l.malli=ifelse(merkki=="BMW",substring(l.malli,1,1),l.malli),
         l.malli=ifelse(merkki=="MERCEDES-BENZ", str_match(l.malli,"^[0-9]{1,3}|^[A-Za-z]+"),l.malli),
         merkki.l.malli=paste(ifelse(is.na(merkki),"",merkki),
                              ifelse(is.na(l.malli),"",l.malli)),
         merkki.l.malli=ifelse(merkki.l.malli=="",NA,merkki.l.malli)) %>%
  select(-merkkiSelvakielinen) %>% 
  return
}

est.kori <- function(henkiloautot, korjaustiedosto="korikorjaus.csv")
{  
### Täydennetään puuttuvat koritiedot 
### fixMallitMerkit on ajettava ensin!!
  
  korjaus<-read.csv("korikorjaus.csv", 
                        quote="", 
                        fileEncoding = "UTF-8",
                        sep="\t", stringsAsFactors = FALSE) %>%
  mutate_if(is.character,iconv,to="UTF-8")

## jos puuluokittimen todennäköisyys on alle 0.4 tai suora lookupin p on korkeampi kuin 
## puuluokittimen p, käytetään suoraa lookuppia (vaikka sen frekvenssi olisi matala)

hekiloautot<-mutate(henkiloautot, kori=map.korityyppi(korityyppi))
  
kori <- left_join(select(henkiloautot, data, jarnro, mallimerkinta, malli, merkki, kori),
                  select(korjaus,malli,merkki,mallimerkinta,kori.nn,kori.tree,p.kori.nn,p.kori.tree),
                  by=c("merkki","malli","mallimerkinta")) %>%
  mutate(kori.est=kori,
         kori.est=ifelse(p.kori.tree > p.kori.nn, kori.tree, kori.nn),
         kori.est=ifelse(p.kori.tree > p.kori.nn & p.kori.tree < 0.4, NA, kori.tree),
         kori.est=ifelse(is.na(kori),kori.est,kori))

left_join(henkiloautot,select(kori, data, jarnro, kori.est), 
                   by=c("data","jarnro")) %>%
  mutate(kori.est=ifelse(is.na(kori),kori.est,kori)) %>% return
}
