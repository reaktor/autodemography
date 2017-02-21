#source("initTrafi.R")

library(tidyr)
library(Matrix)
library(tidytext)

trafi.db<- src_sqlite("trafi.db") 

autot <-tbl(trafi.db,"henkiloauto_viimeisin") %>% 
  select(data,jarnro, 
         iskutilavuus, sylintereidenLkm, suurinNettoteho, omamassa, Co2, kayttoonottoVuosi,
         merkkiSelvakielinen,kaupallinenNimi,mallimerkinta,
         kayttovoima,korityyppi) %>%
  collect(n=Inf)

autot.b <- mutate(autot,
                  kayttoonottoVuosi=ifelse(kayttoonottoVuosi<1900,NA,kayttoonottoVuosi),
                  omamassa=limitl(omamassa,700,NA),
                  omamassa=limith(omamassa,5000,NA),
                  iskutilavuus=limitl(iskutilavuus,100,NA),
                  iskutilavuus=limith(iskutilavuus,15000,NA),
                  suurinNettoteho=limith(suurinNettoteho,520,NA),
                  suurinNettoteho=round(limitl(suurinNettoteho,1,NA)),
                  sylintereidenLkm=limitl(sylintereidenLkm,1,NA),
                  sylintereidenLkm=limith(sylintereidenLkm,16,NA))


korjaus <- read.csv("mallitmerkkikorjaus.csv", quote="", fileEncoding = "UTF-8",sep="\t", stringsAsFactors = FALSE) %>%
  mutate_if(is.character,iconv,to="UTF-8") %>% 
  unique %>% 
  rename(mallimerkinta=malli.orig, 
         merkkiSelvakielinen=merkki.orig,
         kaupallinenNimi=k.malli.orig,
         malli=k.malli) 

korikorjaus<-read.csv("korikorjaus.csv", 
                      quote="", 
                      fileEncoding = "UTF-8",
                      sep="\t", stringsAsFactors = FALSE) %>%
  mutate_if(is.character,iconv,to="UTF-8")


autot.b<-left_join(autot.b,
                 select(korjaus,-n), 
                 by=c("merkkiSelvakielinen","kaupallinenNimi","mallimerkinta")) %>% 
  select(-merkkiSelvakielinen,-kaupallinenNimi) %>% 
  mutate(polttoaine=map.polttoaine(kayttovoima),
         kori=map.korityyppi(korityyppi))

## jos puuluokittimen todennäköisyys on alle 0.4 tai suora lookupin p on korkeampi kuin 
## puuluokittimen p, käytetään suoraa lookuppia (vaikka sen frekvenssi olisi matala)

kori <- left_join(select(autot,data,jarnro, mallimerkinta,malli,merkki,kori),
                  select(korikorjaus,malli,merkki,mallimerkinta,kori.nn,kori.tree,p.kori.nn,p.kori.tree),
                  by=c("merkki","malli","mallimerkinta")) %>%
  mutate(kori.est=kori,
         kori.est=ifelse(p.kori.tree > p.kori.nn, kori.tree, kori.nn),
         kori.est=ifelse(p.kori.tree > p.kori.nn & p.kori.tree < 0.4, NA, kori.tree),
         kori.est=ifelse(is.na(kori),kori.est,kori))

##

autot.b<-left_join(autot,select(kori,data,jarnro, kori.est), 
                   by=c("data","jarnro")) %>%
  mutate(kori.est=ifelse(is.na(kori), kori.est, kori))
         
         
autot.b<-mutate(autot.b,
         co2=ifelse(polttoaine == "Sähkö", 0, Co2),
         co2=ifelse(polttoaine == "Sähkö" & Co2 > 0, NA, co2),
         co2=ifelse((polttoaine != "Sähkö") & (Co2 == 0), NA, co2),
         co2=ifelse(co2>700,NA,co2))


autot.b <- mutate(autot.b,
            kayttoonottoVuosi=ifelse(kayttoonottoVuosi<1900,NA,kayttoonottoVuosi),
            omamassa=limitl(omamassa,700,NA),
            omamassa=limith(omamassa,5000,NA),
            iskutilavuus=limitl(iskutilavuus,100,NA),
            iskutilavuus=limith(iskutilavuus,15000,NA),
            suurinNettoteho=limith(suurinNettoteho,520,NA),
            suurinNettoteho=round(limitl(suurinNettoteho,1,NA)),
            sylintereidenLkm=limitl(sylintereidenLkm,1,NA),
            sylintereidenLkm=limith(sylintereidenLkm,16,NA))
          
co2<-group_by(autot.b, merkki, malli, mallimerkinta,iskutilavuus,suurinNettoteho) %>% 
  summarise(N=n(), 
            co2.Na=sum(is.na(co2)), 
            co2.miss=co2.Na/N,
            co2.median=median(co2,na.rm=TRUE), 
            co2.max=max(co2,na.rm=TRUE),
            co2.min=min(co2,na.rm=TRUE),
            abserr=sum(abs(co2-co2.median),na.rm=TRUE)/N*co2.Na)



###



autot.b<-mutate(co2.e=ifelse(polttoaine == "Sähkö", ifelse(Co2 > 0,NA,0),0),
                co2.e=ifelse(polttoaine != "Sähkö" & Co2==0, NA, Co2),
                co2.e=ifelse(Co2>700,NA,Co2))
                



autot.b<-left_join(autot.b, co2, by=c("merkki","malli","mallimerkinta"))

d<-select(autot.b,Co2,iskutilavuus,suurinNettoteho,omamassa,kayttoonottoVuosi,
         polttoaine,kori.est)

train<-sample_n(filter(d,!is.na(Co2)),100000)
validate<-sample_n(filter(d,!is.na(Co2)),50000)

param <- list("booster" = "gbtree", # tree
              "objective" = "reg:linear",   
              "eval_metric" = "rmse",    # evaluation metric 
              #"nthread" = 4,   # number of threads to be used 
              "max_depth" = 7,    # maximum depth of tree 
              "silent" = 0,
              "eta" = 0.3,    # step size shrinkage 
              #"gamma" = 0,    # minimum loss reduction 
              "subsample" = 1    # part of data instances to grow tree 
              #"colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              #"min_child_weight" = 1  # minimum sum of instance weight needed in a child 
)

malli.co2 <- xgboost(param=param, 
                  data = select(t,-Co2) %>% data.matrix, 
                  label = select(t,Co2) %>% data.matrix, 
                  nrounds=300, 
                  prediction=TRUE, 
                  verbose=TRUE)

autot.b$co2.tree<-predict(malli.co2, 
                         select(autot.b, 
                                iskutilavuus,
                                suurinNettoteho,
                                omamassa,
                                kayttoonottoVuosi, 
                                polttoaine,
                                kori.est) %>% data.matrix)


autot.b<-mutate(autot.b,err=co2.est-Co2, abserr=abs(err))
quantile(autot.b$abserr,na.rm=TRUE,p=c(.5,.95))

autot.b<-mutate(co2.est=ifelse(polttoaine == "Sähkö", ifelse(Co2>0,NA,0),0),
                co2.est=ifelse(polttoaine != "Sähkö" & Co2==0,NA, Co2),
                co2.est=ifelse(polttoaine != "Sähkö" & is.na(Co2) & co2)



## tallenna 

malli<-mutate(co2,key=mallimerkinta,
              key=mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
                         ifelse(is.na(merkki),"",merkki), key, USE.NAMES=FALSE),
              key=mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
                         ifelse(is.na(malli),"",malli), key, USE.NAMES=FALSE),
              key=toupper(key))

x=cast_sparse(s.tab(malli$ix, 
                    paste(malli$merkki,malli$malli,malli$key),
                    N=malli$N.tot, 
                    sep=" |-|,|\)|\(|") %>%
                rename(ix=id) %>%
                left_join(., select(malli,ix,kori,p,merkki),by="ix") %>% 
                mutate(word=as.character(word),
                       word=ifelse(is.na(word),"NA",word)),ix,word) 

# compute feature importance matrix
importance_matrix = xgb.importance(colnames(x), model=malli.co2)
