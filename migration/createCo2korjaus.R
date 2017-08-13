#source("initTrafi.R")

library(tidyr)
library(Matrix)
library(tidytext)
library(xgboost)

trafi.db<- src_sqlite("trafi.db") 

autot <- tbl(trafi.db, "henkiloauto_uniqcombos") %>%
  select(
    #data,
    #jarnro,
    combo,
    iskutilavuus,
    sylintereidenLkm,
    suurinNettoteho,
    omamassa,
    Co2,
    kayttoonottoVuosi,
    merkkiSelvakielinen,
    kaupallinenNimi,
    mallimerkinta,
    kayttovoima,
    korityyppi
  ) %>%
  collect(n = Inf) %>%
  mutate(kori = map.korityyppi(korityyppi)) %>%
  fix.merkki.malli %>%
  fix.kori

autot <- mutate(
  autot,
  kayttoonottoVuosi = ifelse(kayttoonottoVuosi < 1900, NA, kayttoonottoVuosi),
  omamassa = limitl(omamassa, 700, NA),
  omamassa = limith(omamassa, 5000, NA),
  iskutilavuus = limitl(iskutilavuus, 100, NA),
  iskutilavuus = limith(iskutilavuus, 15000, NA),
  suurinNettoteho = limith(suurinNettoteho, 520, NA),
  suurinNettoteho = round(limitl(suurinNettoteho, 1, NA)),
  sylintereidenLkm = limitl(sylintereidenLkm, 1, NA),
  sylintereidenLkm = limith(sylintereidenLkm, 16, NA),
  polttoaine = map.polttoaine(kayttovoima)
)

# Jos Co2>0, polttoaine ei voi olla sähkö, jos polttoaine = sähkö ja co2 puutuu = 0
autot.b<-mutate(autot,
         polttoaine=ifelse(polttoaine=="Sähkö" & !is.na(Co2) & Co2 > 0, NA, polttoaine),        
         co2=ifelse(polttoaine == "Sähkö", 0, Co2),
         co2=ifelse((polttoaine != "Sähkö") & (Co2 == 0), NA, co2),
         co2=ifelse(co2>700,NA,co2))

# Liitetään suoraan ne automallit+tilavuus joiden co2 2 yksikän sisällä, jos co2 puuttuu

co2 <- group_by(autot.b, merkki, mallimerkinta, iskutilavuus) %>%
  summarise(
    N = n(),
    co2.Na = sum(is.na(co2)),
    co2.nn = median(co2, na.rm = TRUE),
    co2.max = max(co2, na.rm = TRUE),
    co2.min = min(co2, na.rm = TRUE)
  )

autot.b <- left_join(
  autot.b,
  filter(co2, co2.max - co2.min <= 2) %>%
    select(co2.nn, merkki, mallimerkinta, iskutilavuus),
  by = c("merkki", "mallimerkinta", "iskutilavuus")
) %>%
  mutate(co2 = ifelse(is.na(co2), co2.nn, co2))

d <-
  select(
    autot.b,
    co2,
    iskutilavuus,
    suurinNettoteho,
    omamassa,
    kayttoonottoVuosi,
    polttoaine,
    kori.est,
    merkki
  )

train<-sample_n(filter(d,!is.na(co2) & polttoaine!="Sähkö"),100000)

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
                  data = select(train,-co2) %>% data.matrix, 
                  label = select(train,co2) %>% data.matrix, 
                  nrounds=300, 
                  prediction=TRUE, 
                  verbose=TRUE)

## Malli ja taulukko talteen
save(file="malli.co2.RData", malli.co2)

write.table(co2, file="co2.stat.csv",quote=FALSE,
            sep="\t",row.names=FALSE,fileEncoding="UTF-8",append=FALSE)

