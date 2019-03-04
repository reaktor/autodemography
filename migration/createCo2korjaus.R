#source("initTrafi.R")

library(tidyr)
library(Matrix)
library(tidytext)
library(xgboost)

autot <- tbl(trafi.db, "autotiedot") %>%
  select(
    combination.id,
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
  collect(n = Inf)


autot <- left_join(autot, select(fix.kori(autot), combination.id, kori), by="combination.id")

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

# Jos Co2 > 0, polttoaine ei voi olla sähkö, jos polttoaine = sähkö ja co2 puutuu = 0
autot <- mutate(
  autot,
  polttoaine = ifelse(polttoaine == "Sähkö" & !is.na(Co2) & Co2 > 0, NA, polttoaine),
  co2 = ifelse(polttoaine == "Sähkö", 0, Co2),
  co2 = ifelse((polttoaine != "Sähkö") & (Co2 == 0), NA, co2),
  co2 = ifelse(co2 > 700, NA, co2)
)

# Liitetään suoraan ne automallit+tilavuus joiden co2 2 yksikän sisällä, jos co2 puuttuu

co2 <- group_by(autot, merkkiSelvakielinen, mallimerkinta, iskutilavuus) %>%
  summarise(
    N = n(),
    co2.Na = sum(is.na(co2)),
    co2.nn = median(co2, na.rm = TRUE),
    co2.max = max(co2, na.rm = TRUE),
    co2.min = min(co2, na.rm = TRUE)
  ) %>% mutate(
  co2.max = ifelse(!is.finite(co2.max), NA, co2.max),
  co2.min = ifelse(!is.finite(co2.min), NA, co2.min))

autot <- left_join(
  autot,
  filter(co2, co2.max - co2.min <= 5) %>%
    select(co2.nn, merkkiSelvakielinen, mallimerkinta, iskutilavuus),
  by = c("merkkiSelvakielinen", "mallimerkinta", "iskutilavuus")
) %>%
  mutate(co2 = ifelse(is.na(co2), co2.nn, co2))

data <-
  transmute(
    autot,
    co2,
    iskutilavuus,
    suurinNettoteho,
    omamassa,
    kayttoonottoVuosi,
    polttoaine,
    kori,
    merkki=toupper(merkkiSelvakielinen)
  )

train <- sample_n(filter(data, !is.na(co2) & polttoaine != "Sähkö"), 200000)

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

#xgb.cv(param = param,
#       data = select(train, -co2) %>% data.matrix,
#       label = select(train, co2) %>% data.matrix,
#       nrounds = 300,
#       prediction = TRUE,
#       early_stopping_rounds = 3,
#      verbose = TRUE, nfold = 5, stratified=TRUE)


malli.co2 <- xgboost(
  param = param,
  data = select(train, -co2) %>% data.matrix,
  label = select(train, co2) %>% data.matrix,
  nrounds = 300,
  prediction = TRUE,
  verbose = TRUE
)

autot$Co2.xgboost <- predict(malli.co2, select(data, -co2) %>% data.matrix) 

e <-transmute(autot, 
          combination.id, 
          Co2.orig = Co2,
          Co2 = ifelse(is.na(co2), Co2.xgboost, co2), 
          Co2.est = round(ifelse(is.na(co2.nn), Co2.xgboost, co2.nn) ))


## Malli ja taulukko talteen
saveRDS(file = here::here("Data","malli.co2.rds"), malli.co2)

write.table(co2, file = here::here("Data", "co2.stat.csv"), quote=FALSE,
            sep="\t",row.names=FALSE, fileEncoding="UTF-8",append=FALSE)


write.table(e, file = here::here("Data", "co2.korjaus.csv"), quote=FALSE,
            sep="\t",row.names=FALSE, fileEncoding="UTF-8",append=FALSE)

