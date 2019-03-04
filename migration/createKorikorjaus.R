source("initTrafi.R")

library(Matrix)
library(xgboost)

autot <- tbl(trafi.db, "autotiedot") %>%
  transmute(
    merkkiSelvakielinen,
    kaupallinenNimi,
    mallimerkinta,
    korityyppi,
    combination.id,
    N.combinations
  ) %>%
  collect(n = Inf)

###

autot <- mutate(autot, kori = map.korityyppi(korityyppi))

# Lasketaan korityypit per merkki - malli 
malli <-
  group_by(autot, 
           merkkiSelvakielinen, 
           kaupallinenNimi, 
           mallimerkinta, 
           kori) %>%
  summarise(nn = sum(N.combinations)) %>%
  dcast(
    merkkiSelvakielinen + kaupallinenNimi + mallimerkinta ~ kori,
    fun.aggregate = sum,
    value.var = "nn"
  ) %>% 
  rename(N_missing = "NA")

malli$N_exists <- rowSums(select(malli, Avo:Viistoperä))
malli$N <- malli$N_exists + malli$N_missing

# korityyppien osuus

malli <- mutate_at(malli,
  .vars = names(select(malli, Avo:Viistoperä)), 
  .funs = funs("p" = . / N_exists)) %>%
  mutate(row_index = row_number())
  
# Kerätään suhteelliset osuudet ja lasktetaan naivi voittaja
q <- select(malli, ends_with("_p"), row_index) %>%
  gather(., kori, p, `Avo_p`:`Viistoperä_p`, -row_index) %>%
  group_by(row_index) %>% 
  arrange(-p) %>% 
  dplyr::slice(1) %>%
  mutate(
    kori = gsub("_p$", "", kori),
    kori = ifelse(is.nan(p), NA, kori),
    p = coalesce(p, 0)
  )

malli <- left_join(malli, q, by = "row_index")  %>%
  mutate_at(vars(ends_with("_p")), function(x) coalesce(x,0))

# Tehdään mallimerkinnästä key - string 
malli <- mutate(
  malli,
  key = toupper(paste(merkkiSelvakielinen, 
                      kaupallinenNimi, 
                      mallimerkinta))
)

# merkkiSelvakielinen & Malli-info harvaksi numeeriseksi matriisiksi

x <- tidytext::unnest_tokens(select(malli, row_index, key), 
                        word, 
                        key, 
                        token = "regex", 
                        drop=TRUE, 
                        to_lower=FALSE, 
                        pattern = " |-|,|\\)|\\(|/") %>%
  mutate(word = as.character(word)) %>%
  filter(word != "") %>% 
  count(row_index, word) %>% 
  tidytext::cast_sparse(., row_index, word, n)

### opetus

train.index <- !is.na(malli$kori) & malli$kori > 0.75 
kori.label <- as.numeric(as.factor(malli$kori[train.index])) - 1
N.class <- max(kori.label) + 1

# poistetaan kaikkein harvinaisimmat sanat & tehdään malli
x.relevant.colidx <- Matrix::colSums(x) >= 30 

param <- list(
  "booster" = "gbtree", # tree
  "objective" = "multi:softprob", # multiclass classification
  "num_class" = N.class, # number of classes
  "eval_metric" = "merror", # evaluation metric
  "nthread" = 4,   # number of threads to be used
  "max_depth" = 7, # maximum depth of tree
  "silent" = 0,
  "eta" = .3, # step size shrinkage
  "subsample" = .9,   # part of data instances to grow tree
  "lambda" = 1,
  "alpha" = 1,
  #"colsample_bytree" = .5,  # subsample ratio of columns when constructing each tree
  "min_child_weight" = 1  # minimum sum of instance weight needed in a child
)

xgb.cv(param = param,
       data = x[train.index, x.relevant.colidx],
       label = kori.label,
       sample_weight = malli$N[train.index],
       nrounds = 150,
       prediction = TRUE,
       early_stopping_rounds = 3,
       verbose = TRUE, nfold = 5, stratified=TRUE)

malli.kori <- xgboost(
  param = param,
  data = x[train.index, x.relevant.colidx],
  label = kori.label,
  sample_weight = malli$N[train.index],
  nrounds = 55,
  prediction = TRUE,
  verbose = TRUE
)

# estimoi malli
p.est <- predict(malli.kori, x[, x.relevant.colidx]) %>%
  matrix(., nrow = N.class, ncol = length(.) / N.class) %>%
  t(.)

# tallennetaan naiivi suhteellinen frekvenssi ja mallin output
malli$kori.tree <- levels(as.factor(malli$kori))[max.col(p.est)]
malli$p.kori.tree <- apply(p.est, 1, max)

kori.korjaus <- select(malli, -ends_with("_p"), -row_index) %>%
  rename(
         p.kori.nn = p,
         kori.nn = kori)

## tallenna

write.table(
  kori.korjaus,
  file = here::here("Data", "korikorjaus.csv"),
  quote = FALSE,
  sep = "\t",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  append = FALSE
)
