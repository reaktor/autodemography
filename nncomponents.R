library(dplyr)
library(ggplot2)
library(rstan)
library(stringr)

load("autot.RData")

autot <- mutate(autot,l.malli = k.malli,
                l.malli = ifelse(merkki=="BMW", substring(l.malli, 1, 1), l.malli),
                l.malli = ifelse(merkki=="MERCEDES-BENZ", str_match(l.malli, "^[0-9]{1,3}|^[A-Za-z]+"),l.malli),
                merkki.malli = paste(merkki, l.malli),
                pono.3 = str_pad(pono.3, 3, side="left", pad="0")
                )
add.sums <- function (d) d %>%
  left_join(d %>% group_by(merkki.malli) %>% summarise(N.mm = sum(N))) %>% 
  left_join(d %>% group_by(pono.3) %>% summarise(N.pono3 = sum(N)))
  
autot.pono3 <- 
  autot %>% filter(kaytto=="Yksityinen") %>% group_by(merkki.malli, pono.3) %>% summarise(N=n()) %>% 
  arrange(-N) %>% filter(!is.na(pono.3) & !is.na(merkki.malli)) %>%
  add.sums %>% filter(N.pono3>5 & N.mm>10) %>% select(-N.mm, -N.pono3) %>% add.sums %>% ungroup %>%
  mutate(pono.3 = as.factor(pono.3), merkki.malli=as.factor(merkki.malli))

mc <- stan_model("nonnegative.stan")
stan.data <- autot.pono3 %>% { list(L = nrow(.), K = 4, 
                                    N_area = nlevels(.$pono.3), N_car = nlevels(.$merkki.malli), 
                                    i_car = as.numeric(.$merkki.malli), 
                                    i_area = as.numeric(.$pono.3), 
                                    n = .$N) }

# fit.opt <- optimizing(mc, data=stan.data)
system.time(fit.vb <- vb(mc, data=stan.data, algorithm="meanfield"))
hist(extract(fit.vb, "theta")$theta, n=100)
extract(fit.vb, "logmu_larea")[[1]] %>% apply(2, median)*1000
extract(fit.vb, "sigma_lcar")[[1]] %>% apply(2, median)
extract(fit.vb, "sigma_larea")[[1]] %>% apply(2, median)
# Normalize the poisson intensities first to get component memberships,
# then take median over samples as a posterior summary.
cars_multinomial <- extract(fit.vb, "lambda_car")$lambda_car %>% 
  apply(c(1, 2), function (i) i/sum(i)) %>% 
  apply(c(1, 3), median) %>% 
  t
cars_scores <- data.frame(merkki.malli=levels(autot.pono3$merkki.malli), 
                          score=cars_multinomial, 
                          N=rowSums(cars_poisson)) %>%
  tbl_df() %>% arrange(-N)
#saveRDS(fit.vb, "fit3.rds")
cars_scores %>% arrange(-N) %>% print(n=25)
cars_scores %>% arrange(-score.1) %>% print(n=25)
cars_scores %>% arrange(-score.2) %>% print(n=25)
cars_scores %>% arrange(-score.3) %>% print(n=25)
cars_scores %>% arrange(-score.4) %>% print(n=25)
cars_scores %>% arrange(-score.5) %>% print(n=25)
cars_scores %>% arrange(-score.6) %>% print(n=25)
