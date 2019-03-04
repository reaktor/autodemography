library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyr)
library(lubridate)

directory <- here::here()

full.path <- function(file.name,
                      data.path = "Data",
                      wd = directory)
  paste(wd, data.path, file.name, sep = "/")

setwd(directory)

trafi.db <- src_sqlite(full.path("trafi.db"), create = FALSE)

data.map.trafi <- function() {
  koodisto <-
    read.csv(
      file = full.path("16968-Koodisto_2015.csv"),
      sep = ";",
      fileEncoding = "MAC",
      header = TRUE
    )
  
  # otetaan vain suomen kieli ja kivammat nimet
  # Ohjaamotyypin kohdalla PITKASELITE on suomeksi väärin, otetaan siinä lyhytselite
  # valitaan kieleksi suomie
  # Ajoneuvoryhmälle on muakavampi ottaa myös leyhytselite
  
  # TRAFI: koodistoa... / lyhennetään nimiä >
  
  map.koodit = c(
    #"Kuntien numerot ja nimet" = "kunta",
    "Ajoneuvomerkit" = "merkki",
    "Ajoneuvoluokkaa tarkempi luokittelu ajoneuvoille" = "ryhma",
    "Direktiivien mukainen kooditus, jossa huomioitu myös kansalliset ajoneuvoluokat." = "luokka",
    "Korityyppi" = "kori",
    "Ohjaamotyyppi" = "ohjaamo",
    "Polttoaine" = "polttoaine",
    "Ajoneuvon väri" =  "vari",
    "Voimanvälitys ja tehostamistapa" = "valitys",
    "Ajoneuvon käyttö" = "kaytto",
    "Vaihteistotyyppi" = "vaihteisto"
  )
  
  koodit <-
    transmute(
      koodisto,
      kieli = as.character(KIELI),
      kuvaus = as.character(KOODISTONKUVAUS),
      koodi = as.character(KOODINTUNNUS),
      nimi = as.character(PITKASELITE),
      nimi.2 = as.character(LYHYTSELITE)
    ) %>%
    filter(kieli == "fi") %>%
    select(-kieli) %>%
    mutate(
      kuvaus = plyr::revalue(kuvaus, map.koodit),
      nimi = ifelse(kuvaus == "ohjaamo", nimi.2, nimi),
      nimi = ifelse(kuvaus == "luokka", nimi.2, nimi)
    ) %>%
    select(-nimi.2)
  
  koodit[dim(koodit)[1] + 1, ] <- c("kaytto", "nul", NA)
  koodit[dim(koodit)[1] + 1, ] <- c("kaytto", "", NA)
  koodit[dim(koodit)[1] + 1, ] <- c("kori", "", NA)
  koodit[dim(koodit)[1] + 1, ] <- c("vari", "", NA)
  koodit[dim(koodit)[1] + 1, ] <- c("ryhma", "26", "Maastohenkilöauto")
  koodit[dim(koodit)[1] + 1, ] <- c("ryhma", "508", "Henkilöauto")
  koodit[dim(koodit)[1] + 1, ] <-
    c("ryhma", "61", "Selväkielisenä syötettävä nimitys")
  koodit[dim(koodit)[1] + 1, ] <- c("ryhma", NA, NA)
  
  ## Kooditaulut
  
  map.trafi = list()
  for (i in unique(koodit$kuvaus)) {
    map.trafi[[i]] <- filter(koodit, kuvaus == i) %>%
      transmute(., koodi = as.character(koodi), nimi = as.character(nimi))
  }
  
  # vaihdetaan M1G-koodi
  map.trafi[["luokka"]][["nimi"]][map.trafi[["luokka"]][["koodi"]] == "M1G"] <-
    "Henkilöauto (maasto)"
  
  ## lyhennellään värien koodauksia
  
  map.trafi[["vari"]]["nimi"] <-
    plyr::mapvalues(as.vector(map.trafi[["vari"]][["nimi"]]), c("Ruskea (beige)"), c("Ruskea"), warn_missing = FALSE)
  
  ## lyhennellään käyttötavan koodausta # korjataan koodia
  map.trafi[["kaytto"]]["nimi"] <-
    gsub(" .*$", "", map.trafi[["kaytto"]][["nimi"]])
  # character padding oli tarpeen jossain vaiheessa, mutta 2017 muuttui takaisin
  #map.trafi[["valitys"]]["koodi"]<-str_pad(map.trafi[["valitys"]][["koodi"]], 1, side="left", pad="0")
  map.trafi[["valitys"]]["koodi"] <-
    as.numeric(map.trafi[["valitys"]][["koodi"]])
  return(map.trafi)
}

# Trafi muuttujien uudeleenkoodausfunktio
map.trafi <- function(var.nimi, orig.val, data = data.map.trafi()) {
  plyr::mapvalues(orig.val, data[[var.nimi]][["koodi"]], data[[var.nimi]][["nimi"]], warn_missing = FALSE)
}

## Polttoaineiden uudelleenkoodausta (yksinkertaisempi koodaus)

map.polttoaine <- function(v)
  plyr::mapvalues(
    v,
    c(
      "Bensiini",
      "Dieselöljy",
      "Bensiini/Etanoli",
      "Bensiini/CNG",
      "Sähkö",
      "CNG",
      "Bensiini/Sähkö",
      "",
      "Bensiini + moottoripetroli",
      "Muu",
      "Bensiini/Puu",
      "Diesel/Biodiesel",
      "Bensiini/LPG",
      "Diesel/CNG",
      "Etanoli",
      "Etanoli (E85)",
      "Diesel/Sähkö",
      "Biometaani",
      "Diesel/Biodiesel/CNG",
      "HL-ryhmän maakaasu",
      "LPG",
      "Puu",
      "Vety"
    ),
    c(
      "Bensiini",
      "Diesel",
      "Bensiini/Etanoli",
      "Bensiini/CNG",
      "Sähkö",
      "CNG",
      "Bensiini/Sähkö",
      NA,
      "Bensiini",
      "Muu",
      "Muu",
      "Diesel",
      "Muu",
      "Muu",
      "Bensiini/Etanoli",
      "Bensiini/Etanoli",
      "Muu",
      "Muu",
      "Muu",
      "Muu",
      "Muu",
      "Muu",
      "Muu"
    ), warn_missing = FALSE
  )

map.korityyppi <- function(korityyppi) {
  kori = ifelse(
    korityyppi %in% c(
      NA,
      "Avoauto (AE)",
      "Coupé (AD)",
      "Farmari (AC)",
      "Matkailuauto (SA)",
      "Monikäyttöajoneuvo (AF)",
      "Sedan (AA)",
      "Umpikorinen (BB)",
      "Viistoperä (AB)"
    ),
    korityyppi,
    "Muu"
  )
  kori = gsub(" \\(.*$|ajoneuvo|auto", "", kori)
  return(kori)
}

map.autovarikartta <-
  function(v)
    plyr::mapvalues(
      v,
      c(
        "Valkoinen",
        "Sininen",
        "Musta",
        "Harmaa",
        NA,
        "Punainen",
        "Vihreä",
        "Ruskea (beige)",
        "Monivär.",
        "Hopea",
        "Keltainen",
        "Violetti",
        "Oranssi",
        "Turkoosi"
      ),
      c(
        "snow2",
        "blue",
        "black",
        "gray",
        "gray",
        "red",
        "green",
        "brown",
        "red",
        "silver",
        "yellow",
        "purple",
        "organge",
        "cyan"
      ),  warn_missing = FALSE
    )

kuntanumeromap2018 <- readRDS(file=here::here("Data","kuntanumeromap2018.rds"))

# Kuntanumerosta kunnan nykyinen nimi
map.kunta <- function(v) 
  plyr::mapvalues(as.integer(v), 
                  as.integer(kuntanumeromap2018$kuntano.old), 
                  kuntanumeromap2018$kunta, 
                  warn_missing = FALSE) %>% 
  iconv(.,to="UTF-8")

# Kuntanumerosta kunnan nykyinen numero
map.kuntano <- function(v)
  plyr::mapvalues(as.integer(v), 
                  as.integer(kuntanumeromap2018$kuntano.old),
                  kuntanumeromap2018$kuntano,
                  warn_missing = FALSE)


#Postinumero tai kunta on ahvenanmaalla
is.ahvenanmaa <-function(v)  ifelse(v %in% c("Sottunga","Föglö", "Kumlinge", "Lumparland", "Sund", 
                                             "Vårdö", "Hammarland", "Eckerö","Lemland", "Finström", 
                                             "Geta", "Kökar","Saltvik","Jomala","Brändö","Maarianhamina","Mariehamn") | 
                                      grepl("^22",v), TRUE, FALSE)


map.vanhat.kuntanimet<-
  function(v) plyr::mapvalues(v,c("Maarianhamina - Mariehamn","Pedersören kunta","Koski Tl"), 
                              c("Maarianhamina",iconv("Pedersören",to="UTF-8"),"Koski"))


# korvataan ylä- ja alarajalla
cut.lh <- function(x, limits) {
  x <- replace(x, x < limits[1], limits[1])
  x <- replace(x, x > limits[2], limits[2]) 
  return(x)
}

# nvl => coalesce
#nvl <- function(a, b) {
#  ifelse(is.na(a), b, a)
#}

# Laske edellisen oletetun katsastuspäivämäärän jälkeinen kvartaalin viimeinen päivä
map.default.katsastus.kvartaali <- function(data.date, kayttoonottopvm) {
  i <- interval(ymd(kayttoonottopvm), ymd(data.date)) / years(1)
  i <- floor(i)
  j <- ifelse(i < 3, NA, ifelse(i >= 5, i, 3))
  as.character(ceiling_date(ymd(kayttoonottopvm) + years(j), "quarter") -
                 1)
}

#s.tab <- function(rownames, s, sep = " ", N = 1) {
#  s <- str_split(s, sep)
#  l <- sapply(s, length)
#  return(data.frame(
#    id = unlist(unname(mapply(
#      function(a, b)
#        rep(a, b), rownames, l
#    ))),
#    word = unname(unlist(s)),
#    N = unlist(mapply(function(a, b)
#      rep(a, b), N, l))
#  ))
#}

attribute.count <- function(autot, attr = "merkki", base = "kuntanimi")
{
  n <- group_by_(autot, base) %>% summarise(N = n()) %>% ungroup
  if (!is.na(attr)) {
    n[[attr]] <- "sum.N"
    rbind(n,
          group_by_(autot, base, attr) %>%
            summarise(N = n()) %>% ungroup %>% return)
  }
  else {
    n[["sum.N"]] <- "sum.N"
    return(n)
  }
}

count.normalized <- function(autot, attr = "merkki", base = "kuntanimi")
{
  group_by_(autot, .dots = c(base, attr)) %>%
    summarise(N = n()) %>%
    ungroup %>%
    group_by_(.dots = base) %>% mutate(sum.N = sum(N)) %>% ungroup
}

auto.rank <-
  function(autot,
           attr = "merkki",
           cf.limit = Inf,
           r.limit = Inf)
  {
    group_by_(autot, attr) %>%
      summarise(N = n()) %>%
      ungroup %>%
      mutate(r = rank(-N, ties.method = "max")) %>%
      arrange(r) %>%
      mutate(cf = cumsum(N) / sum(N), df = N / sum(N))
  }

auto.stat <-
  function(autot,
           attrs = c("merkki", "merkki.l.malli", "polttoaine", "kori", "vari"))
  {
    auto.stats <- list()
    for (a in attrs)
      auto.stats[[a]] <- auto.rank(autot, a)
    return(auto.stats)
  }

auto.aggregate <- function(autot,
                           group.attr = "kuntanimi",
                           attrs = c("matkamittarilukema",
                                     "suurinNettoteho",
                                     "Co2",
                                     "omamassa")) {
  select_(autot, .dots = unique(c(group.attr, attrs))) %>%
    group_by_(group.attr) %>%
    summarise_all(funs(mean(., na.rm = TRUE), median(., na.rm = TRUE)))
}


