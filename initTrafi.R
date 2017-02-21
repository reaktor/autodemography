library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

working.directory<-"~/Projects/autodemography"
setwd(working.directory)

load("Data/vaesto.RData")

data.map.trafi <- function() {
  koodisto<-read.csv(file="Data/16968-Koodisto_2015.csv",sep=";",fileEncoding="MAC",header = TRUE)
  
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
    "Vaihteistotyyppi" = "vaihteisto")
  
  koodit<-
    transmute(koodisto,kieli=as.character(KIELI),
              kuvaus=as.character(KOODISTONKUVAUS),
              koodi=as.character(KOODINTUNNUS),
              nimi=as.character(PITKASELITE),
              nimi.2=as.character(LYHYTSELITE)) %>% 
    filter(kieli=="fi") %>% 
    select(-kieli) %>%
    mutate(kuvaus=revalue(kuvaus,map.koodit),
           nimi=ifelse(kuvaus=="ohjaamo",nimi.2, nimi),
           nimi=ifelse(kuvaus=="luokka",nimi.2, nimi)) %>%
    select(-nimi.2)
  
  koodit[dim(koodit)[1]+1,]<-c("kaytto","nul",NA)
  koodit[dim(koodit)[1]+1,]<-c("kaytto","",NA)
  koodit[dim(koodit)[1]+1,]<-c("kori","",NA)
  koodit[dim(koodit)[1]+1,]<-c("vari","",NA)
  koodit[dim(koodit)[1]+1,]<-c("ryhma","26","Maastohenkilöauto")
  koodit[dim(koodit)[1]+1,]<-c("ryhma","508","Henkilöauto")
  koodit[dim(koodit)[1]+1,]<-c("ryhma","61","Selväkielisenä syötettävä nimitys")
  koodit[dim(koodit)[1]+1,]<-c("ryhma",NA,NA)
  
  ## Kooditaulut
  map.trafi=list()
  for (i in unique(koodit$kuvaus)) {
    map.trafi[[i]] <-filter(koodit, kuvaus==i) %>% 
      transmute(., koodi = as.character(koodi), nimi=as.character(nimi))}
  
  # vaihdetaan M1G-koodi
  map.trafi[["luokka"]][["nimi"]][map.trafi[["luokka"]][["koodi"]]=="M1G"]<-"Henkilöauto (maasto)"
  
  ## lyhennellään värien koodauksia
  
  map.trafi[["vari"]]["nimi"]<-mapvalues(as.vector(map.trafi[["vari"]][["nimi"]]), c("Ruskea (beige)"), c("Ruskea"));
  
  ## lyhennellään käyttötavan koodausta # korjataan koodia
  map.trafi[["kaytto"]]["nimi"]<-gsub(" .*$","",map.trafi[["kaytto"]][["nimi"]])
  # on char
  map.trafi[["valitys"]]["koodi"]<-str_pad(map.trafi[["valitys"]][["koodi"]], 1, side="left", pad="0")
  return(map.trafi)
}

# Trafi muuttujien uudeleenkoodausfunktio
map.trafi<-function(var.nimi,orig.val, data=data.map.trafi()) {
  mapvalues(orig.val,data[[var.nimi]][["koodi"]],data[[var.nimi]][["nimi"]])
}
  
# vaihdetaan joitakin kuntakoodeja: tuntemattomat, ulkomaat ja Ahvenanmaa (åland-rekisteri on erikseen)
#map.trafi[["kunta"]][["nimi"]][map.trafi[["kunta"]][["nimi"]] %in% 
#                                 c("Sottunga","Föglö", "Kumlinge", "Lumparland", "Sund", 
#                                   "Vårdö", "Hammarland", "Eckerö","Lemland", "Finström", 
#                                   "Geta", "Kökar","Saltvik","Jomala","Brändö","Maarianhamina", 
#                                   "Pohjoismaat","Ei vak asuinkuntaa","Ulkomaat","Tuntematon",NA)]<-NA


## Polttoaineiden uudelleenkoodausta

map.polttoaine <- function(v) mapvalues(v,
                                        c("Bensiini","Dieselöljy","Bensiini/Etanoli","Bensiini/CNG", "Sähkö", "CNG", "Bensiini/Sähkö", "", "Bensiini + moottoripetroli", "Muu","Bensiini/Puu", "Diesel/Biodiesel", "Bensiini/LPG", "Diesel/CNG", "Etanoli", "Etanoli (E85)",             "Diesel/Sähkö","Biometaani","Diesel/Biodiesel/CNG","HL-ryhmän maakaasu","LPG", "Puu","Vety"),
                                        c("Bensiini","Diesel",    "Bensiini/Etanoli","Bensiini/CNG", "Sähkö", "CNG", "Bensiini/Sähkö", NA, "Bensiini",                   "Muu","Muu",          "Diesel",           "Muu",          "Muu",        "Bensiini/Etanoli", "Bensiini/Etanoli", "Muu","Muu","Muu","Muu", "Muu", "Muu", "Muu")
                                        )

map.kunta <- function(v) 
  mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
            as.character(geo$kunta.vanhat2uudet$kunta))

map.kuntano <- function(v)
  mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
                  as.character(geo$kunta.vanhat2uudet$kuntano))

map.korityyppi <- function(korityyppi) {
kori=ifelse(korityyppi %in% c(NA,"Avoauto (AE)", "Coupé (AD)", "Farmari (AC)", 
                              "Matkailuauto (SA)", "Monikäyttöajoneuvo (AF)", "Sedan (AA)", 
                              "Umpikorinen (BB)", "Viistoperä (AB)"), korityyppi,"Muu")
kori=gsub(" \\(.*$|ajoneuvo|auto","",kori)
return(kori)}

# korvataan low-arvoa pienemmät "repvaluella"
limitl<-function(x,low,repvalue){ 
  x[x<low & !is.na(x)]<-repvalue;
  return(x);
}

# korvataan high-arvoa suuremmat "repvaluella"
limith<-function(x,high,repvalue){ 
  x[x>high & !is.na(x)]<-repvalue;
  return(x);
}

# puuttuuko arvo (misvalues) vai ei 
notmissing<-function(z,misvalues=NA, nomis=NA) {
  misvalues<-cbind(misvalues,NA);
  m<-lapply(z[,!(names(z) %in% nomis)], function(x) {!(x %in% misvalues)}) 
  return(cbind(data.frame(m),z[nomis]))
}

s.tab<-function(rownames, s, sep=" ",N=1) {
  s<-str_split(s,sep)
  l<-sapply(s,length)
  return(data.frame(id=unlist(unname(mapply(function(a,b) rep(a,b), rownames, l))),
                    word=unname(unlist(s)),
                    N= unlist(mapply(function(a,b) rep(a,b), N, l))))
}

nvl <- function(a,b) {
  ifelse(is.na(a),b,a)
}

map.autovarikartta<-function(v) mapvalues(v,c("Valkoinen","Sininen","Musta","Harmaa",
                                    NA,"Punainen","Vihreä","Ruskea (beige)","Monivär.","Hopea","Keltainen","Violetti","Oranssi","Turkoosi"),
                              c("snow2","blue","black","gray","gray","red","green","brown","red","silver","yellow","purple","organge","cyan"))

map.ikaluokka<-
  function(v) as.numeric(mapvalues(as.character(v), 
                        c("0-4",  "5-9",  "10-14","15-19","20-24",
                          "25-29","30-34","35-39","40-44","45-49",
                          "50-54","55-59","60-64","65-69","70-74",
                          "75-79","80-84","85-89","90-"),
                        c(2,       7,      12,    17,      22,
                          27,      32,     37,    42,      47,
                          52,      57,     62,    67,      72,
                          77,      82,     87,    91)))

