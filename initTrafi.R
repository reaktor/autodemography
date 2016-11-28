library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

working.directory<-"/Users/jhimberg/Projects/trafiOpenData"
setwd(working.directory)

map.trafi<-function(filepath="Data/16968-Koodisto_2015.csv", sep=";",fileEncoding="MAC") {
  
  koodisto<-read.csv(file=filepath,sep=sep,fileEncoding=fileEncoding,header = TRUE)
  
  # otetaan vain suomen kieli ja kivammat nimet
  # Ohjaamotyypin kohdalla PITKASELITE on suomeksi väärin, otetaan siinä lyhytselite
  # valitaan kieleksi suomi
  #
  # Ajoneuvoryhmälle on muakavampi ottaa myös lyhytselite
  
  # TRAFI: koodistoa... / lyhennetään nimiä > 
  
  map.koodit = c(
    "Kuntien numerot ja nimet" = "kunta", 
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
  
  # datassa on ollut kunta 180, (Jyväskylän mlk) ja 841 (Temmes) joka on 
  # suurimmaksi osaksi "Tyrnävä"
  # lisätään kuntakoodi 841 : "Tyrnävä, 180: "Jyväskylä" (HUOM: koodit ovat ilmeisesti alunperin 
  # uniikkeja; on myös automerkki 841, käytettävä aina kuvaus+koodi -paria
  
  koodit[dim(koodit)[1]+1,]<-c("kunta",841,"Tyrnävä")
  koodit[dim(koodit)[1]+1,]<-c("kunta",180,"Jyväskylä")
  koodit[dim(koodit)[1]+1,]<-c("kaytto","nul","x")
  koodit[dim(koodit)[1]+1,]<-c("kaytto","","x")
  koodit[dim(koodit)[1]+1,]<-c("kori","","x")
  koodit[dim(koodit)[1]+1,]<-c("vari","","x")
  koodit[dim(koodit)[1]+1,]<-c("ryhma","26","Maastohenkilöauto")
  koodit[dim(koodit)[1]+1,]<-c("ryhma","508","Henkilöauto")
  koodit[dim(koodit)[1]+1,]<-c("ryhma","61","Selväkielisenä syötettävä nimitys")
  koodit[dim(koodit)[1]+1,]<-c("ryhma",NA,"x")
  
  ## Kooditaulut
  map.trafi=list()
  for (i in unique(koodit$kuvaus)) {
    map.trafi[[i]] <-filter(koodit, kuvaus==i) %>% 
      transmute(., koodi = as.character(koodi), nimi=as.character(nimi))}
  
  ## tehdään kuntien koodaus uudestaan: etunollat pois
  
  map.trafi[["kunta"]] <- filter(koodit, kuvaus=="kunta") %>% 
    transmute(., koodi = as.character(as.numeric(as.character(koodi))), nimi=as.character(nimi));
  
  
  # vaihdetaan M1G-koodi
  map.trafi[["luokka"]][["nimi"]][map.trafi[["luokka"]][["koodi"]]=="M1G"]<-"Henkilöauto.m"
  
  # vaihdetaan joitakin kuntakoodeja: tuntemattomat, ulkomaat ja Ahvenanmaa (åland-rekisteri on erikseen)
  map.trafi[["kunta"]][["nimi"]][map.trafi[["kunta"]][["nimi"]] %in% 
                                   c("Sottunga","Föglö", "Kumlinge", "Lumparland", "Sund", 
                                     "Vårdö", "Hammarland", "Eckerö","Lemland", "Finström", 
                                     "Geta", "Kökar","Saltvik","Jomala","Brändö","Maarianhamina", 
                                     "Pohjoismaat","Ei vak asuinkuntaa","Ulkomaat","Tuntematon",NA)]<-"x"
  
  
  ## lyhennellään värien koodauksia
  
  map.trafi[["vari"]]["nimi"]<-mapvalues(as.vector(map.trafi[["vari"]][["nimi"]]), c("Ruskea (beige)"), c("Ruskea"));
  
  ## lyhennellään käyttötavan koodausta # korjataan koodia
  map.trafi[["kaytto"]]["nimi"]<-gsub(" .*$","",map.trafi[["kaytto"]][["nimi"]])
  return(map.trafi)}

## Polttoaineiden uudelleenkoodausta

map.polttoaine<-list()
map.polttoaine$orig<-c("","Bensiini","Biodiesel","Biometaani","CNG","Dieselöljy","Etanoli","Etanoli (E85)","HL-ryhmän maakaasu","LPG","Sähkö")
map.polttoaine$lyh<-c(NA,"Bensiini","Diesel","Kaasu","Kaasu","Diesel","Etanoli","Etanoli","Kaasu","Kaasu","Sähkö")


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

nvl <- function(a,b){
  ifelse(is.na(a),b,a)
}

autovarikartta.map<-function(v) mapvalues(v,c("Valkoinen","Sininen","Musta","Harmaa",
                                    NA,"Punainen","Vihreä","Ruskea (beige)","Monivär.","Hopea","Keltainen","Violetti","Oranssi","Turkoosi"),
                              c("snow2","blue","black","gray","gray","red","green","brown","red","silver","yellow","purple","organge","cyan"))


ikaluokka.map<-
  function(v) as.numeric(mapvalues(as.character(v), 
                        c("0-4",  "5-9",  "10-14","15-19","20-24",
                          "25-29","30-34","35-39","40-44","45-49",
                          "50-54","55-59","60-64","65-69","70-74",
                          "75-79","80-84","85-89","90-"),
                        c(2,       7,      12,    17,      22,
                          27,      32,     37,    42,      47,
                          52,      57,     62,    67,      72,
                          77,      82,     87,    91)))

#n<-notmissing(select(autot,ends_with(".orig"),ryhma,k.malli,data),misvalues=c("",NA,"x"),
#              c("data","kayttoonottoVuosi.orig")) %>% 
#  mutate(N=1) %>% 
#  mutate(kayttoonottoVuosi.orig=ifelse(kayttoonottoVuosi.orig<1960,1959,kayttoonottoVuosi.orig)) %>%
#  group_by(kayttoonottoVuosi.orig,data) %>% 
#  summarise_each(funs(sum)) %>%
#  mutate_each(funs(./N),-N,-kayttoonottoVuosi.orig,-data) 
#ggplot(data=melt(select(n,-N),id.vars=c("data","kayttoonottoVuosi.orig")), 
#       aes(x=kayttoonottoVuosi.orig,y=value,color=data))+geom_line()+facet_wrap(~variable,scales="fixed")



