library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyr)
library(lubridate)

working.directory<-"~/Projects/autodemography"
setwd(working.directory)


source("initGeoDemografia.R")

load("Data/geografia.RData")
load("Data/demografia.RData")

demografia$postinumero$suhteellinen <- demografia$postinumero$data %>%
  select(-starts_with("he_0")) %>% 
  select(-starts_with("he_1")) %>%
  select(-starts_with("he_2")) %>% 
  select(-starts_with("he_3")) %>% 
  select(-starts_with("he_4")) %>% 
  select(-starts_with("he_5")) %>%
  select(-starts_with("he_6")) %>% 
  select(-starts_with("he_7")) %>%
  select(-starts_with("he_8")) %>%
  select(-nimi,-kuntano) %>% 
  mutate(naiset.osuus=he_naiset/(he_miehet+he_naiset)) %>%
  mutate_at(vars(starts_with("ko_"),-ko_ika18y),funs(./ko_ika18y)) %>%
  mutate_at(vars(hr_pi_tul,hr_ke_tul,hr_hy_tul,hr_ovy), funs(./hr_tuy)) %>%
  mutate_at(vars(starts_with("pt_"),-pt_vakiy), funs(./pt_vakiy)) %>%
  mutate_at(vars(starts_with("tp_"),-tp_tyopy), funs(./tp_tyopy)) %>%
  mutate_at(vars(starts_with("te_"),-te_taly,-te_takk,-te_as_valj), funs(./te_taly)) %>%
  mutate_at(vars(starts_with("tr_"),-tr_kuty,-tr_ktu,-tr_mtu), funs(./tr_kuty)) %>%
  mutate_at(vars(starts_with("ra_"),-ra_raky,-ra_as_kpa), funs(./ra_raky)) %>% 
  mutate(aluejako=plyr::mapvalues(pono.level,c(1,2,3),c("pono.1","pono.2","pono.3"))) %>% 
  select(-pono.level) %>% 
  rename(alue=pono) %>% 
  mutate(vuosi=as.numeric(vuosi))

load("Data/kunnat.kartogrammi.RData")
geo$kunta$kartogrammi<-transmute(kunnat.kartogrammi,id,long,lat,order,hole,piece,group,kunta,kuntanimi=iconv(kuntanimi,to="UTF-8"))
rm(kunnat.kartogrammi)

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
    mutate(kuvaus=plyr::revalue(kuvaus,map.koodit),
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
  
  map.trafi[["vari"]]["nimi"]<-plyr::mapvalues(as.vector(map.trafi[["vari"]][["nimi"]]), c("Ruskea (beige)"), c("Ruskea"));
  
  ## lyhennellään käyttötavan koodausta # korjataan koodia
  map.trafi[["kaytto"]]["nimi"]<-gsub(" .*$","",map.trafi[["kaytto"]][["nimi"]])
  # character padding oli tarpeen jossain vaiheessa, mutta 2017 muuttui takaisin
  #map.trafi[["valitys"]]["koodi"]<-str_pad(map.trafi[["valitys"]][["koodi"]], 1, side="left", pad="0")
  map.trafi[["valitys"]]["koodi"]<-as.numeric(map.trafi[["valitys"]][["koodi"]])
  return(map.trafi)
}

# Trafi muuttujien uudeleenkoodausfunktio
map.trafi<-function(var.nimi,orig.val, data=data.map.trafi()) {
  plyr::mapvalues(orig.val,data[[var.nimi]][["koodi"]],data[[var.nimi]][["nimi"]])
}
  
## Polttoaineiden uudelleenkoodausta

map.polttoaine <- function(v) plyr::mapvalues(v,
                                        c("Bensiini","Dieselöljy","Bensiini/Etanoli","Bensiini/CNG", "Sähkö", "CNG", "Bensiini/Sähkö", "", "Bensiini + moottoripetroli", "Muu","Bensiini/Puu", "Diesel/Biodiesel", "Bensiini/LPG", "Diesel/CNG", "Etanoli", "Etanoli (E85)",             "Diesel/Sähkö","Biometaani","Diesel/Biodiesel/CNG","HL-ryhmän maakaasu","LPG", "Puu","Vety"),
                                        c("Bensiini","Diesel",    "Bensiini/Etanoli","Bensiini/CNG", "Sähkö", "CNG", "Bensiini/Sähkö", NA, "Bensiini",                   "Muu","Muu",          "Diesel",           "Muu",          "Muu",        "Bensiini/Etanoli", "Bensiini/Etanoli", "Muu","Muu","Muu","Muu", "Muu", "Muu", "Muu")
                                        )

map.korityyppi <- function(korityyppi) {
kori=ifelse(korityyppi %in% c(NA,"Avoauto (AE)", "Coupé (AD)", "Farmari (AC)", 
                              "Matkailuauto (SA)", "Monikäyttöajoneuvo (AF)", "Sedan (AA)", 
                              "Umpikorinen (BB)", "Viistoperä (AB)"), korityyppi,"Muu")
kori=gsub(" \\(.*$|ajoneuvo|auto","",kori)
return(kori)}


# rajoita ylä- ja alaraja muuttujalla kvantiileihin 
cut.quantile<- function(df, vars, quantile.limits=c(0.025,0.975)) {
  for (i in vars) df[[i]] <- cut.lh(df[[i]], quantile(df[[i]], quantile.limits, na.rm=TRUE))
return(df)}

# tasoitus lmerillä 

naivi.p <- function(df, alue="pono.3",  N="sum.N") {
  Y<-select_(df,alue)
  for (i in names(select(df, -one_of(c(alue,N))))) {
    f<-paste0( "cbind(`", i , "`," , N ,"-`", i ,"`) ~ (1|",alue,") + 1")
    print(i)
      m<-do.call("glmer", list(as.formula(f), data=as.name("df"), family=as.name("binomial")))
      c<- 1/(1+exp(-(ranef(m)[[alue]] + summary(m)$coefficients[1])))
      y<-data.frame(p=c[[1]], alue=dimnames(c[1])[[1]])
      names(y)<-c(paste0(i,".p"),alue)
      if (var(c[[1]])>0) Y<-merge(Y,y, by=alue)
  }
  return(Y)
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

map.autovarikartta<-function(v) plyr::mapvalues(v,c("Valkoinen","Sininen","Musta","Harmaa",
                                    NA,"Punainen","Vihreä","Ruskea (beige)","Monivär.","Hopea","Keltainen","Violetti","Oranssi","Turkoosi"),
                              c("snow2","blue","black","gray","gray","red","green","brown","red","silver","yellow","purple","organge","cyan"))


kartta <- function(df, aluejako="pono.3", title.label=NA, geo_=geo, color.map="PuBu", color.limits=c(NA,NA)) {
  # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan jonka nimi ei ole "alue" 
  geodata <- list()
  geodata[["pono.5"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=pono) 
  geodata[["pono.3"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,3))
  geodata[["pono.2"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,2))
  geodata[["pono.1"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,1))
  geodata[["kuntanimi"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntanimi)
  geodata[["kunta"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntano)
  geodata[["kartogrammi.kuntanimi"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kuntanimi)
  geodata[["kartogrammi.kunta"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kunta)
  geodata <- left_join(geodata[[aluejako]](geo_), df, by="alue")
  
  attr=names(select(df, -alue))[1] 
  if(is.na(title.label)) title.label <- attr
  p <- ggplot(data=arrange(geodata, order), aes(x=long, y=lat))+ 
    geom_polygon_interactive(aes_string(fill = attr, group="group", tooltip = "alue"), 
                             colour=NA)+
    theme_void()+theme(legend.title=element_blank())+ggtitle(title.label)
  
  p <- p + scale_fill_gradientn(colours= brewer.pal(9,color.map), 
                                values = NULL, 
                                space = "Lab", 
                                na.value = "grey50", 
                                guide = "colourbar", 
                                limits = color.limits)
  
  if(!grepl("^pono",aluejako)) 
    p<-p+coord_equal(ratio=1) 
  else 
    p<-p+coord_equal(ratio=2.1) 
  return(p)
}

kartta.animaatio <- function(df, aluejako="pono.3", geo_=geo, color.map="PuBu", title.label=NA) {
  # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan, ei muita
  
  geodata <- list()
  geodata[["pono.5"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=pono) 
  geodata[["pono.3"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,3))
  geodata[["pono.2"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,2))
  geodata[["pono.1"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,1))
  geodata[["kuntanimi"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntanimi)
  geodata[["kunta"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntano)
  geodata[["kartogrammi.kuntanimi"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kuntanimi)
  geodata <- left_join(geodata[[aluejako]](geo_), df, by="alue")
  attr=names(select(df, -alue, -aika))[1] 
  if(is.na(title.label)) title.label <- attr
  
  p <-ggplot(data=arrange(geodata,order), aes(x=long, y=lat, frame=aika))+ 
    geom_polygon_interactive(aes_string(fill=attr, group="group", tooltip="alue"), colour=NA)+
    scale_fill_gradientn(colours= brewer.pal(9,color.map), values = NULL, space = "Lab", na.value = "grey50", 
                         guide = "colourbar") + theme_void() +
    theme(legend.title=element_blank()) + ggtitle(title.label) 
  return(p)
}


attribute.count <- function(autot, attr="merkki", base="kuntanimi")
{ n<-group_by_(autot, base) %>% summarise(N=n()) %>% ungroup
  if(!is.na(attr)) {
    n[[attr]]<-"sum.N"
  rbind(n,group_by_(autot,base,attr) %>% 
          summarise(N=n()) %>% ungroup %>% return) }
  else {
    n[["sum.N"]]<-"sum.N"
    return(n) }
}

count.normalized <- function(autot, attr="merkki", base="kuntanimi")
{
  group_by_(autot,.dots=c(base,attr)) %>% 
    summarise(N=n()) %>%
    ungroup %>% 
    group_by_(.dots=base) %>% mutate(sum.N=sum(N)) %>% ungroup
  }

auto.rank <-function(autot, attr="merkki", cf.limit=Inf, r.limit=Inf)
{
  group_by_(autot,attr) %>% 
    summarise(N=n()) %>% 
    ungroup %>% 
    mutate(r=rank(-N,ties.method="max")) %>% 
    arrange(r) %>% 
    mutate(cf=cumsum(N)/sum(N),df=N/sum(N))
}  

auto.statistic<-function(autot,attrs=c("merkki","merkki.l.malli","polttoaine","kori","vari"))
{
  auto.stats<-list()
  for (a in attrs) auto.stats[[a]]<-auto.rank(autot,a) 
  return(auto.stats)
}

auto.aggregate <- function(autot, group.attr="kuntanimi", 
              attrs=c("matkamittarilukema",
                      "suurinNettoteho",
                      "Co2",
                      "omamassa")) {
  select_(autot, .dots=unique(c(group.attr,attrs))) %>% 
  group_by_(group.attr) %>% 
  summarise_all(funs(mean(.,na.rm=TRUE), median(.,na.rm=TRUE)))
}

fix.auto <- function(henkiloauto)
  mutate(henkiloauto,
         ryhma=ifelse(is.na(ajoneuvoryhma) & 
                        korityyppi=="Matkailuauto (SA)","Matkailuauto", ajoneuvoryhma),
         ryhma=ifelse(is.na(ryhma),
                      plyr::mapvalues(ajoneuvoluokka,c("Henkilöauto","Henkilöauto (maasto)"),
                                c("Henkilöauto","Maastohenkilöauto")),
                      ryhma),
         ryhma=plyr::mapvalues(ryhma,c(NA,"Henkilöauto",
                                 "Maastohenkilöauto","Maastoauto",
                                 "Matkailuauto","Museoajoneuvo"),
                         c("Henkilö","Henkilö",
                           "Maasto","Maasto",
                           "Matkailu","Museo")),
         ryhma=ifelse(ryhma %in% c("Henkilö","Maasto","Matkailu","Museo"), ryhma, "Muu"),
         ajoneuvoryhma=plyr::mapvalues(ajoneuvoryhma,c(NA,""),c("Henkilöauto","Henkilöauto")),
         kayttoonottoVuosi=ifelse(kayttoonottoVuosi<1900,NA,kayttoonottoVuosi),
         ovienLukumaara=limith(ovienLukumaara,8,NA),
         istumapaikkojenLkm=limith(istumapaikkojenLkm,9,NA),
         omamassa=limitl(omamassa,700,NA),
         omamassa=limith(omamassa,5000,NA),
         ajonKokPituus=limitl(ajonKokPituus,100,NA),
         ajonKokPituus=limith(ajonKokPituus,13500,NA),
         ajonLeveys=limith(ajonLeveys,2600,NA),
         ajonLeveys=limitl(ajonLeveys,50,NA),
         iskutilavuus=limitl(iskutilavuus,100,NA),
         iskutilavuus=limith(iskutilavuus,15000,NA),
         suurinNettoteho=limith(suurinNettoteho,520,NA),
         suurinNettoteho=round(limitl(suurinNettoteho,1,NA)),
         sylintereidenLkm=limitl(sylintereidenLkm,1,NA),
         sylintereidenLkm=limith(sylintereidenLkm,16,NA),
         ahdin=plyr::mapvalues(ahdin,c("","true","false"),c(NA,TRUE,FALSE)),
         sahkohybridi=plyr::mapvalues(sahkohybridi,c("","true","false"),c(NA,TRUE,FALSE)),
         kori=map.korityyppi(korityyppi),
         sahkohybridi=ifelse(kayttoonottoVuosi < 1997, FALSE, sahkohybridi),
         polttoaine=map.polttoaine(kayttovoima),
         polttoaine=ifelse(polttoaine == "Sähkö" & !is.na(Co2) & Co2 > 0, NA, polttoaine)) %>% 
  return

fix.auto.historia <- function(henkiloauto.historia)
  mutate(henkiloauto.historia,
         kuntano=map.kuntano(kunta),
         kuntanimi=map.kunta(kunta),
         matkamittarilukema.orig=matkamittarilukema,
         matkamittarilukema=limith(matkamittarilukema,999998,NA),
         matkamittarilukema=limitl(matkamittarilukema,1,NA),
         pono.3=str_pad(alue, 3, side="left", pad="0")) %>% 
  return

fix.merkki.malli <- function(henkiloautot, korjaustiedosto = "Data/mallitmerkkikorjaus.csv")
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
           l.malli=ifelse(merkki=="BMW", substring(l.malli,1,1), l.malli),
           l.malli=ifelse(merkki=="BMW" & grepl("COMPACT",malli), "COMPACT", l.malli),
           l.malli=ifelse(merkki=="MERCEDES-BENZ", str_match(l.malli,"^[0-9]{1,3}|^[A-Za-z]+"),l.malli),
           merkki.l.malli=paste(ifelse(is.na(merkki),"",merkki),
                                ifelse(is.na(l.malli),"",l.malli)),
           merkki.l.malli=ifelse(merkki.l.malli=="",NA,merkki.l.malli)) %>%
    select(-merkkiSelvakielinen) %>% 
    return
}

fix.kori <- function(henkiloauto, korjaustiedosto="Data/korikorjaus.csv")
{  
  ### Täydennetään puuttuvat koritiedot: 
  ### fix.merkit.mallit on ajettava ensin, samoin fix.attribuutit (kori oltava)
  
  korjaus<-read.csv("Data/korikorjaus.csv", 
                    quote="", 
                    fileEncoding = "UTF-8",
                    sep="\t", stringsAsFactors = FALSE) %>%
    mutate_if(is.character, iconv,to="UTF-8")
  
  ## jos puuluokittimen todennäköisyys on alle 0.4 tai suora lookupin p on korkeampi kuin 
  ## puuluokittimen p, käytetään suoraa lookuppia (vaikka sen frekvenssi olisi matala)
  ## kori: jos on, alkuperäinen (kori.orig) jos ei ole estimaatti, 
  
  kori <- left_join(select(henkiloauto, combo, mallimerkinta, malli, merkki, kori),
                    select(korjaus, malli, merkki, mallimerkinta,
                           kori.nn, kori.tree, p.kori.nn, p.kori.tree),
                    by=c("merkki","malli","mallimerkinta")) %>%
    mutate(kori.orig=kori,
           p=ifelse(is.na(p.kori.nn),0,p.kori.nn),
           kori.est=ifelse(p.kori.tree > p, kori.tree, kori.nn),
           kori.est=ifelse(p.kori.tree > p & p.kori.tree < 0.4, NA, kori.tree),
           kori=ifelse(is.na(kori), kori.est, kori))
  
  left_join(select(henkiloauto, -kori), select(kori, combo, kori.orig, kori, kori.est), 
            by=c("combo"))  %>% return
}

fix.co2 <-function(henkiloautot,korjaustiedosto.nn="Data/co2.stat.csv", co2.malli="Data/malli.co2.RData")
{
  
  ### Täydennetään puuttuvat Co2 -tiedot 
  ### fix.merkit.mallit, est.kori ja fix.attribuutit on ajettava ensin!!
  co2<-read.csv(korjaustiedosto.nn, 
                quote="", 
                fileEncoding = "UTF-8",
                sep="\t", stringsAsFactors = FALSE) %>%
    mutate_if(is.character,iconv,to="UTF-8")  
  
  load(co2.malli)
  
  henkiloautot <- mutate(henkiloautot,
                         Co2.orig=Co2,
                         Co2=ifelse(polttoaine == "Sähkö", 0, Co2),
                         Co2=ifelse((polttoaine != "Sähkö") & (Co2 == 0), NA, Co2),
                         Co2=ifelse(Co2 >700, NA,Co2))
  
  # Jos Co2>0, polttoaine ei voi olla sähkö, jos polttoaine = sähkö ja co2 puutuu = 0
  # Liitetään suoraan ne automallit+tilavuus joiden co2 2 yksikän sisällä, jos co2 puuttuu
  
  henkiloautot<-left_join(henkiloautot, filter(co2, co2.max-co2.min <= 2) %>% 
                            select(co2.nn,merkki,mallimerkinta,iskutilavuus),
                          by=c("merkki", "mallimerkinta", "iskutilavuus")) %>% 
    mutate(Co2=ifelse(is.na(Co2), co2.nn, Co2)) %>% select(-co2.nn)
  
  henkiloautot$Co2.modelled <- predict(malli.co2, 
                                       select(henkiloautot, 
                                              iskutilavuus,
                                              suurinNettoteho,
                                              omamassa,
                                              kayttoonottoVuosi, 
                                              polttoaine,
                                              kori) %>% data.matrix)
  
  henkiloautot <- mutate(henkiloautot,
                         Co2 = ifelse(!is.na(Co2.orig), Co2.orig, ifelse(is.na(Co2), Co2.modelled, Co2)),
                         Co2=ifelse(polttoaine == "Sähkö", 0, Co2))
  
  return(henkiloautot)
  
}

# Laske edellisen oletetun katsastuspäivämäärän jälkeinen kvartaalin viimeinen päivä

katsastus.Q <-function(data.date,kayttoonottopvm) {
  i<-interval(ymd(kayttoonottopvm),ymd(data.date)) / years(1)
  i<-floor(i)
  j<-ifelse(i<3,NA,ifelse(i>=5,i,3))
  as.character(ceiling_date(ymd(kayttoonottopvm)+years(j), "quarter")-1)
}

