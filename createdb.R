# Luo tietokanta
trafi.db <- src_sqlite("trafi.db", create = TRUE)

# Avaa tietokanta
#trafi.db <- src_sqlite("trafi.db", create = FALSE)

datat <- list(file=c("/Users/jhimberg/Projects/trafiOpenData/Data/AvoinData_4.2.csv",
         "/Users/jhimberg/Projects/trafiOpenData/Data/AvoinData 4.3.csv",
         "/Users/jhimberg/Projects/trafiOpenData/Data/AvoinData20160101.csv",
         "/Users/jhimberg/Projects/trafiOpenData/Data/Ajoneuvot 4.6.csv",
         "/Users/jhimberg/Projects/trafiOpenData/Data/AvoinData 4.5.csv",
         "/Users/jhimberg/Projects/trafiOpenData/Data/AvoinData 4.7.csv"),
         ext=c("4.2","4.3","4.4","4.6","4.5","4.7"),
         sep=c(",",",",";",";",";",";"))

for (z in datat$file) {
f<-read.csv(file=z,header=TRUE,sep=datat[["sep"]][which(datat[["file"]] == z)]) %>%  
  mutate(kayttoonotto = as.character(as.Date(as.character(kayttoonottopvm),"%Y%m%d")),
         kayttoonottoVuosi = 
           as.integer(substring(as.Date(as.character(kayttoonottopvm),"%Y"),1,4)),
         kayttoonotto=ifelse(is.na(kayttoonottoVuosi),NA,kayttoonotto),
         kayttoonotto.pvm.imputoitu = is.na(kayttoonotto),
         kayttoonottopvm = ifelse(is.na(kayttoonotto),
                                       paste(as.character(kayttoonottoVuosi),
                                             "-06-30",sep=""),
                                       as.character(as.Date(kayttoonotto,origin="1970-01-01"))),
         ensirekisterointipvm = as.character(ensirekisterointipvm),
         ensirekVuosi = as.integer(substring(as.Date(as.character(ensirekisterointipvm),"%Y"),1,4))
  ) 

max.date<-max(f$ensirekisterointipvm,na.rm=TRUE) %>% as.character

mutate(f,merkkiSelvakielinen=iconv(merkkiSelvakielinen,from="latin1",to="UTF-8"),
       mallimerkinta=iconv(mallimerkinta,from="latin1",to="UTF-8"), 
       kunta=mapvalues(kunta,map.trafi()[["kunta"]][["koodi"]],map.trafi()[["kunta"]][["nimi"]]),
       kunta=iconv(kunta,to="UTF-8"),
       kaupallinenNimi=iconv(kaupallinenNimi,from="latin1","UTF-8"),
       vaihteisto=as.character(mapvalues(vaihteisto,map.trafi()[["vaihteisto"]][["koodi"]],map.trafi()[["vaihteisto"]][["nimi"]])),
       korityyppi=as.character(mapvalues(korityyppi,map.trafi()[["kori"]][["koodi"]],map.trafi()[["kori"]][["nimi"]])),
       vari=as.character(mapvalues(vari,map.trafi()[["vari"]][["koodi"]],map.trafi()[["vari"]][["nimi"]])),
       ajoneuvoluokka=as.character(mapvalues(ajoneuvoluokka,map.trafi()[["luokka"]][["koodi"]],map.trafi()[["luokka"]][["nimi"]])),
       ajoneuvoryhma=as.character(mapvalues(ajoneuvoryhma,map.trafi()[["ryhma"]][["koodi"]],map.trafi()[["ryhma"]][["nimi"]])),
       ajoneuvonkaytto=as.character(mapvalues(ajoneuvonkaytto,map.trafi()[["kaytto"]][["koodi"]],map.trafi()[["kaytto"]][["nimi"]])),
       yksittaisKayttovoima=as.character(mapvalues(yksittaisKayttovoima,map.trafi()[["polttoaine"]][["koodi"]],map.trafi()[["polttoaine"]][["nimi"]])),
       data=datat[["ext"]][which(datat[["file"]] == z)],
       date=max.date) %>% 
  db_insert_into(trafi.db$con,"trafi",.)
}

# Frekvenssitaulu kaikille ajoneuvoluokille voidaan tehdä näin
trafi.db <- src_sqlite("trafi.db", create = FALSE)

tbl(trafi.db,"trafi") %>% 
  select(ajoneuvoluokka,
         ajoneuvoryhma,
         ajoneuvonkaytto,
         kunta,alue,
         kayttoonottoVuosi,
         ensirekVuosi,
         data) %>%
  collect(n=Inf) %>% 
  group_by(ajoneuvoluokka,
           ajoneuvoryhma,
           ajoneuvonkaytto,
           kunta,alue,
           kayttoonottoVuosi,
           ensirekVuosi,
           data) %>% 
  summarise(N=n()) %>% 
  ungroup %>% 
  copy_to(trafi.db,.,"stat_ajoneuvoluokka",temporary=FALSE)

####
# lasketaan tietojen olemassaolo 





