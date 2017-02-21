# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("~/Projects/autodemography/initTrafi.R")

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

autot.orig<-tbl(trafi.db, "ajoneuvot") %>% 
  filter(ajoneuvoluokka %in% c("M1","M1G")) %>% 
  collect(n=Inf)

henkiloautot<-mutate(autot.orig,
              kayttoonotto = as.character(as.Date(as.character(kayttoonottopvm),"%Y%m%d")),
              kayttoonotto=ifelse(is.na(kayttoonottoVuosi),NA,kayttoonotto),
              kayttoonotto.pvm.imputoitu = is.na(kayttoonotto),
              kayttoonottopvm = ifelse(is.na(kayttoonotto),
                                       paste(as.character(kayttoonottoVuosi),
                                             "-06-30",sep=""),
                                       as.character(as.Date(kayttoonotto,origin="1970-01-01"))),
              ensirekisterointipvm = as.character(ensirekisterointipvm),
              #merkkiSelvakielinen=iconv(merkkiSelvakielinen,from="latin1",to="UTF-8"),
              #mallimerkinta=iconv(mallimerkinta,from="latin1",to="UTF-8"), 
              kunta=str_pad(kunta, 3, side="left", pad="0"),
              kunta=ifelse(is.na(kunta),"999",kunta),
              #kaupallinenNimi=iconv(kaupallinenNimi,from="latin1","UTF-8"),
              vaihteisto=map.trafi("vaihteisto",vaihteisto),
              ohjaamotyyppi=map.trafi("ohjaamo",ohjaamotyyppi),
              korityyppi=map.trafi("kori",korityyppi),
              vari=map.trafi("vari",vari),
              ajoneuvoluokka=map.trafi("luokka",ajoneuvoluokka),
              ajoneuvoryhma=map.trafi("ryhma",ajoneuvoryhma),
              ajoneuvonkaytto=map.trafi("kaytto",ajoneuvonkaytto),
              yksittaisKayttovoima=map.trafi("polttoaine",yksittaisKayttovoima),
              kayttovoima=map.trafi("polttoaine",kayttovoima),
              voimanvalJaTehostamistapa=map.trafi("valitys",voimanvalJaTehostamistapa))

rm(autot.orig)

henkiloautot <- 
  mutate(henkiloautot, 
         ensirekisterointipvm=ifelse(ensirekisterointipvm=="",NA,ensirekisterointipvm),
         ensirekVuosi=ifelse(ensirekisterointipvm=="",NA,ensirekVuosi))

##

# muuttujat jotka voivat selvästi muuttua 
# jos teknisiä tietoa muutetaan (voihan väri tai moottori vaihtua), auto "menee rikki"
# toivotaan että tätä sattuuu aika harvoin

id.attribuutit<-names(select(henkiloautot,
                             -kunta,
                             -alue,
                             -max.date,
                             -matkamittarilukema,
                             -ajoneuvonkaytto,
                             -data, 
                             -jarnro))

henkiloauto.historia <- mutate(henkiloautot,
                               attribuutti.id=group_indices_(henkiloautot,.dots=id.attribuutit)) %>%
  group_by(attribuutti.id) %>% 
  mutate(N.attribuutti.id=n(),
         historia=ifelse(length(unique(data))==N.attribuutti.id,T,F)) %>% 
  select(
    historia,
    N.attribuutti.id,
    attribuutti.id, 
    data, 
    jarnro, 
    kunta,
    alue,
    ajoneuvonkaytto, 
    matkamittarilukema, 
    max.date) %>%
  ungroup

henkiloauto.viimeisimmat <- bind_rows(
  filter(henkiloauto.historia, historia) %>% 
    group_by(attribuutti.id) %>% 
    arrange(data) %>% 
    slice(n()) %>% 
    ungroup,
  filter(henkiloauto.historia, !historia) 
) %>% 
  mutate(id=row_number())

henkiloauto.historia<-bind_rows(
  left_join(filter(henkiloauto.historia,historia), 
            filter(henkiloauto.viimeisimmat,historia) %>% select(id,attribuutti.id), by="attribuutti.id"),
  left_join(filter(henkiloauto.historia,!historia), 
            filter(henkiloauto.viimeisimmat,!historia) %>% select(id,jarnro,data,attribuutti.id), by=c("jarnro","data","attribuutti.id"))
)

henkiloautot <- left_join(henkiloautot, select(henkiloauto.historia, data, jarnro, id, historia), by=c("data","jarnro")) %>%
  group_by(id) %>% mutate(N.id=n()) %>% ungroup
  
henkiloauto.viimeisimmat <- left_join(select(henkiloauto.viimeisimmat, jarnro, data), henkiloautot, by=c("data","jarnro"))
henkiloauto.historia <- filter(henkiloauto.historia, historia) %>% 
  select(-N.attribuutti.id,-historia,-attribuutti.id) 

if (db_has_table(trafi.db$con,"henkiloautot")) db_drop_table(trafi.db$con,"henkiloautot")
as.data.frame(henkiloautot) %>% db_insert_into(trafi.db$con,"henkiloautot",.)

if (db_has_table(trafi.db$con,"henkiloauto_viimeisin")) db_drop_table(trafi.db$con,"henkiloauto_viimeisin")
as.data.frame(henkiloauto.viimeisimmat) %>% db_insert_into(trafi.db$con,"henkiloauto_viimeisin",.)

if (db_has_table(trafi.db$con,"henkiloauto_historia")) db_drop_table(trafi.db$con,"henkiloauto_historia")
as.data.frame(henkiloauto.historia) %>% db_insert_into(trafi.db$con,"henkiloauto_historia",.)

