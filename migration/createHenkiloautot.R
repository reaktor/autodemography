# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("~/Projects/autodemography/initTrafi.R")

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

# Valitaan varsinaiset henkilöautot ja uudelleenkoodataan muuttujia

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
              kunta=str_pad(kunta, 3, side="left", pad="0"),
              kunta=ifelse(is.na(kunta),"999",kunta),
              vaihteisto=map.trafi("vaihteisto",vaihteisto),
              ohjaamotyyppi=map.trafi("ohjaamo",ohjaamotyyppi),
              korityyppi=map.trafi("kori",korityyppi),
              vari=map.trafi("vari",vari),
              ajoneuvoluokka=map.trafi("luokka",ajoneuvoluokka),
              ajoneuvoryhma=map.trafi("ryhma",ajoneuvoryhma),
              ajoneuvonkaytto=map.trafi("kaytto",ajoneuvonkaytto),
              yksittaisKayttovoima=map.trafi("polttoaine",yksittaisKayttovoima),
              kayttovoima=map.trafi("polttoaine",kayttovoima),
              voimanvalJaTehostamistapa=map.trafi("valitys",voimanvalJaTehostamistapa)) %>%
  select(-kayttoonotto)

rm(autot.orig)

henkiloautot <- 
  mutate(henkiloautot, 
         ensirekisterointipvm=ifelse(ensirekisterointipvm=="",NA,ensirekisterointipvm),
         ensirekVuosi=ifelse(ensirekisterointipvm=="",NA,ensirekVuosi))

##
# muuttujat jotka voivat selvästi muuttua otatetaan pois 
# jos teknisiä tietoa muutetaan (voihan väri tai moottori vaihtua...), auto "menee teknisesti rikki"
# toivotaan että tätä sattuuu aika harvoin (ahdin ja sahkohybridi pitäisi ehkä ottaa pois, jos ne ovat johdettuja muuttujia?)

combo.attribute<-names(select(henkiloautot,
                             -kunta,
                             -alue,
                             -max.date,
                             -matkamittarilukema,
                             -ajoneuvonkaytto,
                             -data, 
                             -jarnro))

# id.attribuuttien yhdistelmä on pseudo-id: haetaan sellaiset joille on enintään 
# yksi yhtä data id:tä (4.2, 4.3, ...) kohti. Näille annetaan pseudo.id

henkiloautot$combo = group_indices_(henkiloautot,.dots=combo.attribute)

henkiloautot<- group_by(henkiloautot, combo) %>%
  mutate(N.combo = n(), 
         historia = ifelse(length(unique(data)) == N.combo, T, F)) %>% 
  ungroup %>% 
  mutate(record.id = ifelse(historia, str_pad(combo, 8, side = "left", pad ="0"), NA))

# otataan vielä ne joilla on uniikki alue ja mukana kaikissa datoissa 4.xx vain kerran

h<-filter(henkiloautot, !historia)
henkiloautot<-filter(henkiloautot, historia)

N.data=length(unique(henkiloautot$data))
combo.attribute<-unique(c(combo.attribute, "alue"))


h$t.id <- group_indices_(h, .dots=combo.attribute)

h<- group_by(h,t.id) %>% 
  mutate(N.t.uniq.id = length(unique(data)), 
         N.t.id = length(data)) %>% 
  ungroup

h<-mutate(h,
    stabile.alue = ifelse(N.data == N.t.id & N.data==N.t.uniq.id, T, F),
    historia = ifelse(N.data == N.t.id, T, F),
    record.id = ifelse(stabile.alue, paste("1", str_pad(alue,3, side="left", pad=0), 
                                           str_pad(combo, 8, side="left", pad="0"), sep=""), record.id),
    historia = ifelse(!historia & stabile.alue, T, historia)
  ) 

henkiloautot <- bind_rows(henkiloautot,h) %>% 
  select(-t.id, -historia, -stabile.alue, -N.t.id) %>% 
  rename(date=max.date) 

rm(h)

henkiloautot <- mutate(henkiloautot, 
                       data=paste(str_sub(data,1,2),str_pad(str_sub(data,3,5), 2,"left", pad="0"),sep=""))

henkiloauto.historia<-select(henkiloautot,
     record.id,
     combo, 
     data, 
     jarnro, 
     kunta,
     alue,
     ajoneuvonkaytto, 
     matkamittarilukema, 
     date) %>%
  ungroup

# Uniikit combot otetaan yksi (vanhimmasta datasta) jokaiselle record.id:lle 

henkiloauto.uniikit <- group_by(henkiloautot, combo) %>% 
  arrange(data) %>% 
  slice(1) %>% 
  ungroup %>% 
  select( 
    -record.id, 
    -date, 
    -data, 
    -alue, 
    -jarnro, 
    -ajoneuvonkaytto, 
    -matkamittarilukema, 
    -kunta)

# record.id on nyt "pseudoid" - jos se puuttuu ei autoa voida combonkaan avulla identifioida 
# samaksi eri dataseteissä

# Kaikki henkilöautot kantaan ja fileen

if (db_has_table(trafi.db$con,"henkiloautot")) db_drop_table(trafi.db$con, "henkiloautot")
as.data.frame(henkiloautot) %>% db_insert_into(trafi.db$con, "henkiloautot",.)

con <- DBI::dbConnect(RSQLite::SQLite(), paste(working.directory,"/trafi.db",sep=""))
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hkaytto on henkiloautot(ajoneuvonkaytto);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hdata on henkiloautot(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvuosi on henkiloautot(kayttoonottoVuosi);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hjarnro on henkiloautot(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hrecord on henkiloautot("record.id");')
DBI::dbDisconnect(con)

if (db_has_table(trafi.db$con,"henkiloauto_historia")) db_drop_table(trafi.db$con, "henkiloauto_historia")
as.data.frame(henkiloauto.historia) %>% db_insert_into(trafi.db$con, "henkiloauto_historia",.)

con <- DBI::dbConnect(RSQLite::SQLite(), paste(working.directory,"/trafi.db",sep=""))
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhdata on henkiloauto_historia(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhjarnro on henkiloauto_historia(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hhrid on henkiloauto_historia("record.id");')
DBI::dbDisconnect(con)

if (db_has_table(trafi.db$con,"henkiloauto_uniqcombos")) db_drop_table(trafi.db$con, "henkiloauto_uniqcombos")
as.data.frame(henkiloauto.uniikit) %>% db_insert_into(trafi.db$con, "henkiloauto_uniqcombos",.)

con <- DBI::dbConnect(RSQLite::SQLite(), paste(working.directory,"/trafi.db",sep=""))
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvryhma on henkiloauto_uniqcombos(ajoneuvoryhma);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvvuosi on henkiloauto_uniqcombos(kayttoonottoVuosi);")
DBI::dbDisconnect(con)

write.table(henkiloauto.historia, file="henkiloautot.txt",quote=FALSE,
            sep="\t", na="", dec=".", row.names=FALSE, fileEncoding="UTF-8")

write.table(henkiloauto.historia, file="henkiloauto_historia.txt",quote=FALSE,
            sep="\t", na="", dec=".", row.names=FALSE, fileEncoding="UTF-8")

write.table(henkiloauto.uniikit, file="henkiloauto_uniqcombos.txt",quote=FALSE,
            sep="\t", na="", dec=".", row.names=FALSE, fileEncoding="UTF-8")


