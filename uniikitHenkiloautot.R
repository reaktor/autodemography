# Indeksoi "uniikit" henkilöautot (idx)
# 

source("~/Projects/autodemography/initTrafi.R")

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

henkiloautot <-tbl(trafi.db,"henkiloautot") %>%
  collect(n=Inf)

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

viimeisimmat <- bind_rows(
  filter(henkiloauto.historia, historia) %>% 
    group_by(attribuutti.id) %>% 
    arrange(data) %>% 
    slice(n()) %>% 
    ungroup,
  filter(henkiloauto.historia, !historia) 
) %>% 
  mutate(id=row_number())

historia<-bind_rows(
left_join(filter(henkiloauto.historia,historia), 
          filter(viimeisimmat,historia) %>% select(id,attribuutti.id), by="attribuutti.id"),
left_join(filter(henkiloauto.historia,!historia), 
          filter(viimeisimmat,!historia) %>% select(id,jarnro,data,attribuutti.id), by=c("jarnro","data","attribuutti.id"))
)

henkiloautot <- left_join(henkiloautot, select(historia, data, jarnro, id, historia), by=c("data","jarnro"))
henkiloauto.viimeisin <- left_join(select(viimeisimmat, jarnro,data),henkiloautot, by=c("data","jarnro"))
henkiloauto.historia <- filter(h, historia) %>% 
  select(-N.attribuutti.id,-historia,-attribuutti.id) 


if (db_has_table(trafi.db$con,"henkiloautot")) db_drop_table(trafi.db$con,"henkiloautot")
as.data.frame(autot) %>% db_insert_into(trafi.db$con,"henkiloautot",.)

if (db_has_table(trafi.db$con,"henkiloauto_viimeisin")) db_drop_table(trafi.db$con,"henkiloauto_viimeisin")
as.data.frame(henkiloauto.viimeisin) %>% db_insert_into(trafi.db$con,"henkiloauto_viimeisin",.)

if (db_has_table(trafi.db$con,"henkiloauto_historia")) db_drop_table(trafi.db$con,"henkiloauto_historia")
as.data.frame(henkiloauto.historia) %>% db_insert_into(trafi.db$con,"henkiloauto_historia",.)
