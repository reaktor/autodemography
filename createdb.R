source("~/Projects/autodemography/initTrafi.R")

files<-paste(working.directory,"/Data/",c("AvoinData_4.2.csv",
              "AvoinData 4.3.csv",
              "AvoinData20160101.csv",
              "Ajoneuvot 4.6.csv",
              "AvoinData 4.5.csv",
              "AvoinData 4.7.csv",
              "Tieliikenne AvoinData 4,8.csv"),sep="")

datat <- list(file=files,
              ext=c("4.2","4.3","4.4","4.6","4.5","4.7","4.8"),
              sep=c(",",",",";",";",";",";",";"))

# Luo tietokanta
trafi.db <- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=TRUE)


# Avaa tietokanta (lisää)
#trafi.db <- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)
#add.data.n<-7

for (z in datat$file) 
  read.csv(file=z, header=TRUE,sep=datat[["sep"]][which(datat[["file"]] == z)],fileEncoding="latin1") %>% 
  mutate(kayttoonottoVuosi = as.integer(str_sub(kayttoonottopvm,1,4)),
         kayttoonottoVuosi = ifelse(kayttoonottoVuosi< 1900|kayttoonottoVuosi>2018,NA,kayttoonottoVuosi),
         ensirekVuosi = as.integer(str_sub(ensirekisterointipvm,1,4)),
         ensirekVuosi = ifelse(ensirekVuosi< 1900 | kayttoonottoVuosi>2018, NA, kayttoonottoVuosi),
         max.date=max(as.character(ensirekisterointipvm), na.rm=TRUE) %>% as.character,
         data=datat[["ext"]][which(datat[["file"]] == z)]) %>%
  db_insert_into(trafi.db$con,"ajoneuvot",.)

## Laske ajoneuvostatistiikat kaikille ajoneuvoluokille 

if (db_has_table(trafi.db$con,"stat_ajoneuvot")) db_drop_table(trafi.db$con,"stat_ajoneuvot")
tbl(trafi.db,"ajoneuvot") %>% 
  select(ajoneuvoluokka,
         ajoneuvoryhma,
         ajoneuvonkaytto,
         kunta, alue,
         kayttoonottoVuosi,
         ensirekVuosi,
         data) %>%
  collect(n=Inf) %>% 
  group_by(ajoneuvoluokka, ajoneuvoryhma, ajoneuvonkaytto, kunta, alue, kayttoonottoVuosi, ensirekVuosi, data) %>% 
  summarise(N=n()) %>% ungroup %>% 
  copy_to(trafi.db,.,"stat_ajoneuvot",temporary=FALSE)

## indeksoinnit tauluille
#CREATE INDEX luokka on ajoneuvot(ajoneuvoluokka);
#CREATE INDEX data on ajoneuvot(data);
#CREATE INDEX vuosi2 on ajoneuvot(kayttoonottoVuosi);
#CREATE INDEX jarnro on ajoneuvot(jarnro);

#CREATE INDEX hkaytto on henkiloautot(ajoneuvonkaytto);
#CREATE INDEX hryhma on henkiloautot(ajoneuvoryhma);
#CREATE INDEX hdata on henkiloautot(data);
#CREATE INDEX hvuosi on henkiloautot(kayttoonottoVuosi);
#CREATE INDEX hjarnro on henkiloautot(jarnro);

#CREATE INDEX hhdata on henkiloautohistoria(data);
#CREATE INDEX hhjarnro on henkiloautohistoria(jarnro);



