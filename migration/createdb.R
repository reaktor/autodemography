source("initTrafi.R")

# File names  
files <- paste(
  here::here("Data"),
  c("AvoinData_4.2.csv",
    "AvoinData 4.3.csv",
    "AvoinData20160101.csv",
    "Ajoneuvot 4.6.csv",
    "AvoinData 4.5.csv",
    "AvoinData 4.7.csv",
    "AvoinData4.8_uusi.csv",
    "Tieliikenne AvoinData 4.9.csv",
    "Tieliikenne AvoinData 4.10.csv",
    "Tieliikenne 5.0.csv",
    "tieliikenne 5.1.csv",
    "Tieliikenne 5.2.csv",
    "Ajoneuvojen avoin data 5.3.csv",
    "Tieliikenne avoindata 5.4.csv"), 
  sep = "/"
)

datat <- list(
  file = files,
  ext = c(
    "4.02",
    "4.03",
    "4.04",
    "4.06",
    "4.05",
    "4.07",
    "4.08",
    "4.09",
    "4.10",
    "5.00",
    "5.01",
    "5.02",
    "5.03",
    "5.04"),
  sep = c(
    ",",
    ",",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";",
    ";")
)

# Make the db from scratch
#trafi.db <- src_sqlite(paste(here::here("Data"),"/trafi.db",sep=""), create=TRUE)
# Luo alusta
#for (z in datat$file) 
#{
#z<- datat$file[11]
#f<-  read.csv(file=z, header=TRUE,sep=datat[["sep"]][which(datat[["file"]] == z)],fileEncoding="latin1") 
#f<-  mutate(f,kayttoonottoVuosi = as.integer(str_sub(kayttoonottopvm,1,4)),
#            kayttoonottoVuosi = ifelse(kayttoonottoVuosi< 1900|kayttoonottoVuosi> liikaaVuosi, NA, kayttoonottoVuosi),
#            ensirekVuosi = as.integer(str_sub(ensirekisterointipvm,1,4)),
#            ensirekVuosi = ifelse(ensirekVuosi< 1900 | ensirekVuosi > liikaaVuosi, NA, ensirekVuosi),
#            max.date=max(as.character(ensirekisterointipvm), na.rm=TRUE) %>% as.character,
#            data=datat[["ext"]][which(datat[["file"]] == z)]) 
#
#db_insert_into(trafi.db$con,"ajoneuvot",f)
#}
# indexes
#CREATE INDEX luokka on ajoneuvot(ajoneuvoluokka);
#CREATE INDEX data on ajoneuvot(data);
#CREATE INDEX vuosi2 on ajoneuvot(kayttoonottoVuosi);
#CREATE INDEX jarnro on ajoneuvot(jarnro);

# Add just one file: 

## too far in future
liikaaVuosi <- 2019

# 5.03 = 13
# 5.04 = 14

z <- datat$file[14]

f <-  read.csv(file = z, 
               header=TRUE,sep=datat[["sep"]][which(datat[["file"]] == z)], 
               fileEncoding = "latin1") %>%
  select(one_of(c("ajoneuvoluokka", "ensirekisterointipvm", "ajoneuvoryhma", "ajoneuvonkaytto", "variantti", "versio", 
                  "kayttoonottopvm", "vari", "ovienLukumaara", "korityyppi", "ohjaamotyyppi", "istumapaikkojenLkm", 
                  "omamassa", "teknSuurSallKokmassa", "tieliikSuurSallKokmassa", "ajonKokPituus", "ajonLeveys", 
                  "ajonKorkeus", "kayttovoima", "iskutilavuus", "suurinNettoteho", "sylintereidenLkm", "ahdin", 
                  "sahkohybridi", "merkkiSelvakielinen", "mallimerkinta", "vaihteisto", "vaihteidenLkm", 
                  "kaupallinenNimi", "voimanvalJaTehostamistapa", "tyyppihyvaksyntanro", "yksittaisKayttovoima", "kunta", 
                  "Co2", "matkamittarilukema", "alue", "valmistenumero2", "jarnro")))

f <-  mutate(f,
             kayttoonottoVuosi = as.integer(str_sub(kayttoonottopvm, 1, 4)),
             kayttoonottoVuosi = ifelse(kayttoonottoVuosi< 1900 | kayttoonottoVuosi> liikaaVuosi, NA, kayttoonottoVuosi),
             ensirekVuosi = as.integer(str_sub(ensirekisterointipvm, 1, 4)),
             ensirekVuosi = ifelse(ensirekVuosi < 1900 | ensirekVuosi > liikaaVuosi, NA, ensirekVuosi),
             max.date=max(as.character(ensirekisterointipvm), na.rm=TRUE) %>% as.character,
             data=datat[["ext"]][which(datat[["file"]] == z)]) 
  
db_insert_into(trafi.db$con, "ajoneuvot", f)





