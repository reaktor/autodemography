# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("~/Projects/autodemography/initTrafi.R")

# Ajetaan merkkien/mallien korjaustiedosto!! luodaan skriptillä createMallitMerkitkorjaus.R
# (Voidaan editoida käsin)

#

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

# Henkilöautot
henkiloauto <- tbl(trafi.db,"henkiloauto_uniqcombos") %>% 
  select(combo, kayttoonottopvm, ensirekisterointipvm) %>% 
  collect(n = Inf) 
         
## pseudotietue

# Autojen oletettu historia - erotukset matkamittarilukemien välillä; 
# Onko auton matkamittarilukema muuttunut ="katsastettu"

henkiloauto.historia <- tbl(trafi.db,"henkiloauto_historia") %>% 
  filter(!is.na(record.id)) %>% 
  arrange(record.id, data) %>% 
  collect(n=Inf) %>%
  fix.auto.historia %>% 
  select(-kunta) %>%
  mutate(data=factor(data)) %>% 
  group_by(record.id) %>% 
  mutate(
         km.diff = matkamittarilukema - lag(matkamittarilukema),
         data.seuraava=lead(as.numeric(data))-as.numeric(data),
         katsastettu = nvl(nvl(matkamittarilukema.orig) != lag(nvl(matkamittarilukema.orig))),
         kuntano.seuraava=lead(kuntano),
         kuntanimi.seuraava=lead(kuntanimi),
         alue.seuraava=lead(alue),
         ajoneuvonkaytto.seuraava=lead(ajoneuvonkaytto)
         ) %>% 
  ungroup %>%
  left_join(., henkiloauto, by="combo")

# Katsastusaika käyttöönottopäivän perusteella
henkiloauto.historia$katsastus.Q <- katsastus.Q(henkiloauto.historia$date, henkiloauto.historia$kayttoonottopvm) 

# 
matkamittarilukemat <- filter(henkiloauto.historia, !is.na(km.diff) & km.diff > 0)  %>%
  group_by(., record.id, matkamittarilukema) %>% 
  summarise(matkamittari.date = min(date))

henkiloauto.historia <-
  left_join(henkiloauto.historia,
            select(matkamittarilukemat, 
                   record.id, 
                   matkamittari.date, 
                   matkamittarilukema),
            by=c("record.id","matkamittarilukema")) 


# Merkitään autot, joilla negatiivinen kilometrimäärä kahden kerran välissä 
# max.jdiff mittaa kuika paljon järjestysnumero on muuttunut. Suuri erotus tuntuu usein kielivän siitä että
# kyseessä on eri auto, joka on hajonnut dataan epäonnisesti niin että se voi tunnistua yhdeksi
# (näillä usein  paljon negat. ja suuria km-differenssejä)

outlier <- filter(ungroup(henkiloauto.historia), !is.na(record.id)) %>% 
  group_by(record.id) %>% 
  summarise(
    N=n(),
    negatiivinen = sum(km.diff< 0, na.rm=TRUE), 
    max.jdiff = max(abs(jarnro-lag(jarnro, order.by=jarnro)), na.rm=TRUE),
    max.jdiff = ifelse(max.jdiff < 0, 0, max.jdiff)
  ) %>%
  ungroup 
  

# Merkitään onko matkamittarilukeman ajankohta päätelty käyttöönottopvm vai matkamittarilukeman muuttumisen perusteella
henkiloauto.historia <- mutate(henkiloauto.historia, 
                               matkamittari.date.laatu = ifelse(is.na(matkamittari.date), "oletus", "km.diff"),
                               matkamittari.date = ifelse(is.na(matkamittari.date), katsastus.Q, matkamittari.date)) %>% 
  left_join(., select(outlier, record.id, negatiivinen, max.jdiff), by="record.id") 

# Lasketaan käyttövuodet ja poistetaan epäilyttävät autot
henkiloauto.historia <-
  mutate(henkiloauto.historia,
         record.id=ifelse(max.jdiff > 400000, NA, record.id),
         kayttovuodet.matkamittari=as.numeric(interval(ymd(kayttoonottopvm), ymd(matkamittari.date)) / years(1)),
         kayttovuodet=as.numeric(interval(ymd(kayttoonottopvm), ymd(date)) / years(1)),
         rekisterivuodet=as.numeric(interval(ymd(ensirekisterointipvm), ymd(date)) / years(1)),
         km.per.kayttovuosi = round(matkamittarilukema / kayttovuodet.matkamittari)
         )

# Uniikit matkamittarilukemat päivämäärineen
#
matkat <- henkiloauto.historia %>% 
  select(record.id, 
         matkamittarilukema, 
         matkamittari.date, 
         matkamittari.date.laatu, 
         data) %>% 
  filter(!is.na(record.id) & !is.na(matkamittarilukema))


## Edellisen vuoden aikana ajettu km-määrä estimaatti

# paritetaan kaikki otokset samasta autosta
matkat <-inner_join(matkat, transmute(matkat,record.id, 
                          matkamittarilukema.now = matkamittarilukema, 
                          data.now = data,
                          matkamittari.date.now = matkamittari.date,
                          matkamittari.date.laatu.now = matkamittari.date.laatu), by="record.id") %>% 
  filter(as.numeric(data.now) > as.numeric(data))

# lasketaan aikaerot matkamittarilukemien välillä; otetaan yli 6kk
matkat <- mutate(ungroup(matkat),
    t.diff=as.numeric(ymd(matkamittari.date.now)-ymd(matkamittari.date)),
    km.diff=matkamittarilukema.now-matkamittarilukema,
    t.sep=abs(t.diff-365),
    laatu=ifelse(matkamittari.date.laatu.now=="km.diff" & matkamittari.date.laatu=="km.diff",1,2)) %>% 
  filter(t.diff > 180 & km.diff > 0) 

# valitaan ensisijaisesti kahdesta matkamittarilukemien muuttumisen perusteella kohdennetu
# mahdollisimman lähellä vuotta oleva väli = edellisen vuoden aikana ajettu määrä
matkat <- group_by(ungroup(matkat), record.id, data.now) %>% 
  arrange(laatu, t.sep) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(km.diff.vuosi=round(km.diff/(t.diff/365)))

henkiloauto.historia<-left_join(henkiloauto.historia,
                                select(matkat, 
                                       record.id, 
                                       data=data.now, 
                                       km.diff.vuosi),
                                by=c("record.id","data"))

# Merkitään autot joilla ajettu keskimäärin yli 300000km tai 300000km "edellisen vuoden aikana"
henkiloauto.historia <- group_by(henkiloauto.historia, record.id) %>%
  mutate(suuri = sum(ifelse(nvl(km.diff.vuosi)> 300000 | nvl(km.per.kayttovuosi) > 300000, 1, 0))) %>% 
  ungroup

# Poistetaan km-määrät autoilta joilla on epäilyttävän suuria ajomääriä tai negat. lukemia
henkiloauto.historia <-
  mutate(henkiloauto.historia,
         matkamittarilukema = ifelse(suuri | negatiivinen, NA, matkamittarilukema),
         km.diff.vuosi=ifelse(suuri | negatiivinen, NA, km.diff.vuosi),
         km.per.kayttovuosi=ifelse(suuri | negatiivinen, NA, km.per.kayttovuosi)
         )

henkiloauto.historia.quality <-mutate(henkiloauto.historia, z=nvl(matkamittarilukema.orig)) %>% 
  group_by(record.id) %>% 
  summarise(katsastuksia=sum(min(z) == max(z)),
            km.vuodessa.300000=sum(suuri),
            km.negatiivinen=sum(negatiivinen))

henkiloauto.historia <- select(henkiloauto.historia, -suuri, -negatiivinen, -max.jdiff, -kayttoonottopvm) %>%
  mutate(data=as.character(data))


## Seuraavaksi autot joilla ei historiaa: lasketaan ne luvut jotka voidaan
## Huom: tarksitettava että tehdään samalla tavalla kuin edellä!!

H <- tbl(trafi.db,"henkiloauto_historia") %>% 
  filter(is.na(record.id)) %>% 
  collect(n=Inf) %>%
  fix.auto.historia %>% 
  left_join(., henkiloauto, by="combo")

H$katsastus.Q <- katsastus.Q(H$date, H$kayttoonottopvm) 

H<-mutate(H, 
         N = 1,
         katsastettu=0,
         matkamittari.date.laatu = "oletus",
         matkamittari.date = katsastus.Q,
         kayttovuodet.matkamittari=as.numeric(interval(ymd(kayttoonottopvm), ymd(matkamittari.date)) / years(1)),
         kayttovuodet=as.numeric(interval(ymd(kayttoonottopvm), ymd(date)) / years(1)),
         rekisterivuodet=as.numeric(interval(ymd(ensirekisterointipvm), ymd(date)) / years(1)),
         km.per.kayttovuosi = round(matkamittarilukema / kayttovuodet.matkamittari)
         )

henkiloauto.historia <- bind_rows(henkiloauto.historia,H) 
henkiloauto.historia <- mutate(henkiloauto.historia, data.seuraava=ifelse(is.na(data.seuraava), NA, data.seuraava))
         
save(file="historia.RData",henkiloauto.historia,henkiloauto.historia.quality)

if (db_has_table(trafi.db$con,"henkiloauto_historia_diff")) db_drop_table(trafi.db$con, "henkiloauto_historia_diff")
as.data.frame(henkiloauto.historia) %>% db_insert_into(trafi.db$con, "henkiloauto_historia_diff",.)

con <- DBI::dbConnect(RSQLite::SQLite(), paste(working.directory,"/trafi.db",sep=""))
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhhdata on henkiloauto_historia_diff(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhhjarnro on henkiloauto_historia_diff(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hhhrid on henkiloauto_historia_diff("record.id");')
DBI::dbDisconnect(con)
rm

save(file="historia.RData",henkiloauto.historia,henkiloauto.historia.quality)
