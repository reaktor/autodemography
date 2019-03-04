# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

## pseudotietue
# Autojen oletettu historia - erotukset matkamittarilukemien välillä; 
# Onko auton matkamittarilukema muuttunut = "katsastettu"


Historia_ <- tbl(trafi.db$con, "autohistoria") %>% filter(substr(record_id, 13, 13) == "1") %>% collect


Historia <- Historia_

Historia<-  Historia %>%
  #select(combination_id, record_id, date, data, matkamittarilukema) %>% 
  #collect %>%
  mutate(km = fix.matkamittarilukema(matkamittarilukema), 
         km = ifelse(data == "5.03", NA, km)) %>%
  left_join(., tbl(trafi.db$con, "autotiedot") %>% 
              select(combination_id, kayttoonottopvm) %>% 
              collect, 
            by="combination_id") %>%
  filter(!is.na(record_id)) %>% 
  group_by(record_id) %>% 
  arrange(data) %>% 
  mutate(km_diff = km - lag(km),
         katsastettu = ifelse(km != lag(km), T, F), 
         max.jdiff = max(abs(jarnro - lag(jarnro, order.by = jarnro)), na.rm=TRUE),
         max.jdiff = ifelse(max.jdiff < 0, 0, max.jdiff),
         diffs_neg = any(km_diff < 0),
         diffs_large = any(km_diff > 400000)) %>% 
  ungroup

# Katsastuskvartaali käyttöönottopäivän perusteella
Historia$default_katsastus_kvartaali = map.default.katsastus.kvartaali(Historia$date, Historia$kayttoonottopvm)
         
Historia <- mutate(negatiivinen = km.diff<0)
matkamittarilukemat <- 
  filter(henkiloauto.historia, !is.na(km.diff) & km.diff > 0) %>%
  group_by(., record.id, matkamittarilukema) %>% 
  summarise(matkamittari.date = min(date))

henkiloauto.historia <-
  left_join(henkiloauto.historia,
            select(matkamittarilukemat, 
                   record.id, 
                   matkamittari.date, 
                   matkamittarilukema),
            by=c("record.id", "matkamittarilukema")) 

# Merkitään autot, joilla negatiivinen kilometrimäärä kahden kerran välissä 
# max.jdiff mittaa kuika paljon järjestysnumero kannassa on muuttunut. Suuri erotus tuntuu usein kielivän 
# siitä että kyseessä on eri auto, joka on hajonnut dataan epäonnisesti niin, että se voi tunnistua yhdeksi
# (näillä usein paljon negatiivisia ja/tai suuria km-differenssejä)

if (FALSE) {
outlier <- filter(ungroup(henkiloauto.historia), !is.na(record.id)) %>% 
  group_by(record.id) %>% 
  summarise(
    N=n(),
    negatiivinen = sum(km.diff < 0, na.rm = TRUE), 
    max.jdiff = max(abs(jarnro - lag(jarnro, order.by = jarnro)), na.rm=TRUE),
    max.jdiff = ifelse(max.jdiff < 0, 0, max.jdiff)
  ) %>%
  ungroup 
}

#Jos luotetaan edelliseen Poistetaan epäilyttävät autot, maaginen luku 400000 päätetty eo kohdan perusteella
if (FALSE) henkiloauto.historia <- mutate(henkiloauto.historia, record.id=ifelse(max.jdiff > 400000, NA, record.id))

# Merkitään onko matkamittarilukeman ajankohta päätelty käyttöönottopvm vai matkamittarilukeman muuttumisen perusteella
henkiloauto.historia <- mutate(henkiloauto.historia, 
                               matkamittari.date.laatu = ifelse(is.na(matkamittari.date), "default", "km.diff"),
                               matkamittari.date = ifelse(is.na(matkamittari.date), katsastus.Q, matkamittari.date)) %>% 
  left_join(., select(outlier, record.id, negatiivinen, max.jdiff), by="record.id") 

# Lasketaan käyttövuodet ja poistetaan epäilyttävät autot: 
henkiloauto.historia <-
  mutate(henkiloauto.historia,
         record.id=ifelse(max.jdiff > 400000, NA, record.id),
         kayttovuodet.matkamittari = as.numeric(interval(ymd(kayttoonottopvm), ymd(matkamittari.date)) / years(1)),
         kayttovuodet = as.numeric(interval(ymd(kayttoonottopvm), ymd(date)) / years(1)),
         rekisterivuodet = as.numeric(interval(ymd(ensirekisterointipvm), ymd(date)) / years(1)),
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
    t.diff = as.numeric(ymd(matkamittari.date.now) - ymd(matkamittari.date)),
    km.diff = matkamittarilukema.now-matkamittarilukema,
    t.sep = abs(t.diff-365),
    laatu = ifelse(matkamittari.date.laatu.now=="km.diff" & matkamittari.date.laatu=="km.diff", 1, 2)) %>% 
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

# Merkitään autot joilla ajettu keskimäärin yli 400 000km tai 400 000km "edellisen vuoden aikana"
henkiloauto.historia <- group_by(henkiloauto.historia, record.id) %>%
  mutate(suuri = sum(ifelse(nvl(km.diff.vuosi) > 400000 | nvl(km.per.kayttovuosi) > 400000, 1, 0))) %>% 
  ungroup

# Poistetaan km-määrät autoilta joilla on epäilyttävän suuria ajomääriä tai negat. lukemia
henkiloauto.historia <-
  mutate(henkiloauto.historia,
         matkamittarilukema = ifelse(suuri | negatiivinen, NA, matkamittarilukema),
         km.diff.vuosi = ifelse(suuri | negatiivinen, NA, km.diff.vuosi),
         km.per.kayttovuosi = ifelse(suuri | negatiivinen, NA, km.per.kayttovuosi)
         )

henkiloauto.historia.quality <- 
  mutate(henkiloauto.historia, 
         z = nvl(matkamittarilukema.orig)) %>% 
  group_by(record.id) %>% 
  summarise(katsastuksia = sum(min(z) == max(z)),
            km.vuodessa.300000 = sum(suuri),
            km.negatiivinen = sum(negatiivinen))

henkiloauto.historia <- 
  select(henkiloauto.historia, 
         -suuri, 
         -negatiivinen, 
         -max.jdiff, 
         -kayttoonottopvm) %>%
  mutate(data = as.character(data))


## Seuraavaksi autot joilla ei historiaa: lasketaan ne luvut jotka voidaan
## Huom: tarksitettava että tehdään samalla tavalla kuin edellä!!

H <- tbl(trafi.db, "henkiloauto_historia") %>% 
  filter(is.na(record.id)) %>% 
  collect(n=Inf) %>%
  left_join(., henkiloauto, by="combo")

H$katsastus.Q <- katsastus.Q(H$date, H$kayttoonottopvm) 

H <- mutate(
  H, 
  N = 1,
  katsastettu = 0,
  matkamittari.date.laatu = "default",
  matkamittari.date = katsastus.Q,
  kayttovuodet.matkamittari = as.numeric(interval(ymd(kayttoonottopvm), ymd(matkamittari.date)) / years(1)),
  kayttovuodet = as.numeric(interval(ymd(kayttoonottopvm), ymd(date)) / years(1)),
  rekisterivuodet = as.numeric(interval(ymd(ensirekisterointipvm), ymd(date)) / years(1)),
  km.per.kayttovuosi = round(matkamittarilukema / kayttovuodet.matkamittari)
)

henkiloauto.historia <- bind_rows(henkiloauto.historia,H) 
henkiloauto.historia <- select(henkiloauto.historia, -kunta,-alue,-N)         

if (db_has_table(trafi.db$con,"henkiloauto_historia_diff")) db_drop_table(trafi.db$con, "henkiloauto_historia_diff")
as.data.frame(henkiloauto.historia) %>% db_insert_into(trafi.db$con, "henkiloauto_historia_diff",.)

con <- DBI::dbConnect(RSQLite::SQLite(), full.path("trafi.db"), sep="")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhhdata on henkiloauto_historia_diff(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhhjarnro on henkiloauto_historia_diff(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hhhrid on henkiloauto_historia_diff("record.id");')
DBI::dbDisconnect(con)