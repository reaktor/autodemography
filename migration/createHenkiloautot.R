# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("initTrafi.R")

# Valitaan varsinaiset henkilöautot ja uudelleenkoodataan muuttujia

# Poistetaan taulut jos on 
if (db_has_table(trafi.db$con, "autot")) 
  db_drop_table(trafi.db$con, "autot") 

if (db_has_table(trafi.db$con,"autohistoria")) 
  db_drop_table(trafi.db$con, "autohistoria")

if (db_has_table(trafi.db$con,"autotiedot")) 
  db_drop_table(trafi.db$con, "autotiedot")

# Partitioidaan vuoden mukaan 
V <- tbl(trafi.db, "ajoneuvot") %>% 
  filter(ajoneuvoluokka %in% c("M1", "M1G")) %>% 
  count(kayttoonottoVuosi) %>% 
  collect 

for (vuosi in V$kayttoonottoVuosi) {
# vuosi=2015
  
  if (is.na(vuosi))   
    autot.orig <- tbl(trafi.db, "ajoneuvot") %>% 
      filter(is.na(kayttoonottoVuosi) & ajoneuvoluokka %in% c("M1", "M1G")) %>%
      collect(n = Inf) else
        autot.orig <- tbl(trafi.db, "ajoneuvot") %>% 
          filter(kayttoonottoVuosi == vuosi & ajoneuvoluokka %in% c("M1", "M1G")) %>%
          collect(n = Inf)
      
      print(vuosi)
      
      henkiloautot <- mutate(autot.orig,
        kayttoonotto = as.character(as.Date(as.character(kayttoonottopvm), "%Y%m%d")),
        kayttoonotto = ifelse(is.na(kayttoonottoVuosi), NA, kayttoonotto),
        kayttoonottopvm_orig = kayttoonotto,
        kayttoonottopvm = ifelse(
          is.na(kayttoonottopvm_orig),
          paste(as.character(kayttoonottoVuosi), "-06-30", sep = ""),
          kayttoonottopvm_orig
        ),
        ensirekisterointipvm = as.character(ensirekisterointipvm),
        alue = str_pad(alue, 3, side = "left", pad = "0"),
        kunta_orig = kunta,
        kunta = str_pad(kunta, 3, side = "left", pad = "0"),
        kunta = ifelse(is.na(kunta), "999", kunta),
        kunta = map.kuntano(kunta),
        vaihteisto = map.trafi("vaihteisto", vaihteisto),
        ohjaamotyyppi = map.trafi("ohjaamo", ohjaamotyyppi),
        korityyppi = map.trafi("kori", korityyppi),
        vari = map.trafi("vari", vari),
        ajoneuvoluokka = map.trafi("luokka", ajoneuvoluokka),
        ajoneuvoryhma = map.trafi("ryhma", ajoneuvoryhma),
        ajoneuvonkaytto = map.trafi("kaytto", ajoneuvonkaytto),
        yksittaisKayttovoima = map.trafi("polttoaine", yksittaisKayttovoima),
        kayttovoima = map.trafi("polttoaine", kayttovoima),
        voimanvalJaTehostamistapa = map.trafi("valitys", voimanvalJaTehostamistapa)
      ) %>%
        select(-kayttoonotto) %>%
        rename(date = max.date)
      
      henkiloautot <- 
        mutate(henkiloautot, 
               ensirekisterointipvm = ifelse(ensirekisterointipvm == "", NA, ensirekisterointipvm),
               ensirekVuosi = ifelse(ensirekisterointipvm == "", NA, ensirekVuosi))
      
      # muuttujat jotka voivat selvästi muuttua otatetaan pois 
      # jos teknisiä tietoa muutetaan (voihan väri tai moottori vaihtua...), auto "menee teknisesti rikki"
      # toivotaan että tätä sattuuu aika harvoin (ahdin ja sahkohybridi pitäisi ehkä ottaa pois, jos ne ovat johdettuja muuttujia?)
      #
      # combination.id: annetaan id kullekin attribuuttien uniikille yhdistelmille
      
      combo_attribute <- names(select(henkiloautot,
                                      -kunta,
                                      -alue,
                                      -kunta_orig,
                                      -date,
                                      -matkamittarilukema,
                                      -ajoneuvonkaytto,
                                      -data, 
                                      -jarnro))
      
      # combination.id on id uniikille attribuuttiyhdistelmälle. Jos näitä on täsmälleen
      # yksi yhtä data id:tä (4.02, 4.03, ...) kohti, lienee kyseessä aina sama auto. Näille annetaan pseudonyymi "record.id"
      # vuosi laitetaan eteen jotta id:t olisivat uniikkeja 
      
      henkiloautot$combination_id <- group_indices_(henkiloautot, .dots = combo_attribute) 
      
      if (is.na(vuosi)) yyyy <- "0000" else yyyy <- as.character(vuosi)
      
      henkiloautot$combination_id <- 
        paste0("Y", yyyy, "C", str_pad(henkiloautot$combination_id, 6, side = "left", pad = "0"))
      
      # historia -lippu TRUE kun combination.id on em. tavalla uniikki (eli kyseessä sama auto)
      henkiloautot <- henkiloautot %>%
        group_by(combination_id) %>%
        mutate(historia = ifelse(length(unique(data)) == n(), T, F)) %>%
        ungroup %>%
        group_by(combination_id, historia) %>% 
        mutate(N_combinations = n(), 
               N_unique_data_date_pairs = length(unique(paste0(data, "|", alue))),
               N_uniq_data = length(unique(data)),
               N_uniq_alue = length(unique(alue))) %>%
        ungroup %>%
        mutate(record_id = ifelse(historia, paste0("UNIQ", combination_id), NA),
               record_id = ifelse(!historia & (N_combinations == N_unique_data_date_pairs) & (N_combinations == N_uniq_data * N_uniq_alue) & !(alue %in% c("000","999")),
                                  paste0("A", alue, combination_id), 
                                  record_id)
               #record_id = ifelse(!historia & (N_combinations == N_unique_data_date_pairs) & !(N_combinations == N_uniq_data * N_uniq_alue) & !(alue %in% c("000","999")),
               #                  paste0("B", alue, combination_id), 
               #                  record_id)
        ) %>%
        ungroup %>%
        group_by(record_id, alue, data) %>%
        mutate(N_data_alue = n()) %>% 
        ungroup
      
      henkiloautot <- 
      
      # otataan vielä ne joilla on uniikki alue ja mukana kaikissa datoissa vain kerran
      
      #historiaton <- filter(henkiloautot, !historia) %>% arrange(combination_id)
      #henkiloautot <- filter(henkiloautot, historia)
      
      #N.data <- length(unique(henkiloautot$data))
      
      # alue mukaan kombinaation muodostaviin attribuutteihin
      #historiaton <- mutate(historiaton, id = paste0("A", alue, combination_id)) %>%
      #  group_by(id) %>% 
      #  mutate(N_uniq_data = length(unique(data)), 
      #         N_data = n()) %>% 
      #  ungroup %>% 
      #  mutate(uniikki.alue = ifelse(N_uniq_id == N.temp.id, T, F),
      #         record.id = ifelse(uniikki.alue, 
      #                            paste("A", str_pad(alue, 3, side = "left", pad = 0), combination_id, sep = ""), 
      #                            record_id),
      #         historia = ifelse(!historia & uniikki.alue, T, historia)
      #  )
      #
      #  
      #henkiloautot <- bind_rows(henkiloautot, historiaton) %>% 
      #  select(-temp.id, -historia, -uniikki.alue, -N.temp.id, -N.uniq.temp.id)
      #
      # Tätä ei tarvita enää, jos tietokantaan luotavissa datanimissä on pad 0 
      #henkiloautot <- mutate(henkiloautot, 
      #                       data = paste0(str_sub(data, 1, 2), str_pad(str_sub(data, 3, 5), 2, "left", pad = "0")))
      
      henkiloautot <- henkiloautot[, c("data", "jarnro", "date", "combination_id", "record_id", 
                     "kayttoonottoVuosi", "kayttoonottopvm", "kayttoonottopvm_orig",
                     "ensirekVuosi", "ensirekisterointipvm", 
                     "alue", "kunta", "kunta_orig", 
                     "ajoneuvoluokka", "ajoneuvonkaytto", "ajoneuvoryhma", "korityyppi", 
                     "merkkiSelvakielinen", "kaupallinenNimi", "mallimerkinta", "vari",
                     "kayttovoima", "yksittaisKayttovoima", "sahkohybridi", 
                     "ajonKokPituus","ajonKorkeus","ajonLeveys", 
                     "omamassa", "teknSuurSallKokmassa","tieliikSuurSallKokmassa", 
                     "ahdin", "Co2", "iskutilavuus", "suurinNettoteho","sylintereidenLkm",
                     "matkamittarilukema",
                     "vaihteidenLkm","vaihteisto", "voimanvalJaTehostamistapa",
                     "ohjaamotyyppi", "istumapaikkojenLkm", "ovienLukumaara", 
                     "tyyppihyvaksyntanro", "valmistenumero2", "variantti", "versio", "N_combinations")]
      
      
      
      henkiloauto.historia <- transmute(henkiloautot,
                                     record_id,
                                     combination_id, 
                                     data, 
                                     jarnro, 
                                     kunta_orig,
                                     kunta,
                                     alue,
                                     ajoneuvonkaytto, 
                                     matkamittarilukema,
                                     date) %>%
        ungroup
      
      # Uniikit combot otetaan yksi (vanhimmasta datasta) jokaiselle record.id:lle 
      
      henkiloauto.uniikit <- 
        group_by(henkiloautot, combination_id) %>% 
        arrange(data) %>% 
        slice(1) %>% 
        ungroup %>% 
        select( 
          -record_id, 
          -date, 
          -data, 
          -alue, 
          -jarnro, 
          -ajoneuvonkaytto, 
          -matkamittarilukema,
          -kunta_orig,
          -kunta)
      
      # record_id on nyt "pseudoid" - jos se puuttuu ei autoa voida kombinaation avulla identifioida 
      # samaksi eri dataseteissä
      
      as.data.frame(henkiloautot) %>% 
        db_insert_into(trafi.db$con, "autot", .)
      
      as.data.frame(henkiloauto.historia) %>% 
        db_insert_into(trafi.db$con, "autohistoria",.)
      
      as.data.frame(henkiloauto.uniikit) %>% 
        db_insert_into(trafi.db$con, "autotiedot",.)
      
}

con <- DBI::dbConnect(RSQLite::SQLite(), full.path("trafi.db"), sep="")

tmp <- DBI::dbSendStatement(con, "CREATE INDEX hkaytto on autot(ajoneuvonkaytto);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hdata on autot(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvuosi on autot(kayttoonottoVuosi);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hjarnro on autot(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hrecord on autot("record.id");')
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhdata on autohistoria(data);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hhjarnro on autohistoria(jarnro);")
tmp <- DBI::dbSendStatement(con, 'CREATE INDEX hhrid on autohistoria("record.id");')
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvryhma on autotiedot(ajoneuvoryhma);")
tmp <- DBI::dbSendStatement(con, "CREATE INDEX hvvuosi on autotiedot(kayttoonottoVuosi);")

DBI::dbDisconnect(con)

