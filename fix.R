# Ajoneuvoryhmälle yksinkertaisempi ryhmittely ja korjaa muutama epäloogisuus

fix.autoryhma <- function(ajoneuvoluokka, ajoneuvoryhma, korityyppi) {

  ryhma <- ifelse(is.na(ajoneuvoryhma) & grepl("Matkailuauto", korityyppi, ignore.case = TRUE), "Matkailuauto", ajoneuvoryhma)
  
  ryhma <- ifelse(is.na(ryhma),
                 plyr::mapvalues(ajoneuvoluokka, 
                                 c("Henkilöauto", "Henkilöauto (maasto)"),
                                 c("Henkilöauto", "Maastohenkilöauto"), warn_missing = FALSE), ryhma)

  ryhma <- plyr::mapvalues(
    ryhma,
    c(
      NA,
      "Henkilöauto",
      "Maastohenkilöauto",
      "Maastoauto",
      "Matkailuauto",
      "Museoajoneuvo"
    ),
    c("Henkilö", "Henkilö",
      "Maasto", "Maasto",
      "Matkailu", "Museo"), warn_missing = FALSE
  )
  
  ryhma <- ifelse(
    ryhma %in% c("Henkilö", "Maasto", "Matkailu", "Museo"),
    ryhma,
    "Muu"
  )
  return(ryhma)
}

fix.kayttoonottoVuosi <- function(kayttoonottoVuosi) replace(kayttoonottoVuosi, kayttoonottoVuosi < 1900, NA)

fix.ovienLukumaara <- function(ovienLukumaara) replace(ovienLukumaara, ovienLukumaara < 0 | ovienLukumaara > 8, NA)

fix.istumapaikkojenLkm <- function(istumapaikkojenLkm) replace(istumapaikkojenLkm, istumapaikkojenLkm < 0 | istumapaikkojenLkm > 9, NA)

fix.omamassa <- function(omamassa, teknSuurSallKokmassa, tieliikSuurSallKokmassa) { 
  omamassa <- replace(omamassa, omamassa < 100 | omamassa > 20000, NA)
  omamassa <- ifelse(omamassa > nvl(teknSuurSallKokmassa, tieliikSuurSallKokmassa), NA, omamassa)
}

fix.ajonKokPituus <- function(ajonKokPituus) replace(ajonKokPituus < 100 | ajonKokPituus > 13500, NA)

fix.ajonLeveys <- function(ajonLeveys) replace(ajonLeveys, ajonLeveys < 50 | ajonLeveys > 2600, NA)

fix.iskutilavuus <- function(iskutilavuus) replace(iskutilavuus, iskutilavuus < 50 | iskutilavuus > 15000, NA)

fix.suurinNettoteho <- function(suurinNettoteho) replace(suurinNettoteho, suurinNettoteho > 599 | suurinNettoteho < 1, NA)

fix.sylintereidenLkm <- function(sylintereidenLkm) replace(sylintereidenLkm, sylintereidenLkm < 1 | sylintereidenLkm > 16, NA) 

fix.ahdin <- function(ahdin) plyr::mapvalues(ahdin, c("", "true", "false"), c(NA, TRUE, FALSE))

fix.matkamittarilukema <- function(matkamittarilukema) replace(matkamittarilukema, matkamittarilukema > 3e6 | matkamittarilukema <= 0, NA)
fix.sahkohybridi <- function(sahkohybridi) {
  sahkohybridi <- plyr::mapvalues(sahkohybridi, c("", "true", "false"), c(NA, TRUE, FALSE)) 
    sahkohybridi <- ifelse(kayttoonottoVuosi <= 1996, FALSE, sahkohybridi)
    return(sahkohybridi)}

fix.korityyppi <- function(korityyppi) map.korityyppi(korityyppi)

fix.kayttovoima <- function(kayttovoima) {
  kayttovoima <- map.polttoaine(kayttovoima) 
  kayttovoima <- ifelse(polttoaine == "Sähkö" & !is.na(Co2) & Co2 > 0, NA, kayttovoima)
  return(kayttovoima)
  }

# Kuntanimien ja numeroiden korjaaminen
# Rajoita kilometrimääät alle kolmeen miljoonaan ja vaadi vähintään 1 (nolla voi olla puuttuva?) 
# Kolmen numeron postinumero

fix.autohistoria <- function(henkiloauto_historia)
  mutate(
    henkiloauto_historia,
    kuntano = map.kuntano(kunta),
    kuntanimi = map.kunta(kunta),
    matkamittarilukema.orig = matkamittarilukema,
    matkamittarilukema = replace(matkamittarilukema, 
                                 matkamittarilukema < 1 | matkamittarilukema >= 3000000, NA),
    pono.3 = str_pad(alue, 3, side = "left", pad = "0")
  ) %>%
  return

fix.merkkimalli <-
  function(henkiloautot,
           korjaustiedosto = full.path("mallitmerkkikorjaus.csv"))
  {
    korjaus <-
      read.csv(
        korjaustiedosto,
        quote = "",
        fileEncoding = "UTF-8",
        sep = "\t",
        stringsAsFactors = FALSE
      ) %>%
      mutate_if(is.character, iconv, to = "UTF-8") %>%
      unique %>%
      rename(
        mallimerkinta = malli.orig,
        merkkiSelvakielinen = merkki.orig,
        kaupallinenNimi = k.malli.orig,
        malli = k.malli
      )
    
    # Lisätään myös "lyhyt" automalli BMW:lle ja Mersulle
    
    left_join(
      henkiloautot,
      select(korjaus, -n),
      by = c("merkkiSelvakielinen", "kaupallinenNimi", "mallimerkinta")
    ) %>%
      mutate(
        l.malli = malli,
        l.malli = ifelse(merkki == "BMW", substring(l.malli, 1, 1), l.malli),
        l.malli = ifelse(merkki == "BMW" &
                           grepl("COMPACT", malli), "COMPACT", l.malli),
        l.malli = ifelse(
          merkki == "MERCEDES-BENZ",
          str_match(l.malli, "^[0-9]{1,3}|^[A-Za-z]+"),
          l.malli
        ),
        merkki.l.malli = paste(ifelse(is.na(merkki), "", merkki),
                               ifelse(is.na(l.malli), "", l.malli)),
        merkki.l.malli = ifelse(merkki.l.malli == "", NA, merkki.l.malli)
      ) %>%
      select(-merkkiSelvakielinen) %>%
      return
  }

fix.kori <-
  function(henkiloauto, korjaustiedosto = here::here("Data", "korikorjaus.csv"))
  {
    ### Täydennetään puuttuvat koritiedot:
    ### fix.merkit.mallit on ajettava ensin, samoin fix.attribuutit (kori oltava)
    
    korjaus <- read.csv(
      korjaustiedosto,
      quote = "",
      fileEncoding = "UTF-8",
      sep = "\t",
      stringsAsFactors = FALSE
    ) %>%
      mutate_if(is.character, iconv, to = "UTF-8")
    
    ## jos puuluokittimen todennäköisyys on alle 0.4 tai suora lookupin p on korkeampi kuin
    ## puuluokittimen p, käytetään suoraa lookuppia (vaikka sen frekvenssi olisi matala)
    ## korityyppi: alkuperäinen (korityyppi)
    
    kori <-
      left_join(
        transmute(henkiloauto, 
                  combination.id, 
                  mallimerkinta, 
                  kaupallinenNimi, 
                  merkkiSelvakielinen, 
                  kori.orig = map.korityyppi(korityyppi)),
        select(
          korjaus,
          kaupallinenNimi,
          merkkiSelvakielinen,
          mallimerkinta,
          kori.nn,
          kori.tree,
          p.kori.nn,
          p.kori.tree
        ),
        by = c("merkkiSelvakielinen", "kaupallinenNimi", "mallimerkinta")
      ) %>%
      mutate(
        p = ifelse(is.na(p.kori.nn), 0, p.kori.nn),
        kori.est = ifelse(p.kori.tree > p, kori.tree, kori.nn),
        kori.est = ifelse(p.kori.tree > p &
                            p.kori.tree < 0.4, NA, kori.tree),
        kori = ifelse(is.na(kori.orig), kori.est, kori.orig)
      )
    
      return(select(kori, combination.id, kori.orig, kori.est, kori))
  }

fix.co2 <-
  function(henkiloautot,
           korjaustiedosto.nn = here::here("Data","co2.stat.csv"),
           co2.malli = here::here("Data","malli.co2.rds"))
  {
    ### Täydennetään puuttuvat Co2 -tiedot

    co2 <- read.csv(
      korjaustiedosto.nn,
      quote = "",
      fileEncoding = "UTF-8",
      sep = "\t",
      stringsAsFactors = FALSE
    ) %>%
      mutate_if(is.character, iconv, to = "UTF-8")

    # Jos Co2>0, polttoaine ei voi olla sähkö, jos polttoaine = sähkö ja co2 puutuu = 0
    # Liitetään suoraan ne automallit+tilavuus joiden co2 2 yksikän sisällä, jos co2 puuttuu
    
    henkiloautot <- mutate(
      henkiloautot,
      Co2.orig = Co2,
      Co2 = ifelse(polttoaine == "Sähkö", 0, Co2),
      Co2 = ifelse((polttoaine != "Sähkö") &
                     (Co2 < 15), NA, Co2),
      Co2 = ifelse(Co2 > 700, NA, Co2)
    )
    
    henkiloautot <-
      left_join(
        henkiloautot,
        filter(co2, co2.max - co2.min <= 2) %>%
          select(co2.nn, merkki, mallimerkinta, iskutilavuus),
        by = c("merkki", "mallimerkinta", "iskutilavuus")
      ) %>%
      mutate(Co2 = ifelse(is.na(Co2), co2.nn, Co2)) %>% 
      select(-co2.nn)
    
    henkiloautot$Co2.modelled <- predict(
      malli.co2,
      select(
        henkiloautot,
        iskutilavuus,
        suurinNettoteho,
        omamassa,
        kayttoonottoVuosi,
        polttoaine,
        kori
      ) %>% data.matrix
    )
    
    henkiloautot <-
      mutate(henkiloautot, Co2 = ifelse(is.na(Co2), Co2.modelled, Co2))
    
    return(henkiloautot)
  }

# Laske edellisen oletetun katsastuspäivämäärän jälkeinen kvartaalin viimeinen päivä
