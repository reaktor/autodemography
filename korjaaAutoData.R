# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

## Lyhennellään nimiä

# teho: on muutama tavallinen auto joissa > 1000 kW teho, rajoitetaan 520 kW (Lamborghini) 
# Rajoitetaan henkilöauton istumapaikat 9:ään, koska laki)
# varmistetaan että co päästö ei ole yli 1000
# rajoitetaan henkilöauton painoa, pituutta, massaa, iskutilavuutta ja tehoa konservatiivisesti
# Matkamittarilukemat rajoitetaan alle miljoonaan (koska 999)
# kayttoonottovuosi vähintään 1900
# korjataan, nimeä, mallia erillisillä tiedostoilla
# sahkohybridejä aikaisintaan vuonna 1997

## yksinkertaistetaan polttoainevalikoimaa 
## pudotetaan koritiedoista pois alle 500 kpl korit
## Korjataan koodistosta puuttuvia ajoneuvoryhmiä
# korjataan muutama matkailuauto (~20) korin mukaan
# lyhennetään ja yhdistetään muutamia koriryhmia

# source("~/Projects/autodemography/initTrafi.R")
source("initTrafi.R")
source("korjaa.R")

# Ajetaan merkkien/mallien korjaustiedosto!! luodaan skriptillä createMallitMerkitkorjaus.R
# (Voidaan editoida käsin)

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

henkiloautot <-tbl(trafi.db,"henkiloautot") %>% 
  filter(data=="4.7") %>% 
  collect(n=Inf) %>% 
  fix.merkit.mallit %>% 
  est.kori 

autot <- 
  transmute(ungroup(henkiloautot),
         ajoneuvoluokka,
         ensirekisterointipvm,
         ensirekVuosi,
         ryhma=ifelse(is.na(ajoneuvoryhma) & 
                        korityyppi=="Matkailuauto (SA)","Matkailuauto", ajoneuvoryhma),
         ryhma=ifelse(is.na(ryhma),
                      mapvalues(ajoneuvoluokka,c("Henkilöauto","Henkilöauto (maasto)"),
                                                     c("Henkilöauto","Maastohenkilöauto")),
                      ryhma),
         ryhma=mapvalues(ryhma,c(NA,"Henkilöauto",
                                 "Maastohenkilöauto","Maastoauto",
                                 "Matkailuauto","Museoajoneuvo"),
                         c("Henkilö","Henkilö",
                           "Maasto","Maasto",
                           "Matkailu","Museo")),
         ryhma=ifelse(ryhma %in% c("Henkilö","Maasto","Matkailu","Museo"), ryhma, "Muu"),
         ajoneuvoryhma=mapvalues(ajoneuvoryhma,c(NA,""),c("Henkilöauto","Henkilöauto")),
         ajoneuvonkaytto,
         #variantti,
         #versio,
         kayttoonottopvm,
         kayttoonottoVuosi=ifelse(kayttoonottoVuosi<1900,NA,kayttoonottoVuosi),
         kayttoonotto.pvm.imputoitu,
         vari,
         #ovienLukumaara=limith(ovienLukumaara,8,NA),
         kori=map.korityyppi(korityyppi),
         korityyppi,
         #ohjaamotyyppi,
         istumapaikkojenLkm=limith(istumapaikkojenLkm,9,NA),
         omamassa=limitl(omamassa,700,NA),
         omamassa=limith(omamassa,5000,NA),
         #teknSuurSallKokmassa,
         #tieliikSuurSallKokmassa,
         ajonKokPituus=limitl(ajonKokPituus,100,NA),
         ajonKokPituus=limith(ajonKokPituus,13500,NA),
         ajonLeveys=limith(ajonLeveys,2600,NA),
         ajonLeveys=limitl(ajonLeveys,50,NA),
         #ajonKorkeus,
         #kayttovoima,
         iskutilavuus=limitl(iskutilavuus,100,NA),
         iskutilavuus=limith(iskutilavuus,15000,NA),
         suurinNettoteho=limith(suurinNettoteho,520,NA),
         suurinNettoteho=round(limitl(suurinNettoteho,1,NA)),
         sylintereidenLkm=limitl(sylintereidenLkm,1,NA),
         sylintereidenLkm=limith(sylintereidenLkm,16,NA),
         #ahdin=mapvalues(ahdin,c("","true","false"),c(NA,TRUE,FALSE)),
         sahkohybridi=mapvalues(sahkohybridi,c("","true","false"),c(NA,TRUE,FALSE)),
         sahkohybridi=ifelse(kayttoonottoVuosi < 1997,FALSE,sahkohybridi),
         merkkiSelvakielinen,
         mallimerkinta,
         vaihteisto,
         #vaihteidenLkm,
         kaupallinenNimi,
         #voimanvalJaTehostamistapa,
         #tyyppihyvaksyntanro,
         polttoaine=map.polttoaine(kayttovoima),
         #yksittaisKayttovoima,
         kuntano=map.kuntano(kunta),
         kuntanimi=map.kunta(kunta),
         Co2=limith(Co2,1000,NA),
         Co2=limitl(Co2,1,NA),
         matkamittarilukema=limith(matkamittarilukema,999998,NA),
         matkamittarilukema=limitl(matkamittarilukema,1,NA),
         pono.3=str_pad(alue, 3, side="left", pad="0"),
         #alue,
         #valmistenumero2,
         jarnro,
         data,
         date=max.date,
         id,
         historia,
         N.id)





