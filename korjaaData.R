# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

trafi.db<- src_sqlite("trafi.db")

# koodit; alunperin Excelissä; manuaalisesti csv:ksi; luetaessa tässä tapauksessa 
# file-encoding = MAC, mutta muuten jotain muuta

# hlöautoille puuttuvia arvoja ()
# lyhennellaan kenttien nimia ja jatetaan osa pois

autot<-tbl(trafi.db,"trafi") %>% 
  filter(ajoneuvoluokka %in% c("Henkilöauto","Henkilöauto.m")) %>% 
  select(
    luokka=ajoneuvoluokka,   
    kaytto=ajoneuvonkaytto,
    ensirekisterointipvm, 
    ryhma=ajoneuvoryhma,             
    kayttoonottoVuosi,
    kayttoonottopvm,
    kayttoonotto.pvm.imputoitu,
    ensirekisterointipvm,
    ensirekVuosi,
    vari,                      
    ovet=ovienLukumaara,            
    kori=korityyppi,               
    ohjaamo=ohjaamotyyppi,             
    paikat=istumapaikkojenLkm,        
    omamassa,                  
    kokmassa=teknSuurSallKokmassa,     
    -tieliikSuurSallKokmassa,   
    pituus=ajonKokPituus,             
    leveys=ajonLeveys,                
    -ajonKorkeus,              
    kayttovoima=yksittaisKayttovoima,
    sahkohybridi,
    iskutilavuus,              
    kW=suurinNettoteho,           
    -sylintereidenLkm,         
    -ahdin,                     
    merkki=merkkiSelvakielinen,
    malli=mallimerkinta,  
    -vaihteisto,               
    -vaihteidenLkm,             
    k.malli=kaupallinenNimi,          
    -voimanvalJaTehostamistapa, 
    -tyyppihyvaksyntanro,      
    kunta, 
    pono.3=alue,
    Co2,
    matkamittarilukema,
    id=jarnro,
    data,
    date) %>% 
  collect(n=Inf) 

names(autot)<-paste(names(autot),".orig",sep="")

# Korjataan ensin muutamia "" ja "x" -> NA koodauksia; ei tehdä näille uutta muuttujaa

autot <- 
  mutate(autot, 
         ensirekisterointipvm.orig=ifelse(ensirekisterointipvm.orig=="",NA,ensirekisterointipvm.orig),
         ensirekVuosi.orig=ifelse(ensirekisterointipvm.orig=="",NA,ensirekVuosi.orig),
         vari.orig=ifelse(vari.orig=="x",NA, vari.orig),
         kaytto.orig=ifelse(kaytto.orig=="x",NA, kaytto.orig))
         
# teho: on muutama tavallinen auto joissa > 1000 kW teho, rajoitetaan 520 kW (Lamborghini) 
# Rajoitetaan henkilöauton istumapaikat 9:ään, koska laki)
# varmistetaan että co päästö ei ole yli 1000
# rajoitetaan henkilöauton painoa, pituutta, massaa, iskutilavuutta ja tehoa konservatiivisesti
# Matkamittarilukemat rajoitetaan alle miljoonaan (koska 999)
# kayttoonottovuosi vähintään 1900
# korjataan, nimeä, mallia erillisillä tiedostoilla
# sahkohybridejä aikaisintaan vuonna 1997

autot <- 
  mutate(autot, 
    kayttoonottoVuosi=ifelse(kayttoonottoVuosi.orig<1900,NA,kayttoonottoVuosi.orig),
    malli=toupper(malli.orig),
    k.malli=toupper(k.malli.orig), 
    paikat=limith(paikat.orig,9,NA),
    pituus=limitl(pituus.orig,100,NA), 
    pituus=limith(pituus,13500,NA),
    omamassa=limitl(omamassa.orig,300,NA),
    omamassa=limith(omamassa,30000,NA),
    iskutilavuus=limitl(iskutilavuus.orig,100,NA),
    iskutilavuus=limith(iskutilavuus,15000,NA),
    leveys=limith(leveys.orig,2600,NA),
    leveys=limitl(leveys,50,NA),
    kW=limith(kW.orig,520,NA),
    kW=round(limitl(kW,1,NA)),
    Co2=limith(Co2.orig,1000,NA),
    Co2=limitl(Co2.orig,1,NA),
    matkamittarilukema=limith(matkamittarilukema.orig,999998,NA),
    matkamittarilukema=limitl(matkamittarilukema,1,NA),
    sahkohybridi=mapvalues(sahkohybridi.orig,c("","true","false"),c(NA,TRUE,FALSE)),
    sahkohybridi=ifelse(kayttoonottoVuosi < 1997,FALSE,sahkohybridi)) 

## yksinkertaistetaan polttoainevalikoimaa 
## pudotetaan koritiedoista pois alle 500 kpl korit
## Korjataan koodistosta puuttuvia ajoneuvoryhmiä

autot<-
  mutate(autot,
         kayttovoima=ifelse(!(kayttovoima.orig %in% map.polttoaine$orig),"Muu",kayttovoima.orig),
         kayttovoima=mapvalues(kayttovoima,map.polttoaine$orig,map.polttoaine$lyh),
         kori=ifelse(kori.orig %in% names(table(kori.orig)[table(kori.orig)<500]),"muu",kori.orig),
         kori=ifelse(kori=="Muut erikoiskäyttöön tarkoitetut ajoneuvot (SG)","muu",kori),
         kori=ifelse(kori=="x",NA,kori)
         )

# korjataan muutama matkailuauto (~20) korin mukaan
# lyhennetään ja yhdistetään muutamia koriryhmia

autot<-mutate(autot, ryhma=ifelse(ryhma.orig =="x" & kori.orig=="Matkailuauto (SA)","Matkailuauto",ryhma.orig))

autot<-mutate(autot,
              ryhma=mapvalues(ryhma,
                                  c("x","Henkilöauto",
                                    "Museoajoneuvo",
                                    "Maastoauto","Maastohenkilöauto",
                                    "Matkailuauto",
                                    "Invataksi","Esteetön taksiauto",
                                    "Pelastusauto","Sairasauto","Ruumisauto","Poliisiajoneuvo",
                                    "Huoltoauto"),
                                  c("Henkilö","Henkilö",
                                    "Museo",
                                    "Maasto","Maasto",
                                    "Matkailu",
                                    "Inva/esteetön","Inva/esteetön",
                                    "Pelastus","Ambulanssi","Ruumisauto","Poliisi",
                                    "Huolto")),
              ryhma=ifelse(ryhma %in% c("Henkilö","Museo","Maasto","Matkailu","Inva/esteetön",
                                                "Pelastus","Ambulanssi","Ruumisauto","Poliisi","Huolto"),ryhma,"Muu"),
              ryhma=ifelse(ryhma=="Henkilö" & luokka.orig=="Henkilöauto.m","Maasto",ryhma))

# Ajetaan merkkien/mallien korjaustiedosto!! luodaan skriptillä korjaaAutomerkit.R

korjaus<-tbl(trafi.db,"mallitmerkkikorjaus") %>% collect(n=Inf)

# korjataan merkkejä
# Korjaa automallit

autot<-left_join(
  select(autot,-k.malli), 
  select(korjaus,-n), 
  by=c("merkki.orig","k.malli.orig","malli.orig"))

# sahkohybridi muuttuja on selvästi rikki
# luokka ei ole relevantti muuttuja koska filtteroity sen mukaan
# poistetaan id-muuttujien orig-arvot, naita ei tarvitse korjailla
# kayttoonottoVuosi / ensirekVuosi ovat johdettuja samoin kuin imputointi
#

# ei-muutetut luokat saavat alkuperäisen nimen

ix<-(c(setdiff(names(select(autot,ends_with(".orig"))),paste(names(select(autot,-ends_with(".orig"))),".orig",sep=""))))
names(autot)[names(autot) %in% ix] <- gsub(".orig","",names(autot)[names(autot) %in% ix])

if (db_has_table(trafi.db$con,"henkiloautot")) db_drop_table(trafi.db$con,"henkiloautot")
as.data.frame(autot) %>% db_insert_into(trafi.db$con,"henkiloautot",.) 











