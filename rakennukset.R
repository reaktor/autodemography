library(readr) 

source("~/Projects/autodemography/initGeoDemografia.R")
source("~/Projects/autodemography/initTrafi.R")

X <- read_delim("~/Projects/buildings/Suomi_osoitteet_2017-11-15.OPT", delim=";", trim_ws=TRUE,   
col_types="ccciiicccccccccc", 
col_names=c(
  "rakennustunnus",
  "kunta",
  "maakunta",
  "kayttotarkoitus",
  "ETRS_TM35FIN_P",
  "ETRS_TM35FIN_I",
  "osoitenumero",
  "kadunnimi.fi", 
  "kadunnimi.se",
  "katunumero",
  "postinumero",
  "aanestysalue.nro",
  "aanestysalue.nimi.fi",
  "aanestysalue.nimi.se",
  "sijaintikiinteisto",
  "poiminta.pvm")
)

rakennukset <-mutate(X, kadunnimi.fi=iconv(kadunnimi.fi,"latin1","utf-8"),
          kadunnimi.se=iconv(kadunnimi.se,"latin1","utf-8"),
          aanestysalue.nimi.fi=iconv(aanestysalue.nimi.fi,"latin1","utf-8"),
          aanestysalue.nimi.se=iconv(aanestysalue.nimi.se,"latin1","utf-8"),
          kayttotarkoitus=plyr::mapvalues(kayttotarkoitus,c(0,1,2), c(NA, "asunto/toimitila", "tuotanto/muu")),
          kadunnimi=ifelse(is.na(kadunnimi.fi), kadunnimi.se, kadunnimi.fi),
          kuntanimi=map.kunta(kunta)
          ) 

pono2aanestysalue <-count(rakennukset %>% 
                            filter(kayttotarkoitus == "asunto/toimitila" & !is.na(aanestysalue.nro) & !grepl("^0000",postinumero)), kunta, aanestysalue.nro, postinumero) %>% 
  group_by(kunta, aanestysalue.nro) %>% 
  mutate(n.aanestysalue=sum(n)) %>% 
  ungroup %>% 
  group_by(postinumero) %>% 
  mutate(n.pono=sum(n)) %>% 
  ungroup %>% 
  mutate(w.aanestysalue2pono = n/n.aanestysalue, 
         w.pono2aanestysalue= n/n.pono) 

save(file="~/Projects/buildings/rakennukset.Rdata", rakennukset)
save(file="~/Projects/buildings/aanestysalue2pono.Rdata", pono2aanestysalue)


#Äänet
PV<-read_csv2("~/Projects/buildings/tpv-2018_1_aea_maa.csv",col_names=FALSE, trim_ws=TRUE) %>% 
  mutate_if(is.character, funs(iconv(., from="latin1", to="utf-8"))) %>% 
  select(kunta=X3,
         aluejako=X4, 
         alue=X5, 
         alueen.nimi.FI=X16, 
         ehdokas=X19, aania=X35, 
         aanten.osuus=X38) %>% 
  mutate(aania=as.numeric(aania), 
         aanten.osuus=as.numeric(aanten.osuus)/10) %>% 
  filter(!(aluejako %in% c("M","V"))) %>%
  mutate(aluejako=plyr::mapvalues(aluejako,c("K","A"),c("kunta","äänestysalue")), 
         alue=ifelse(aluejako=="kunta",kunta,alue))

tmp <-left_join(filter(PV, aluejako == "äänestysalue") %>% 
                  rename(aanestysalue.nro=alue), 
                pono2aanestysalue,
                by=c("kunta", "aanestysalue.nro")) %>% 
  mutate(aania = aania*w.aanestysalue2pono) %>% 
  group_by(ehdokas, postinumero) %>% 
  summarise(aania=sum(aania, na.rm=TRUE)) %>% 
  ungroup %>% 
  rename(alue=postinumero) %>% group_by(alue) %>%
  mutate(N.alue=sum(aania, na.rm=TRUE), aanten.osuus=aania/N.alue) %>%
  ungroup
  

q<-spread(select(tmp, alue, ehdokas, aania), ehdokas, aania)  %>% left_join(., select(tmp, alue, N.alue) %>% distinct(), by="alue") 

q<-left_join(rename(q, postinumero=alue), filter(paavo.5, vuosi=="2018") %>% rename(postinumero=pono)) 

             save(file="~/Projects/buildings/aanestysalue2pono.Rdata", pono2aanestysalue)


#Pysyvä rakennustunnus: 10 merkkiä pitkä aakkosnumeerinen tunnus, jonka ensimmäinen merkki on ”1” ja viimeinen merkki Modulo-31 sään- nöllä laskettu tarkistusmerkki, joka on joko numero tai iso kirjain. Tarkis- tusmerkin laskentasääntö ja käytössä olevat tarkistusmerkit ovat samat kuin henkilötunnuksen.
#Sijaintikunta: Kunta, jossa rakennus sijaitsee. Kuntanumero, sisältää etunolla
#Maakunta: Maakunnan numero, sisältää etunollat.
#Käyttötarkoitus
#char(1)
#Rakennuksen käyttötarkoituksen mukaisesti, johdetut koodiarvot:
#1 = Asuin- tai toimitilarakennus (rakennuksen käyttötarkoitus väliltä 001- 599)
#2 = Tuotanto- tai muu rakennus (rakennuksen käyttötarkoitus väliltä 600- 999)
#0 = Rakennuksen käyttötarkoituksesta ei tietoa (puuttuu tai 0)
#Pohjoiskoordinaatti: Rakennuksen keskipisteen pohjoiskoordinaatti, ETRS-TM35FIN
#Itäkoordinaatti: Rakennuksen keskipisteen itäkoordinaatti. Koordinaatisto ETRS- TM35FIN
#Osoitenumero: Moniosoitteisen rakennuksen osoitteen järjestysnumero.
#Kadunnimi suomeksi
#Kadunnimi ruotsiksi
#Katunumero
#Postinumero
#Äänestysalue: Tieto siitä mihin kunnassa olevaan äänestysalueeseen rakennus kuuluu. Äänestysaluetunnuksia on kahden tyyppisiä, tavallisimmassa tyypissä pelkkä nu- mero väliltä 001-999 (etunollat käytössä); ns. jakokirjaimellisessa tyypissä numeron perässä on iso kirjain, esim. ”001A”, ”001B”.
#Äänestysalueen nimi suomeksi
#Äänestysalueen nimi ruotsiksi
#Sijaintikiinteistö: Sen kiinteistön tunnus, jolla rakennus sijaitsee. Tieto puuttuu jos rakennukselle ei löydy sijaintikiinteistöä tai voimassaolevan rakennuksen sijaintikiinteistö on lakannut
#Tietojen poimintapäivä: pp.kk.vvvvv