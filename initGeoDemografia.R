
working.directory<-"~/Projects/autodemography"
setwd(working.directory)

# Kunnat v. 2017
map.kunta <- function(v) 
  plyr::mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
                  as.character(geo$kunta.vanhat2uudet$kunta)) %>% iconv(.,to="UTF-8")

map.kuntano <- function(v)
  plyr::mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
                  as.character(geo$kunta.vanhat2uudet$kuntano))

#postinumero tai kunta on ahvenanmaalla
is.ahvenanmaa <-function(v)  ifelse(v %in% c("Sottunga","Föglö", "Kumlinge", "Lumparland", "Sund", 
                                             "Vårdö", "Hammarland", "Eckerö","Lemland", "Finström", 
                                             "Geta", "Kökar","Saltvik","Jomala","Brändö","Maarianhamina","Mariehamn") | 
                                      grepl("^22",v),T,F)

# korvataan low-arvoa pienemmät "repvaluella"
limitl<-function(x,low,repvalue){ 
  x[x<low & !is.na(x)]<-repvalue;
  return(x);
}

# korvataan high-arvoa suuremmat "repvaluella"
limith<-function(x,high, repvalue){ 
  x[x>high & !is.na(x)]<-repvalue;
  return(x);
}

# korvataan ylä- ja alarajalla
cut.lh <-function(x, limits) {
  limitl(x,limits[1],limits[1]) %>% 
    limith(.,limits[2],limits[2]) %>% return
}

# nvl
nvl <- function(x,substit=0) {
  ifelse(is.na(x),substit,x)
}

kunnat.latlong<-read.csv(file="Data/kunnat_latlong.csv",sep="\t")

map.ikaluokka<-
  function(v) as.numeric(plyr::mapvalues(as.character(v), 
                                         c("0-4",  "5-9",  "10-14","15-19","20-24",
                                           "25-29","30-34","35-39","40-44","45-49",
                                           "50-54","55-59","60-64","65-69","70-74",
                                           "75-79","80-84","85-89","90-"),
                                         c(2,       7,      12,    17,      22,
                                           27,      32,     37,    42,      47,
                                           52,      57,     62,    67,      72,
                                           77,      82,     87,    91)))

map.vanhat.kuntanimet<-
  function(v) plyr::mapvalues(v,c("Maarianhamina - Mariehamn","Pedersören kunta","Koski Tl"), 
                              c("Maarianhamina",iconv("Pedersören",to="UTF-8"),"Koski"))
