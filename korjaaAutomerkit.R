

# Korajtaan merkkejä aluksi erilaisilla regexpeillä
# tämä voi tehdä ainkain seuraavan virheen: jos on automerkit joissa ero on välilyönnin tai esim. yhdysviivan paikka, 
# nämä niputtuvat samaan esim.: La Paz = Lap Az

# otatan vain henkilöautot

trafi.db <- src_sqlite(paste(working.directory,"/trafi.db",sep=""))

autot<-tbl(trafi.db,"trafi") %>% 
  filter(ajoneuvoluokka %in% c("Henkilöauto","Henkilöauto.m")) %>% 
  select(merkkiSelvakielinen,mallimerkinta,kaupallinenNimi,ajoneuvoryhma,korityyppi,data) %>% 
  collect(n=Inf)

# koodataan uudestaan malleja 
auto.merkit<- 
  transmute(autot,merkki=merkkiSelvakielinen,
            kori=korityyppi,
            malli=mallimerkinta,
            malli.lyh=iconv(kaupallinenNimi,from="latin1","UTF-8"))

## Skriptillä tuotetaan pohja korjaustaulukoksi; korjaa tyypillismmät kirjoitusvirheet jne.
## Lopullinen taulukko voidaan editoida käsin!!

# Haetaan uniikit orig merkit ja niiden määrät 
# Korjataa merkkejä aluksi erilaisilla regexpeillä...

merkit<-group_by(auto.merkit, merkki) %>% 
  summarise(N.orig=n()) %>% 
  select(N.orig,merkki.orig=merkki)

merkit <- merkit %>%
  mutate(merkki.korjattu=toupper(merkki.orig),
         merkki.korjattu=gsub("\\+|/","-",merkki.korjattu),
         merkki.korjattu=gsub("\\.|,","",merkki.korjattu),
         merkki.korjattu=gsub("[[:space:]]+"," ",merkki.korjattu),
         merkki.korjattu=gsub("[[:space:]]*-[[:space:]]*","-",merkki.korjattu),
         merkki.korjattu=gsub("^VOLKSWAGEN VW","VOLKSWAGEN",merkki.korjattu),
         merkki.korjattu=gsub("VW","VOLKSWAGEN",merkki.korjattu),
         merkki.korjattu=gsub("-WV ","-VOLKSWAGEN ",merkki.korjattu),
         merkki.korjattu=gsub("\\(.*\\)","",merkki.korjattu),
         merkki.korjattu=gsub("(WOLKSWAGEN|VOLSWAGEN)","VOLKSWAGEN",merkki.korjattu),
         merkki.korjattu=gsub("VOLKSWAGEN-VOLKSWAGEN","VOLKSWAGEN",merkki.korjattu),
         merkki.korjattu=gsub("^M[-]*B","MERCEDES-BENZ",merkki.korjattu),
         merkki.korjattu=gsub("-MB$","-MERCEDES-BENZ",merkki.korjattu),
         merkki.korjattu=gsub("^[ -]|[ -]$","",merkki.korjattu),
         merkki.korjattu=gsub("GROUP|GMBH|SPA$|GMBH&COKG|SPA$|SRL$|&.?CO.*","",merkki.korjattu),
         merkki.korjattu=gsub("BAYERMOTWERKE-BMW","BMW",merkki.korjattu),
         merkki.korjattu=gsub("MERCEDES-RAPIDO","MERCEDES-BENZ-RAPIDO",merkki.korjattu),
         merkki.korjattu=gsub("MERCEDES-BURSTN","MERCEDES-BENZ-BURSTNER",merkki.korjattu),
         merkki.korjattu=gsub("BUE?R[S|T]+NER|BUE?R[S|T]*NE$","BURSTNER",merkki.korjattu),
         merkki.korjattu=gsub("BÜR[S|T]+NER|BÜR[S|T]+NE$","BURSTNER",merkki.korjattu),
         merkki.korjattu=gsub("^ARMSTRONG SIDD$|^ARMSTRONG SIDDE$","ARMSTRONG-SIDDELEY",merkki.korjattu),
         merkki.korjattu=gsub("FIAT-ADRIATIC","FIAT-ADRIATIK",merkki.korjattu),
         merkki.korjattu=gsub("GOLJAT","GOLIATH",merkki.korjattu),
         merkki.korjattu=gsub("DEFHLEFFS|DETF*H?LEF+S*\\w|DETH?LEF+S*$","DETHLEFFS",merkki.korjattu),
         merkki.korjattu=gsub("FIAT-DUGATO","FIAT-DUCATO",merkki.korjattu),
         merkki.korjattu=gsub("FIAT-MAESSS","FIAT-MAESS",merkki.korjattu),
         merkki.korjattu=gsub("FIAT-MCLOIS","FIAT-MCLOUIS",merkki.korjattu),
         merkki.korjattu=gsub("BISHOFF","BISCHOFF",merkki.korjattu),
         merkki.korjattu=gsub("FIAT-WEINSBERK","FIAT-WEINSBERG",merkki.korjattu),
         merkki.korjattu=gsub("GIOTTILIN$","GIOTTILINE",merkki.korjattu),
         merkki.korjattu=gsub("\\-RAPID ","-RAPIDO ",merkki.korjattu),
         merkki.korjattu=gsub("\\-RAPID$","-RAPIDO",merkki.korjattu),
         merkki.korjattu=gsub("PACARD","PACKARD",merkki.korjattu),
         merkki.korjattu=gsub("STUDEBACKER","STUDEBAKER",merkki.korjattu),
         merkki.korjattu=gsub("STANDART","STANDARD",merkki.korjattu),
         merkki.korjattu=gsub("TOYOTA MOTORSPORT","TOYOTA",merkki.korjattu),
         merkki.korjattu=gsub("PÖLLS|PÖSLL","PÖSSL",merkki.korjattu),
         merkki.korjattu=gsub("TRICANO","TRIGANO",merkki.korjattu),
         merkki.korjattu=gsub("KARM$","KARMANN",merkki.korjattu),
         merkki.korjattu=gsub("SWIF$","SWIFT",merkki.korjattu),
         merkki.korjattu=gsub("[ |-]AUTO[ |-]ROLLE$","AUTOROLLER",merkki.korjattu),
         merkki.korjattu=gsub("[ |-]AUTOROLLE$","AUTOROLLER",merkki.korjattu),
         merkki.korjattu=gsub("EIFFELLAND","EIFELLAND",merkki.korjattu),
         merkki.korjattu=gsub("GULF STREAM GMC","GULFSTREAM",merkki.korjattu),
         merkki.korjattu=gsub("WINNIBAGO","WINNEBAGO",merkki.korjattu),
         merkki.korjattu=gsub("FORD[ |-]CHALLANGER","FORD[ |-]CHALLENGER",merkki.korjattu),
         merkki.korjattu=gsub("CHALLEGER","CHALLENGER",merkki.korjattu),
         merkki.korjattu=gsub("^INFINITY$","^INFINITI$",merkki.korjattu),
         merkki.korjattu=gsub("FORD.CHAUSON","FORD.CHAUSSON",merkki.korjattu),
         merkki.korjattu=gsub("MERSEDES-BENZ","MERCEDES-BENZ",merkki.korjattu),
         merkki.korjattu=gsub("FORD[ |-]RIMON","FORD-RIMOR",merkki.korjattu),
         merkki.korjattu=gsub("EURAMOBILE","EURA-MOBIL",merkki.korjattu),
         merkki.korjattu=gsub("^ADRIA MOBIL$","ADRIA",merkki.korjattu),
         merkki.korjattu=gsub("INDUSTRIE GIOTTILINE","GIOTTILINE",merkki.korjattu),
         merkki.korjattu=gsub("[-| ]DUERRE|[- | ]DUEERRE","-DUE ERRE",merkki.korjattu),
         merkki.korjattu=gsub("KS HUOM","",merkki.korjattu),
         merkki.korjattu=gsub("GM DAEWOO","DAEWOO",merkki.korjattu),        # sama merkki?
         merkki.korjattu=gsub("LADA-VAZ","LADA",merkki.korjattu),           # sama merkki?
         merkki.korjattu=gsub("AMG HUMMER","HUMMER",merkki.korjattu),       # sama merkki
         merkki.korjattu=gsub("[[:space:]]*-[[:space:]]*","-",merkki.korjattu),
         merkki.korjattu=gsub("[[:space:]]+"," ",merkki.korjattu),
         merkki.korjattu=gsub(" $","",merkki.korjattu),
         key=gsub("([[:punct:]]|[[:space:]])*","",merkki.korjattu), # muoto jossa kaikki kirjaimet yhteen
         blank=str_count(merkki.korjattu,"[[:space:]]"),  # lasketaan blankien määrää; ks. kohta
         merkki.korjattu=iconv(merkki.korjattu,to="UTF-8"),
         key=iconv(key,to="UTF-8")
  ) %>% 
  arrange(key,blank) %>% 
  group_by(key) %>% 
  mutate(N=sum(N.orig)) 

# orig = alkuperäinen merkkinimi, merkki.korjattu=yhtenäistetty merkkinimi
# tällä systeemillä otetaan duplikaateista mukaan ne joissa on vähiten tyhjiä => 
# esim. otetaan mukaan merkkijono jossa on väliviiva, jos sellainen asu löytyy

# !!tämä voi tehdä ainkain seuraavan virheen: jos on automerkit joissa ero on välilyönnin tai esim. yhdysviivan paikka, 
# nämä niputtuvat samaan esim.: La Paz = Lap Az

merkit.stat<-merkit %>% 
  slice(1) %>% ungroup %>% 
  select(key,merkki.korjattu) %>% 
  merge(select(merkit,-merkki.korjattu), ., by="key") %>% 
  select(.,-key,-blank) %>% rename(N.korjattu=N)

#Yksinkertainen merkkitaulukko kantaan ja fileen

if (db_has_table(trafi.db$con,"merkitmap")) db_drop_table(trafi.db$con,"merkitmap")
merkit.stat %>% db_insert_into(trafi.db$con,"merkitmap",.) 

write.table(merkit.stat,file="merkitmap.csv",quote=FALSE,sep="\t",row.names=FALSE,fileEncoding="UTF-8",append=FALSE)

autot<-mutate(autot, 
       merkki.orig=merkkiSelvakielinen,
       merkki=mapvalues(merkkiSelvakielinen, merkit.stat$merkki.orig, merkit.stat$merkki.korjattu),
       k.malli.orig=kaupallinenNimi,
       k.malli=toupper(kaupallinenNimi),
       malli.orig=mallimerkinta,
       malli=toupper(mallimerkinta),
       merkki=ifelse(merkki=="QUATTRO","AUDI",merkki),
       merkki=ifelse(merkki %in% c("BMW I","BMW M3"),"BMW",merkki),
       merkki=ifelse(merkki=="FORD-CNG-TECHNIK","FORD",merkki),
       merkki=ifelse(grepl("Jaguar Land Rover Limited", merkki, ignore.case=TRUE) & 
                       grepl("Jaguar",malli,ignore.case=TRUE),"JAGUAR",merkki),
       merkki=ifelse(grepl("Jaguar Land Rover Limited", merkki, ignore.case=TRUE) & 
                       grepl("LAND.?ROVER",malli,ignore.case=TRUE),"LAND-ROVER",merkki),
       merkki=ifelse(grepl("Jaguar Land Rover Limited", merkki, ignore.case=TRUE) & 
                       grepl("FREELANDER|DISCOVERY",malli,ignore.case=TRUE),"LAND-ROVER",merkki),
       k.malli=ifelse(grepl("RANGE.?ROVER", merkki, ignore.case=TRUE), "RANGE-ROVER",k.malli),
       merkki=ifelse(grepl("RANGE.?ROVER", merkki, ignore.case=TRUE) | 
                       grepl("Jaguar Land Rover Limited", merkki, ignore.case=TRUE) & 
                       grepl("RANGE.?ROVER",malli,ignore.case=TRUE)
                       ,"LAND-ROVER",merkki))

autot<-select(autot,-merkkiSelvakielinen,-kaupallinenNimi)

a<-count(autot,merkki,k.malli,malli,k.malli.orig,malli.orig) %>% ungroup

# Poista merkki mallinimestä
a$k.malli<-
  mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
         a$merkki,a$k.malli,USE.NAMES=FALSE)

a$malli<-
  mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
         a$merkki,a$malli,USE.NAMES=FALSE)

hsub <- function(pattern,subst,target) 
  ifelse(grepl(paste("^",pattern,sep=""),target),target,gsub(pattern,subst,target))

clear <- function(txt) {
  txt<-gsub(",",".",txt)
  txt<-gsub("\\(.*\\)"," ",txt)
  txt<-gsub("[\\(|\\)|\\{|\\}]"," ",txt)
  txt<-gsub("[^\\.[:^punct:]]","-",txt,perl=TRUE)
  txt<-gsub("[[:space:]]+"," ",txt)
  txt<-gsub("[[:space:]]+-+"," -",txt)
  txt<-gsub("-+[[:space:]]*","-",txt)
  txt<-gsub("([[:space:]]|-|\\.)*(\\.)+([[:space:]]|-|\\.)*",".",txt)
  txt<-gsub("^[[:punct:]|[:space:]]*","",txt)
  txt<-gsub("[[:punct:]|[:space:]]*$","",txt)
  txt<-gsub("[[:space:]]+"," ",txt)
  return(txt)
}

korjaa.malli<-function(merkki,malli){
  malli<-ifelse(merkki=="VOLKSWAGEN",gsub("VW"," ",malli),malli)
  malli<-gsub("^[0-9]D|^[0-9] D|[ |-][0-9]D\\b","",malli)
  malli<-gsub("[0-9]OV$|[0-9]OV\\W|[0-9]DR"," ",malli)
  malli<-gsub("[0-9]{3,4}CM3"," ",malli)
  malli<-clear(malli)
  
  malli<-gsub("\\bSEDAN\\b|\\bHATCHBACK\\b|\\bHATCBACK\\b|\\bFASTBACK\\b|\\bFARMARI\\b|\\bLIFTBACK\\b|\\bCOUPE\\b|\\bCOUPÉ\\b|\\bSTW\\b|\\bCA.RIOLET\\b|\\bAVANT\\b|\\bCLASSIC\\b|\\bCABRIO\\b|\\bAVOAUTO\\b|\\bVIISTOPERÄ\\b|\\bMONIKÄYTTÖAJONEUVO\\b|\\bUMPIKORINEN\\b|\\bUMPI/AVO\\b|\\bYKSIKERROKSINEN\\b|\\bAUTOMATIC\\b|\\bKOMBI\\b|\\bWAGON\\b|\\bCARAVAN\\b|\\bCONVERTIBLE\\b|\\bVARIANT\\b|\\bTOURING\\b|\\bTOURER\\b|\\bSPORTBACK\\b|\\bBIFUEL\\b|\\bDIESEL\\b|\\bERIKOISKÄYTTÖÖN\\b|\\bAJONEUVOT\\b|\\bAVOLAVAKUORMA\\b|\\bCOMBI\\b|\\bPLUG[-| ]?IN\\b|\\bLIMOUSINE\\b|\\bSPORTS?\\b|\\bTURBO\\b|\\bAWD\\b|\\b4WD\\b|\\bSERIES\\b|\\bMPV\\b|\\b4X4\\b|\\bAWD\\b|\\bX-?DRIVE\\b|\\b4-?MATIC\\b|\\b4-?MOTION\\b|\\bALLROAD\\b|\\bHYBRID\\b|\\bSYNCRO\\b|\\bBLUETEC\\b|\\bKOMPRESSOR"," ",malli)
  malli<-gsub("\\bAUTOMAT\\b|\\bAUTOMATI\\b|\\bCABRIO"," ",malli)
  malli<-gsub("\\bAUTOMA\\b|\\bAUTOM\\b|\\bFAMILIAL\\b|\\bFAMILIA\\b"," ",malli)
  malli<-clear(malli)  
  
  malli<-gsub("\\b(16V|8V|24V|20V|V8|V6|V12)\\b"," ",malli)
  malli<-clear(malli)
  
  malli<-hsub("(-| )[[:alnum:]|\\.]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+$"," ",malli)
  malli<-clear(malli)
  malli<-hsub("(-| )[[:alnum:]|\\.]+-[[:alnum:]]+-[[:alnum:]]+$"," ",malli)
  malli<-clear(malli)
  malli<-hsub("(-| )[[:alnum:]|\\.]+-[[:alnum:]]+$"," ",malli)
  malli<-clear(malli)
  
  malli<-gsub("\\b[1-9]\\.[0-9][[:alnum:]]*\\b"," ",malli)
  malli<-gsub("[0-9]\\.[0-9].*$"," ",malli)
  malli<-clear(malli)
  
  malli<-gsub("\\b(XLI|XSI|CC|GT|GTI|SE|HT|AUT|GL|TD|UNLIMITED|LIMITED|HARDTOP|DE|HB|LS|SL|VAN|CAB|LE|LX|RS|DOOR|SC|TC|TDI|TS|AT|BUSINESS|EDITION|ES|GLS|GLX|GS|GTS|STATION|XL|2DHT|DL|MODEL|CS|GX|HD|SD|CL|FSI|GLI|GTC|GTX|LT|ML|SLS|SV|SW|SX|TB|TCI|XR|4DSEDAN|AVO|CDI|CDTI|CI|CJ|CNG|CRDI|CUOPE|DB|DI|DPF|DT|DTS|DX|EX|FX|GF|GTE|GTA|HDI|IS|LD|TFSI|TIPTRONIC)$"
              ," ",malli)
  
  malli<-ifelse(merkki=="OPEL",gsub("-.*$|STATION|SW|NOTCHBACK|GTC|NB"," ",malli),malli)
  malli<-ifelse(merkki=="ALFA ROMEO",gsub("ALFA"," ",malli),malli)
  malli<-ifelse(merkki=="PEUGEOT",gsub("FAMILIAL|BREAK"," ",malli),malli)
  malli<-ifelse(merkki=="BMW",gsub("(X|S)DRIVE.*$|COMPACT","",malli),malli)
  malli<-ifelse(merkki=="VOLKSWAGEN",gsub("\\b(SPORTSVAN|KUPLA)\\b"," ",malli),malli)
  malli<-ifelse(merkki=="VOLVO",gsub("CROSS COUNTRY"," ",malli),malli)
  malli<-ifelse(merkki=="AUDI",gsub("SPORTBACK"," ",malli),malli)
  malli<-ifelse(merkki=="HONDA",gsub("AERODECK"," ",malli),malli)
  malli<-ifelse(merkki=="SAAB",gsub("VECTOR.*$|SPORTCOM.*$|SPORT|LINEAR.*$|AERO.*$|CD.*$|CSE.*$|SE.*$|ESTATE.*$"," ",malli),malli)
  malli<-ifelse(merkki=="MITSUBISHI",gsub("EVOLUTION.*$|STAR"," ",malli),malli)
  malli<-ifelse(merkki=="GAZ",gsub("WOLGA","VOLGA",malli),malli)
  malli<-ifelse(merkki=="GAZ",gsub("POPEDA","POBEBDA",malli),malli)
  malli<-ifelse(merkki=="GAZ",gsub("TSHAIKA","TSAIKA",malli),malli)
  malli<-clear(malli)
  
  malli<-str_match(malli,"^[0-9]+[[:punct:]][[:alnum:]]+|^[0-9]+|^[[:alnum:]]+[[:space:]|[:punct:]]*[[:alnum:]]*")
  
  malli<-clear(malli)
  
  return(malli)}

A<-mutate(a,k.malli=korjaa.malli(merkki,k.malli),
          malli=korjaa.malli(merkki,malli),
          k.malli.key=gsub("[[:space:]]","",k.malli),
          malli.key=gsub("[[:space:]]","",malli),
          k.malli.key=ifelse(k.malli.key=="" | is.na(k.malli.key),malli.key,k.malli.key),
          key=k.malli.key,
          k.malli=ifelse(k.malli==""|is.na(k.malli),NA,k.malli))

small.limit.1<-20
small.limit.2<-200
key.bound<-list()
key.nonbound<-list()
key.nonbound.n<-list()
key.bound.n<-list()
B<-A

for (i in setdiff(unique(B$merkki),"")) {
  Q<-filter(B,k.malli.key !="" & merkki==i & !is.na(k.malli.key) & str_length(k.malli.key)>=2) %>% 
    group_by(merkki,k.malli.key) %>% 
    summarise(N=sum(n)) %>% 
    group_by(merkki) %>% 
    filter(N>=small.limit.1) %>% 
    select(merkki,k.malli.key,N) %>% 
    mutate(l=length(k.malli.key))
  q<-Q$k.malli.key
  n<-Q$N
  if (length(q)>0) {
    key.bound[i]<-paste("\\b",q[order(-str_length(q))],"\\b",sep="") %>% paste(.,collapse="|")
    key.nonbound[i]<-paste(q[order(-str_length(q))],sep="") %>% paste(.,collapse="|")
    key.bound.n[i]<-paste("\\b",q[order(-n)],"\\b",sep="") %>% paste(.,collapse="|")
    key.nonbound.n[i]<-paste(q[order(-n)],sep="") %>% paste(.,collapse="|")
    B<-mutate(B,key=ifelse(is.na(key) & merkki==i, str_match(malli.key,key.bound[[i]]),key))
    B<-mutate(B,key=ifelse(is.na(key) & merkki==i, str_match(gsub(" ","",malli),key.nonbound[[i]]),key))
  }
}

B<-group_by(B,merkki,key) %>% mutate(N=sum(n)) %>% ungroup

for (i in setdiff(names(key.nonbound),"")) {
  print(i)
  ix<- B$N < small.limit.2 & B$merkki==i
  m <-str_match(B$key[ix],key.nonbound.n[[i]]) 
  B$key[ix]<-m
}

B<-filter(B,!is.na(key))

modeltable<-group_by(B,merkki,key) %>% 
  mutate(l=str_length(k.malli), l=ifelse(is.na(l)|l==0, 0,-as.numeric(N))) %>% 
  arrange(l) %>%
  slice(1) %>% ungroup %>% 
  mutate(k.malli=ifelse(is.na(k.malli), key, k.malli)) %>% 
  select(merkki, k.malli, key) %>% ungroup

mallit.merkki.korjaus <- select(B, merkki, k.malli.orig, malli.orig, key) %>% 
  left_join(.,select(modeltable, merkki, key, k.malli), by=c("merkki","key")) %>%
  select(-key) %>% 
  filter(!is.na(merkki) & merkki != "" & ! is.na(k.malli))

autot<-select(autot,-k.malli)
autot<-left_join(autot,mallit.merkki.korjaus,by=c("merkki","k.malli.orig","malli.orig"))

korjaus<-count(autot,merkki.orig,malli.orig,k.malli.orig,merkki,k.malli) %>% ungroup

korjaus<-mutate(korjaus,k.malli=ifelse(k.malli=="RANGE ROVER","RANGE-ROVER",k.malli))

if (db_has_table(trafi.db$con,"mallitmerkkikorjaus")) db_drop_table(trafi.db$con,"mallitmerkkikorjaus")
as.data.frame(korjaus) %>% db_insert_into(trafi.db$con,"mallitmerkkikorjaus",.) 

write.table(korjaus,file="mallitmerkkikorjaus.csv",quote=FALSE,sep="\t",row.names=FALSE,fileEncoding="UTF-8",append=FALSE)











