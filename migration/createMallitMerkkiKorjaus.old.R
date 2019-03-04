# Korajtaan merkkejä aluksi erilaisilla regexpeillä
# tämä voi tehdä ainkain seuraavan virheen: jos on automerkit joissa ero on välilyönnin tai esim. yhdysviivan paikka,
# nämä niputtuvat samaan esim.: La Paz = Lap Az

# otatan vain henkilöautot

#source("initTrafi.R")

autot <- tbl(trafi.db, "autotiedot") %>%
  select(
    merkkiSelvakielinen,
    mallimerkinta,
    kaupallinenNimi,
    ajoneuvoryhma,
    korityyppi,
    N.id = N.combinations
  ) %>%
  collect(n = Inf)

# koodataan uudestaan malleja
auto.merkit <-
  transmute(
    autot,
    merkki = merkkiSelvakielinen,
    kori = korityyppi,
    malli = mallimerkinta,
    malli.lyh = kaupallinenNimi,
    N = N.id
  )

## Skriptillä tuotetaan pohja korjaustaulukoksi; korjaa tyypillismmät kirjoitusvirheet jne.
## Lopullinen taulukko voidaan editoida käsin!!

# Haetaan uniikit orig merkit ja niiden määrät
# Korjataa merkkejä aluksi erilaisilla regexpeillä...

merkit <- group_by(auto.merkit, merkki) %>%
  summarise(N.orig = sum(N)) %>%
  select(N.orig, merkki.orig = merkki)

merkit <- merkit %>%
  mutate(
    merkki.korjattu = toupper(merkki.orig),
    merkki.korjattu = gsub("\\+|/", "-", merkki.korjattu),
    merkki.korjattu = gsub("\\.|,", "", merkki.korjattu),
    merkki.korjattu = gsub("[[:space:]]+", " ", merkki.korjattu),
    merkki.korjattu = gsub("[[:space:]]*-[[:space:]]*", "-", merkki.korjattu),
    merkki.korjattu = gsub("^VOLKSWAGEN VW", "VOLKSWAGEN", merkki.korjattu),
    merkki.korjattu = gsub("VW", "VOLKSWAGEN", merkki.korjattu),
    merkki.korjattu = gsub("-WV ", "-VOLKSWAGEN ", merkki.korjattu),
    merkki.korjattu = gsub("\\(.*\\)", "", merkki.korjattu),
    merkki.korjattu = gsub("(WOLKSWAGEN|VOLSWAGEN)", "VOLKSWAGEN", merkki.korjattu),
    merkki.korjattu = gsub("VOLKSWAGEN-VOLKSWAGEN", "VOLKSWAGEN", merkki.korjattu),
    merkki.korjattu = gsub("^M[-]*B", "MERCEDES-BENZ", merkki.korjattu),
    merkki.korjattu = gsub("-MB$", "-MERCEDES-BENZ", merkki.korjattu),
    merkki.korjattu = gsub("^[ -]|[ -]$", "", merkki.korjattu),
    merkki.korjattu = gsub(
      "GROUP|GMBH|SPA$|GMBH&COKG|SPA$|SRL$|&.?CO.*", "", merkki.korjattu
    ),
    merkki.korjattu = gsub("BAYERMOTWERKE-BMW", "BMW", merkki.korjattu),
    merkki.korjattu = gsub("MERCEDES-RAPIDO", "MERCEDES-BENZ-RAPIDO", merkki.korjattu),
    merkki.korjattu = gsub("MERCEDES-BURSTN", "MERCEDES-BENZ-BURSTNER", merkki.korjattu),
    merkki.korjattu = gsub("BUE?R[S|T]+NER|BUE?R[S|T]*NE$", "BURSTNER", merkki.korjattu),
    merkki.korjattu = gsub("BÜR[S|T]+NER|BÜR[S|T]+NE$", "BURSTNER", merkki.korjattu),
    merkki.korjattu = gsub(
      "^ARMSTRONG SIDD$|^ARMSTRONG SIDDE$", "ARMSTRONG-SIDDELEY", merkki.korjattu
    ),
    merkki.korjattu = gsub("FIAT-ADRIATIC", "FIAT-ADRIATIK", merkki.korjattu),
    merkki.korjattu = gsub("GOLJAT", "GOLIATH", merkki.korjattu),
    merkki.korjattu = gsub(
      "DEFHLEFFS|DETF*H?LEF+S*\\w|DETH?LEF+S*$", "DETHLEFFS", merkki.korjattu
    ),
    merkki.korjattu = gsub("FIAT-DUGATO", "FIAT-DUCATO", merkki.korjattu),
    merkki.korjattu = gsub("FIAT-MAESSS", "FIAT-MAESS", merkki.korjattu),
    merkki.korjattu = gsub("FIAT-MCLOIS", "FIAT-MCLOUIS", merkki.korjattu),
    merkki.korjattu = gsub("BISHOFF", "BISCHOFF", merkki.korjattu),
    merkki.korjattu = gsub("FIAT-WEINSBERK", "FIAT-WEINSBERG", merkki.korjattu),
    merkki.korjattu = gsub("GIOTTILIN$", "GIOTTILINE", merkki.korjattu),
    merkki.korjattu = gsub("\\-RAPID ", "-RAPIDO ", merkki.korjattu),
    merkki.korjattu = gsub("\\-RAPID$", "-RAPIDO", merkki.korjattu),
    merkki.korjattu = gsub("PACARD", "PACKARD", merkki.korjattu),
    merkki.korjattu = gsub("STUDEBACKER", "STUDEBAKER", merkki.korjattu),
    merkki.korjattu = gsub("STANDART", "STANDARD", merkki.korjattu),
    merkki.korjattu = gsub("TOYOTA MOTORSPORT", "TOYOTA", merkki.korjattu),
    merkki.korjattu = gsub("POESSL|PÖLLS|PÖSLL", "PÖSSL", merkki.korjattu),
    merkki.korjattu = gsub("TRICANO", "TRIGANO", merkki.korjattu),
    merkki.korjattu = gsub("KARM$", "KARMANN", merkki.korjattu),
    merkki.korjattu = gsub("SWIF$", "SWIFT", merkki.korjattu),
    merkki.korjattu = gsub("[ |-]AUTO[ |-]ROLLE$", "AUTOROLLER", merkki.korjattu),
    merkki.korjattu = gsub("[ |-]AUTOROLLE$", "AUTOROLLER", merkki.korjattu),
    merkki.korjattu = gsub("EIFFELLAND", "EIFELLAND", merkki.korjattu),
    merkki.korjattu = gsub("GULF STREAM GMC", "GULFSTREAM", merkki.korjattu),
    merkki.korjattu = gsub("WINNIBAGO", "WINNEBAGO", merkki.korjattu),
    merkki.korjattu = gsub("FORD[ |-]CHALLANGER", "FORD[ |-]CHALLENGER", merkki.korjattu),
    merkki.korjattu = gsub("CHALLEGER", "CHALLENGER", merkki.korjattu),
    merkki.korjattu = gsub("^INFINITY$", "^INFINITI$", merkki.korjattu),
    merkki.korjattu = gsub("FORD.CHAUSON", "FORD.CHAUSSON", merkki.korjattu),
    merkki.korjattu = gsub("MERSEDES-BENZ", "MERCEDES-BENZ", merkki.korjattu),
    merkki.korjattu = gsub("FORD[ |-]RIMON", "FORD-RIMOR", merkki.korjattu),
    merkki.korjattu = gsub("EURAMOBILE", "EURA-MOBIL", merkki.korjattu),
    merkki.korjattu = gsub("^ADRIA MOBIL$", "ADRIA", merkki.korjattu),
    merkki.korjattu = gsub("INDUSTRIE GIOTTILINE", "GIOTTILINE", merkki.korjattu),
    merkki.korjattu = gsub("[-| ]DUERRE|[- | ]DUEERRE", "-DUE ERRE", merkki.korjattu),
    merkki.korjattu = gsub("KS HUOM", "", merkki.korjattu),
    merkki.korjattu = gsub("GM DAEWOO", "DAEWOO", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("LADA-VAZ", "LADA", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("^TESLA$", "TESLA MOTORS", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("AMG HUMMER", "HUMMER", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("^MERCEDES$","MERCEDES-BENZ", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("^QUATTRO$|^AUDI TT$","AUDI", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("^BMW I$|^BMW M3|^BMW MOTORSPORT$", "BMW", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("^FORD-A$|^FORD A$|^FORD-T$|^FORD T$|^FORD-CNG-TECHNIK$", "FORD", merkki.korjattu), # sama merkki?
    merkki.korjattu = gsub("[[:space:]]*-[[:space:]]*", "-", merkki.korjattu),
    merkki.korjattu = gsub("[[:space:]]+", " ", merkki.korjattu),
    merkki.korjattu = gsub(" $", "", merkki.korjattu),
    key = gsub("([[:punct:]]|[[:space:]])*", "", merkki.korjattu), # muoto jossa kaikki kirjaimet yhteen
    blank = str_count(merkki.korjattu, "[[:space:]]"), # lasketaan blankien määrää; ks. kohta
    merkki.korjattu = iconv(merkki.korjattu, to = "UTF-8"),
    key = iconv(key, to = "UTF-8")
  ) %>%
  arrange(key, blank) %>%
  group_by(key) %>%
  mutate(N = sum(N.orig))

# orig = alkuperäinen merkkinimi, merkki.korjattu=yhtenäistetty merkkinimi
# tällä systeemillä otetaan duplikaateista mukaan ne joissa on vähiten tyhjiä =>
# esim. otetaan mukaan merkkijono jossa on väliviiva, jos sellainen asu löytyy

# !!tämä voi tehdä ainkain seuraavan virheen: jos on automerkit joissa ero on välilyönnin tai esim. yhdysviivan paikka,
# nämä niputtuvat samaan esim.: La Paz = Lap Az

merkit.stat <- merkit %>%
  dplyr::slice(1) %>% ungroup %>%
  select(key, merkki.korjattu) %>%
  merge(select(merkit, -merkki.korjattu), ., by = "key") %>%
  select(., -key, -blank) %>%
  rename(N.korjattu = N)

#Yksinkertainen merkkitaulukko kantaan ja fileen

if (db_has_table(trafi.db$con, "merkitmap")) db_drop_table(trafi.db$con, "merkitmap")
merkit.stat %>% db_insert_into(trafi.db$con, "merkitmap", .)

write.table(
  merkit.stat,
  file = full.path("merkitmap.csv"),
  quote = FALSE,
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  append = FALSE
)


## Korjataan merkit; aloitetaan mallien korjaaminen
autot <- mutate(
  autot,
  merkki = plyr::mapvalues(
    merkkiSelvakielinen,
    merkit.stat$merkki.orig,
    merkit.stat$merkki.korjattu
  ),
  k.malli = toupper(kaupallinenNimi),
  malli = toupper(mallimerkinta))

# Korjataan mallista ja merkistä riippuvia asioita

autot <- mutate(
  autot,
  merkki = ifelse(
    grepl("Jaguar Land Rover Limited", merkki, ignore.case = TRUE) & grepl("Jaguar", malli, ignore.case = TRUE), 
    "JAGUAR", 
    merkki
  ),
  merkki = ifelse(
    grepl("Jaguar Land Rover Limited", merkki, ignore.case = TRUE) & grepl("LAND.?ROVER", malli, ignore.case = TRUE), 
    "LAND-ROVER", 
    merkki
  ),
  merkki = ifelse(
    grepl("Jaguar Land Rover Limited", merkki, ignore.case = TRUE) & grepl("FREELANDER|DISCOVERY", malli, ignore.case = TRUE), 
    "LAND-ROVER", 
    merkki
  ),
  k.malli = ifelse(
    grepl("RANGE.?ROVER", merkki, ignore.case = TRUE), 
    "RANGE-ROVER", 
    k.malli
  ),
  merkki = ifelse(
    grepl("RANGE.?ROVER", merkki, ignore.case = TRUE) | 
      grepl("Jaguar Land Rover Limited", merkki, ignore.case = TRUE) &
      grepl("RANGE.?ROVER", malli, ignore.case = TRUE), 
    "RANGE-ROVER", 
    merkki
  )
)

mallit <- group_by(autot, 
                   ajoneuvoryhma,
                   merkki, 
                   k.malli, 
                   malli, 
                   kaupallinenNimi, 
                   mallimerkinta) %>% 
  summarise(n = sum(N.id)) %>% 
  ungroup %>% 
  filter(ajoneuvoryhma %in% c(NA, 'Maastohenkilöauto' ))

# Poista merkki mallinimistä #####

mallit$k.malli <-
  mapply(function(x, y)
    gsub(paste0("\\b",x,"\\b"), "", y, ignore.case = TRUE),
    mallit$merkki,
    mallit$k.malli,
    USE.NAMES = FALSE)

mallit$malli <-
  mapply(function(x, y)
    gsub(paste0("\\b",x,"\\b"), "", y, ignore.case = TRUE),
    mallit$merkki,
    mallit$malli,
    USE.NAMES = FALSE)


# korvaa kuten gsub mutta ei jos pattern on alussa
hsub <- function(pattern, subst, target)
  ifelse(grepl(paste("^", pattern, sep = ""), target), target, gsub(pattern, subst, target))

# poista turhat välimerkki + space -yhdistelmät ja suluissa olevat
clear <- function(txt) {
  txt <- gsub(",", ".", txt)
  txt <- gsub("\\(.*\\)", " ", txt)
  txt <- gsub("[\\(|\\)|\\{|\\}]", " ", txt)
  txt <- gsub("[^\\.[:^punct:]]", "-", txt, perl = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt)
  txt <- gsub("[[:space:]]+-+", " -", txt)
  txt <- gsub("-+[[:space:]]*", "-", txt)
  txt <- gsub("([[:space:]]|-|\\.)*(\\.)+([[:space:]]|-|\\.)*", ".", txt)
  txt <- gsub("^[[:punct:]|[:space:]]*", "", txt)
  txt <- gsub("[[:punct:]|[:space:]]*$", "", txt)
  txt <- gsub("[[:space:]]+", " ", txt)
  return(txt)
}

# Korjaa mallimerkintä, poistetaan ylimääräistä tekstiä

puhdista.malli <- function(merkki, malli) {

  malli <- clear(malli)
  
  # poistetaan "desimaaliluvuista eteenpäin loppuun 1.5 2.0GL  jne
  #malli <- gsub("\\b[1-9]\\.[0-9][[:alnum:]]*\\b", " ", malli)
  malli <- gsub("[0-9]\\.[0-9].*$", "", malli)
  
  # Volkswagen auki, alussa tai keskellä oleva ovisuus (esim. 4D, 4OV) pois, kuutiot pois
  malli <- ifelse(merkki == "VOLKSWAGEN", gsub("VW", " ", malli), malli)
  malli <- gsub("^[0-9]D|^[0-9] D|[ |-][0-9]D\\b", "", malli)
  malli <- gsub("[0-9]OV$|[0-9]OV\\W|[0-9]DR", " ", malli)
  malli <- gsub("[0-9]{3,4}CM3.*$", " ", malli)
  
  malli <- clear(malli)
  
  # Venttiilit pois
  malli <- gsub("\\b(16V|8V|24V|20V|V8|V6|V12)\\b", " ", malli)
  
  malli <- clear(malli)

  # muutamia lisämääreitä, esim. koriin, nelivetoon ja polttoaineeseen liittyen pois
  
  remove.strs <- c(
    "SEDAN",
    "4DSEDAN",
    "HATCHBACK",
    "HATCBACK",
    "FASTBACK",
    "NOTCHBACK",
    "FARMARI",
    "LIFTBACK",
    "COUPE",
    "COUPÉ",
    "STW",
    "CA.RIOLET",
    "CABRIO",
    "AVOAUTO",
    "MATKAILUAUTO",
    "VIISTOPERÄ",
    "MONIKÄYTTÖAJONEUVO",
    "UMPIKORINEN",
    "UMPI/AVO",
    "YKSIKERROKSINEN",
    "STATION WAGON",
    "SPORTWAGON",
    "SPORTSWAGON",
    "WAGON",
    "CARAVAN",
    "HARDTOP",
    "CONVERTIBLE",
    "TOURING",
    "TOURER",
    "SPORTBACK",
    "ERIKOISKÄYTTÖÖN",
    "MPV",
    "AJONEUVOT",
    "AVOLAVAKUORMA",
    "LIMOUSINE",
    "AUTOM",
    "AUTOMA",
    "AUTOMAT",
    "AUTOMATI",
    "AUTOMATIC",
    "TIPTRONIC",
    "SPORTS?",
    "TURBO",
    "TURBODIESEL",
    "AWD",
    "4WD",
    "SERIES",
    "4X4",
    "SYNCRO",
    "X-?DRIVE",
    "4-?MATIC",
    "4-?MOTION",
    "ALLROAD",
    "HYBRID",
    "PLUG[-| ]?IN",
    "BLUETEC",
    "AVANT",
    "COMBI",
    "CLASSIC",
    "VARIANT",
    "BIFUEL",
    "DIESEL",
    "KOMPRESSOR")
  
  malli<- gsub(paste0("\\b", paste0(remove.strs, collapse=".*$|\\b"), ".*$"), "", malli)
  malli <- clear(malli)
  
  # Poistetaan pattern jossa enemmän kuin yksi väliviiva
  
  malli <- gsub(" .*-.*-.*$", "", malli)
  
  #malli <- ifelse(malli %in% c("FOCUS C-MAX", "GRAND C-MAX"), malli, hsub("(-| )[[:alnum:]|\\.]+-[[:alnum:]]+-[[:alnum:]]+\\b", " ", malli))
  #malli <- clear(malli)
  
  #malli <- ifelse(malli %in% c("FOCUS C-MAX", "GRAND C-MAX"), malli, hsub("(-| )[[:alnum:]|\\.]+-[[:alnum:]]+\\b", " ", malli))
  #malli <- clear(malli)
  
  #malli <- ifelse(malli %in% c("FOCUS C-MAX", "GRAND C-MAX"), malli, str_match(malli, "^[0-9]+[[:punct:]][[:alnum:]]+|^[0-9]+|^[[:alnum:]]+[[:space:]|[:punct:]]*[[:alnum:]]*"))
  #malli <- clear(malli)
  
  #malli <- ifelse(malli %in% c("FOCUS C-MAX", "GRAND C-MAX"), malli, str_match(malli, "^[0-9]+[[:punct:]][[:alnum:]]+|^[0-9]+|^[[:alnum:]]+[[:space:]|[:punct:]]*[[:alnum:]]*"))
  #malli <- clear(malli)
  

  
    # poistetaan "desimaaliluvuista eteenpäin loppuun 1.5 2.0GL  jne
  #malli <- gsub("\\b[1-9]\\.[0-9][[:alnum:]]*\\b", " ", malli)
  #malli <- gsub("[0-9]\\.[0-9].*$", "", malli)
  malli <- clear(malli)
  
  # poistetaan GT, GTX yms. tyyppisiä sanoja merkintöjä stringin lopusta
  if(FALSE) {
  malli <-
    gsub(
      "\\bXLI$|
      \\bXSI$|
      [^^|\\b]CC$|
      [^^|\\b]GT$|
      \\bGTI$|
      \\bSE$|
      \\bHT$|
      \\bAUT$|
      \\bGL$|
      \\bTD$|
      \\bUNLIMITED$|
      \\bLIMITED$|
      \\bDE$|
      \\bHB$|
      \\bLS$|
      \\bSL$|
      \\bVAN$|
      \\bCNG$|
      \\bCAB$|
      \\bLE$|
      \\bLX$|
      \\bRS$|
      \\bDOOR$|
      \\bSC$|
      \\bTC$|
      \\bTDI$|
      \\bCDI$|
      \\bTS$|
      \\bAT$|
      \\bBUSINESS$|
      \\bEDITION$|
      \\bES$|
      \\bGLS$|
      \\bGLX$|
      \\bGS$|
      \\bGTS$|
      \\bSTATION$|
      \\bXL$|
      \\b2DHT$|
      \\bDL$|
      \\bMODEL$|
      \\bCS$|
      \\bGX$|
      \\bHD$|
      \\bSD$|
      \\bCL$|
      \\bFSI$|
      \\bGLI$|
      \\bGSI$|
      \\bGTC$|
      \\bGTX$|
      \\bLT$|
      \\bML$|
      \\bSLS$|
      \\bSV$|
      \\bSW$|
      \\bSX$|
      \\bTB$|
      \\bTCI$|
      \\bXR$|
      \\bAVO$|
      \\bCDI$|
      \\bCDTI$|
      \\bCI$|
      \\bCJ$|
      \\bCNG$|
      \\bCRDI$|
      \\bCUOPE$|
      \\bDB$|
      \\bDI$|
      \\bDPF$|
      \\bDT$|
      \\bDTS$|
      \\bDX$|
      \\bEX$|
      \\bFX$|
      \\bGF$|
      \\bGTE$|
      \\bGTA$|
      \\bHDI$|
      \\bIS$|
      \\bLD$|
      \\bTFSI$",
      " ",
      malli
    )}
  
  # poistetaan muutamia merkkikohtaisia stringejä
  malli <- ifelse(merkki == "OPEL", gsub("-.*$|STATION|SW|NOTCHBACK|GTC|NB", " ", malli), malli)
  malli <- ifelse(merkki == "ALFA ROMEO", gsub("ALFA", " ", malli), malli)
  malli <- ifelse(merkki == "PEUGEOT", gsub("FAMILIAL|BREAK", " ", malli), malli)
  malli <- ifelse(merkki == "BMW", gsub("(X|S)DRIVE.*$|ACTIVEHYBRID", "", malli),
           malli)
  malli <-
    ifelse(merkki == "VOLKSWAGEN", gsub("\\bSPORTSVAN\\b", " ", malli), malli)
  malli <- ifelse(merkki == "VOLVO", gsub("CROSS COUNTRY", " ", malli), malli)
  malli <- ifelse(merkki == "AUDI", gsub("SPORTBACK", " ", malli), malli)
  malli <- ifelse(merkki == "HONDA", gsub("AERODECK", " ", malli), malli)
  malli <- ifelse(merkki == "MERCEDES-BENZ", gsub("KOMBI", " ", malli), malli)
  malli <-
    ifelse(merkki == "SAAB",
      gsub(
        "VECTOR.*$|SPORTCOM.*$|SPORT|LINEAR.*$|AERO.*$|CD.*$|CSE.*$|SE.*$|ESTATE.*$", " ", malli), malli
    )
  
  malli <- ifelse(merkki == "MITSUBISHI", gsub("EVOLUTION.*$|STAR", " ", malli), malli)
  malli <- ifelse(merkki == "GAZ", gsub("WOLGA", "VOLGA",  malli), malli)
  malli <- ifelse(merkki == "GAZ", gsub("POPEDA", "POBEBDA", malli), malli)
  malli <- ifelse(merkki == "GAZ", gsub("TSHAIKA", "TSAIKA", malli), malli)
  malli <- clear(malli)
  
  malli <- gsub(" -.*$","",malli)
  malli <- clear(malli)
  
  return(malli)
}

####  Siivotaan mallit
#
  
## Korjataan mallit ja tehdään niistä "key" = välilyönnit pois...

mallit <- mutate(mallit,
  k.malli = puhdista.malli(merkki, k.malli),
  #malli = puhdista.malli(merkki, malli),
  k.malli.key = gsub("[[:space:]]", "", k.malli),
  malli.key = gsub("[[:space:]]", "", malli),
  k.malli.key = ifelse(k.malli.key == "" | is.na(k.malli.key), malli.key, k.malli.key),
  key = k.malli.key,
  k.malli = ifelse(k.malli == "" | is.na(k.malli), NA, k.malli)
)

# Haetaan yleisiä malleja jäljellejääneestä tavarasta...

small.limit.1 <- 5
small.limit.2 <- 50
key.bound <- list()
key.nonbound <- list()
key.nonbound.n <- list()
key.bound.n <- list()

mallit_matched <- mallit

# Haetaan merkeittäin yleisimpiä malliavaimia 

for (i in setdiff(unique(mallit_matched$merkki), "")) {
  print(i)
  Q <- filter(mallit_matched, k.malli.key != "" & merkki == i &
             !is.na(k.malli.key) & str_length(k.malli.key) >= 2) %>%
    group_by(merkki, k.malli.key) %>%
    summarise(N = sum(n)) %>%
    group_by(merkki) %>%
    filter(N >= small.limit.1) %>%
    select(merkki, k.malli.key, N) %>%
    mutate(l = length(k.malli.key))
  q <- Q$k.malli.key
  n <- Q$N
  if (length(q) > 0) {
    key.bound[i] <- paste0("\\b", q[order(-str_length(q))], "\\b") %>% paste(., collapse = "|")
    key.nonbound[i] <- paste0(q[order(-str_length(q))]) %>% paste(., collapse = "|")
    key.bound.n[i] <- paste0("\\b", q[order(-n)], "\\b") %>% paste(., collapse = "|")
    key.nonbound.n[i] <- paste0(q[order(-n)]) %>% paste(., collapse = "|")
    mallit_matched <- mutate(mallit_matched, key = ifelse(is.na(key) & merkki == i, str_match(malli.key, key.bound[[i]]), key))
    mallit_matched <- mutate(mallit_matched, key = ifelse(is.na(key) & merkki == i, str_match(gsub(" ", "", malli), key.nonbound[[i]]),
        key
      ))
  }
}

mallit_matched <- group_by(mallit_matched, merkki, key) %>% 
  mutate(N = sum(n)) %>% 
  ungroup

for (i in setdiff(names(key.nonbound), "")) {
  print(i)
  ix <- mallit_matched$N < small.limit.2 & mallit_matched$merkki == i
  m <- str_match(mallit_matched$key[ix], key.nonbound.n[[i]])
  mallit_matched$key[ix] <- m
}

mallit_matched <- filter(mallit_matched, !is.na(key))

modeltable <- group_by(mallit_matched, merkki, key) %>%
  mutate(l = str_length(k.malli),
         l = ifelse(is.na(l) | l == 0, 0, -as.numeric(N))) %>%
  arrange(l) %>%
  dplyr::slice(1) %>% ungroup %>%
  mutate(k.malli = ifelse(is.na(k.malli), key, k.malli)) %>%
  select(merkki, k.malli, key) %>% 
  ungroup

mallit.merkki.korjaus <-
  select(mallit_matched, merkki, kaupallinenNimi, mallimerkinta, key) %>%
  left_join(.,
            select(modeltable, merkki, key, k.malli),
            by = c("merkki", "key")) %>%
  select(-key) %>%
  filter(!is.na(merkki) & merkki != "" & !is.na(k.malli))

autot <- select(autot, -k.malli)
autot <- left_join(autot, mallit.merkki.korjaus, by = c("merkki", "kaupallinenNimi", "mallimerkinta"))

korjaus <- count(autot, merkkiSelvakielinen, mallimerkinta, kaupallinenNimi, merkki, k.malli) %>% ungroup

#korjaus.bak <- korjaus
### Korjataan yleisimpiä detaljeja

# RANGE-ROVERin kirjoitusasu
korjaus <- mutate(korjaus, k.malli = ifelse(k.malli == "RANGE ROVER", "RANGE-ROVER", k.malli))

# Mini Cooperin variantit
korjaus <- mutate(korjaus, k.malli = ifelse(merkki == "MINI" & grepl("COOPER", k.malli), "COOPER", k.malli))

# Porschen S erote lopussa
korjaus <-
  mutate(korjaus, k.malli = ifelse(merkki == "PORSCHE", gsub(" S$", "", k.malli), k.malli))

#Mazdan liimautunut numero, # Nissan Leaf, Astra Twin, Volvo T8 yms.

korjaus <- mutate(korjaus, k.malli = ifelse(merkki == "MAZDA", gsub("MAZDA", "", k.malli), k.malli))
korjaus <- mutate(korjaus, k.malli = ifelse(merkki == "NISSAN", gsub("LEAF 30KWH", "LEAF", k.malli), k.malli))
korjaus <- mutate(korjaus, k.malli = ifelse(merkki == "OPEL", gsub("ASTRA TWIN", "ASTRA", k.malli), k.malli))
korjaus <- mutate(korjaus, k.malli = ifelse(merkki == "VOLVO", gsub("XC90 T8", "XC90", k.malli), k.malli))

if (db_has_table(trafi.db$con, "mallitmerkkikorjaus"))
  db_drop_table(trafi.db$con, "mallitmerkkikorjaus")
as.data.frame(korjaus) %>% db_insert_into(trafi.db$con, "mallitmerkkikorjaus", .)

write.table(
  korjaus,
  file = full.path("mallitmerkkikorjaus.csv"),
  quote = FALSE,
  sep = "\t",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  append = FALSE
)
