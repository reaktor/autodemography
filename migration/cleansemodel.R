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
  malli <- clear(malli)
  
  # poistetaan GT, GTX yms. tyyppisiä sanoja merkintöjä stringin lopusta
  if(TRUE) {
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
  
  
  words <- function(txt, n=1) paste0(stringr::word(txt, 1:n), collapse=" ")
    
  malli <- clear(malli)    
  
  malli <- clear(ifelse(merkki == "ALFA ROMEO", gsub("ALFA", "", malli), malli))
  malli <- clear(ifelse(merkki == "ALFA ROMEO", words(malli, 1), malli))
  
  #malli <- clear(ifelse(merkki == "AUDI", gsub("SPORTBACK", " ", malli), malli))
  malli <- ifelse(merkki=="AUDI", gsub("(?<=A)( +)(?=[1-8])", "", malli, perl = TRUE), malli)
  malli <- ifelse(merkki=="AUDI", gsub("(?<=S)( +)(?=[1-8])", "", malli, perl = TRUE), malli)
  malli <- clear(ifelse(merkki == "AUDI", words(malli, 1), malli))
  
  malli <- ifelse(merkki == "BMW", gsub("(X|S)DRIVE.*$|ACTIVEHYBRID|ACTIVE|IPERFORMANCE|GRAN|EFFICIENT DYNAMICS|ROADSTER", "", malli),
                  malli)
  malli <- clear(ifelse(merkki == "BMW", word(malli, 1), malli))
  
  malli <- clear(ifelse(merkki == "BUICK", words(malli, 2), malli))
  malli <- clear(ifelse(merkki == "CADILLAC", words(malli, 2), malli))
  malli <- clear(ifelse(merkki == "CITROEN", word(malli, 2), malli))
  malli <- clear(ifelse(merkki == "CITROEN", gsub("ELECTRIQUE|BREAK","", .)))
  malli <- clear(ifelse(merkki == "DAIMLERCHRYSLER", words(malli, 2), malli))
  malli <- clear(ifelse(merkki == "DODGE", words(malli, 1), malli))
  malli <- clear(ifelse(merkki == "VOLVO", words(malli, 1), malli))
  
  # poistetaan muutamia merkkikohtaisia stringejä
  malli <- ifelse(merkki == "OPEL", gsub("-.*$|STATION|SW|NOTCHBACK|GTC|NB", " ", malli), malli)
  malli <- ifelse(merkki == "PEUGEOT", gsub("FAMILIAL|BREAK", " ", malli), malli)
  malli <-
    ifelse(merkki == "VOLKSWAGEN", gsub("\\bSPORTSVAN\\b", " ", malli), malli)
  #malli <- ifelse(merkki == "VOLVO", gsub("CROSS COUNTRY|T8 TWIN ENGINE|TWIN ENGINE", " ", malli), malli)

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