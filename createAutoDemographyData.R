# Data 
# Data alunperin http://www.trafi.fi/tietopalvelut/avoin_data

source("~/Projects/autodemography/initTrafi.R")

# Ajetaan merkkien/mallien korjaustiedosto!! luodaan skriptillä createMallitMerkitkorjaus.R
# (Voidaan editoida käsin)
library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)
library(RColorBrewer)
library(lme4)
library(corrplot)
library(gganimate)

##

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)


i<-group_by(henkiloauto.historia %>% filter(ajoneuvonkaytto=="Yksityinen"), data, kuntanimi) %>% summarise(uusi=sum(rekisterivuodet<=0.25,na.rm=TRUE), N=n()) %>% ungroup %>% transmute(alue=kuntanimi, uusi=uusi/N, data) 
kartta(filter(i, !is.ahvenanmaa(alue) & data=="4.02"), aluejako="kartogrammi.kuntanimi", color.map="BrBG")

henkiloauto <- tbl(trafi.db,"henkiloauto_uniqcombos") %>% 
  select(-record.id, 
         -date, 
         -data, 
         -alue,
         -jarnro,
         -ajoneuvonkaytto,
         -matkamittarilukema,
         -kunta) %>% 
  collect(n=Inf) 

auto <- henkiloauto %>% 
  fix.auto %>%
  fix.merkki.malli %>% 
  fix.kori %>% 
  fix.co2 

rm(henkiloauto)

henkiloauto.historia<-tbl(trafi.db,"henkiloauto_historia_diff") %>%
  collect(n=Inf) 

a<-filter(henkiloauto.historia, data=="4.2") %>% 
  fix.auto.historia %>% 
  select(-alue) %>%
  left_join(select(auto, 
          -ajoneuvoryhma, 
          -variantti, 
          -versio,
          -yksittaisKayttovoima,
          -valmistenumero2,
          -ajonKorkeus,
          -ahdin,
          -ohjaamotyyppi,
          -vaihteidenLkm,
          -tyyppihyvaksyntanro,
          -korityyppi,
          -teknSuurSallKokmassa,
          -tieliikSuurSallKokmassa,
          -voimanvalJaTehostamistapa,
          -kaupallinenNimi,
          -N.combo), by="combo")

b<- mutate(a,
    kayttoika.V=(ymd(date)-ymd(kayttoonottopvm))/365,
    rekisteri.ika.V=(ymd(date)-ymd(ensirekisterointipvm))/365,
    kayttoika.V=ifelse(kayttoika.V >=0 & kayttoika.V < 110,kayttoika.V,NA),
    rekisteri.ika.V=ifelse(rekisteri.ika.V>=0,rekisteri.ika.V,NA))

y<-transmute(a %>% filter(ryhma %in% c("Henkilö","Maasto") & !is.ahvenanmaa(pono.3) &
                            !is.ahvenanmaa(kunta)),
             kayttoonottoVuosi,
             vuosi=ifelse(kayttoonottoVuosi < 1990, 1989, kayttoonottoVuosi),
             polttoaine=ifelse(polttoaine %in% c("Bensiini","Diesel","Sähkö"), polttoaine, "muupolttoaine"),
             kori,
             vari,
             malli=ifelse(merkki.l.malli %in% auto.stats[["merkki.l.malli"]][["merkki.l.malli"]][auto.stats[["merkki.l.malli"]][["N"]]>8000], merkki.l.malli, 
                          ifelse(merkki %in% auto.stats[["merkki"]][["merkki"]][auto.stats[["merkki"]][["N"]]>600], paste(merkki,"muu"), "muumerkki")),
             merkki=ifelse(merkki %in% auto.stats[["merkki"]][["merkki"]][auto.stats[["merkki"]][["N"]]>300], merkki, "muumerkki"),
             pono.3,
             pono.2=substr(pono.3,1,2),
             kuntanimi,
             ika=coalesce(kayttoika.V,rekisteri.ika.V),
             ikaluokka=ifelse(ika > 3,"> 3v","< 3v"),
             malliv=paste(malli,ikaluokka),
             merkkiv=paste(merkki,ikaluokka),
             Co2,
             matkamittarilukema,
             kmv=ifelse(ika < 3 & is.na(matkamittarilukema), NA, matkamittarilukema / ika),
             suurinNettoteho,
             date,
             stat="sum.N")

autodata <- c()

for (d in unique(y$date)) {
  print(d)
  for (a in c("pono.3", "pono.2", "kuntanimi")) {
    print(a)
    for (i in c("stat",
                "kori",
                "polttoaine",
                "vari",
                "malli",
                "merkki",
                "merkkiv",
                "malliv"))
      autodata <-
        rbind(
          autodata,
          attribute.count(mutate_(y, alue = a), attr = i, base = "alue") %>%
            rename_(M = i) %>% mutate(
              attr = i,
              aluejako = a,
              date = d
            )
        )
    for (i in c("Co2",
                "ika",
                "matkamittarilukema",
                "suurinNettoteho",
                "kmv"))
      autodata <-
        rbind(
          autodata,
          rename_(y, alue = a, z = i) %>% 
            group_by(alue) %>%
            summarise(N = mean(z, na.rm = TRUE)) %>% 
            mutate(
              attr = i,
              M = "mean",
              aluejako = a,
              date = d
            )
        )
  }
}

X<- 
  mutate(autodata, M=paste(attr,M,sep=".")) %>% 
  select(alue, aluejako, M, N) %>% filter(aluejako!="kunta") %>% spread(.,M,N,fill=0) %>% 
  select(aluejako, alue, 
         starts_with("merkki"), 
         starts_with("malli"), 
         starts_with("polttoaine."), 
         starts_with("vari."), 
         starts_with("kori."), 
         ends_with(".mean"),
         N=stat.sum.N) %>% 
  mutate_at(vars(starts_with("malli"),
                 starts_with("merkki"),
                 starts_with("polttoaine."), 
                 starts_with("vari."), 
                 starts_with("kori.")), funs(./N)) 

paavo.prosentit<- demografia$paavo$data %>%
  select(-starts_with("he_0")) %>% 
  select(-starts_with("he_1")) %>%
  select(-starts_with("he_2")) %>% 
  select(-starts_with("he_3")) %>% 
  select(-starts_with("he_4")) %>% 
  select(-starts_with("he_5")) %>%
  select(-starts_with("he_6")) %>% 
  select(-starts_with("he_7")) %>%
  select(-starts_with("he_8")) %>%
  select(-nimi,-kuntano) %>% 
  mutate(naiset.osuus=he_naiset/(he_miehet+he_naiset)) %>%
  mutate_at(vars(starts_with("ko_"),-ko_ika18y),funs(./ko_ika18y)) %>%
  mutate_at(vars(hr_pi_tul,hr_ke_tul,hr_hy_tul,hr_ovy), funs(./hr_tuy)) %>%
  mutate_at(vars(starts_with("pt_"),-pt_vakiy), funs(./pt_vakiy)) %>%
  mutate_at(vars(starts_with("tp_"),-tp_tyopy), funs(./tp_tyopy)) %>%
  mutate_at(vars(starts_with("te_"),-te_taly,-te_takk,-te_as_valj), funs(./te_taly)) %>%
  mutate_at(vars(starts_with("tr_"),-tr_kuty,-tr_ktu,-tr_mtu), funs(./tr_kuty)) %>%
  mutate_at(vars(starts_with("ra_"),-ra_raky,-ra_as_kpa), funs(./ra_raky)) %>% 
  mutate(aluejako=mapvalues(pono.level,c(1,2,3),c("pono.1","pono.2","pono.3"))) %>% 
  select(-pono.level) %>% rename(alue=pono) %>% mutate(vuosi=as.numeric(vuosi))

p<-left_join(select(X,-aluejako), paavo.prosentit, by="alue") 
names(p)<-gsub(" ","_",names(p))
p<-filter(p,vuosi==2017)

c<-cor(select(p,-N,-aluejako,-alue,-vuosi)) 

paavo.koodi<-demografia$paavo$vars$koodi
paavo.nimi<-demografia$paavo$vars$nimi
d<-as.data.frame.table(c) %>% 
  filter(Var1!=Var2 & abs(Freq)<0.999) %>%
  mutate(X1=mapvalues(as.character(Var1),paavo.koodi,paavo.nimi),
         X2=mapvalues(as.character(Var2),paavo.koodi,paavo.nimi))





m<-filter(auto.stats$merkki, N > 10000)$merkki 

i <- mutate(y,merkki=ifelse(merkki %in% m, merkki, "muu")) %>% 
  select(ika, merkki, kuntanimi, ajoneuvonkaytto, ryhma)
i<-attribute.count.N(i, attr=c("merkki"), base="ika") %>% mutate(osuus=N/sum.N) 

ggplot(filter(i,ika<15), aes(x=ika,y=osuus))+geom_line()+facet_wrap(~merkki)

kartta(filter(autodata, attr=="suurinNettoteho" & aluejako=="kuntanimi") %>%
         select(alue,ika=N,aluejako), aluejako="kuntanimi")

kartta(cut.quantile(select(p, hr_ktu, alue),"hr_ktu",c(0.025,0.975)), aluejako="pono.3")

kartta(cut.quantile(filter(autodata, attr=="ika" & aluejako=="pono.2"),"N",c(0.01,0.99)) %>% 
         select(alue,ika=N,aluejako), aluejako="pono.2")

kartta(filter(autodata, attr=="ika" & aluejako=="kuntanimi") %>% 
         select(alue,ika=N,aluejako), aluejako="kuntanimi")

p<-naivi.p(filter(autodata, attr=="ika" & aluejako=="pono.3"), "sum.N", c(0.01,0.99)) %>%
  select(alue,ika=N,aluejako)

c<-cut.quantile(filter(autodata, attr=="ika" & aluejako=="pono.3"),"N",c(0.01,0.99))

k<-select(demografia$kunta, Average.Age=`Keski-ikä, molemmat sukupuolet`, alue=kuntanimi,aika=vuosi) %>% filter(!is.na(Average.Age))
k<-left_join(k %>% filter(alue!="KOKO MAA"), k %>% filter(alue=="KOKO MAA") %>% transmute(aika,b=Average.Age), by="aika") %>%
  mutate(diff.from.average.age.years=Average.Age-b) 

p<-kartta.animaatio(select(k,Average.Age,aika,alue), aluejako="kuntanimi", color.map="RdBu", 
                    title.label="Average age by municipality. Year ")


ani.options(loop=TRUE,interval=.5,aniheight=200)
gganimate(p,"koe.gif")

p<-kartta.animaatio(select(demografia$kunta, Service=`Taloudellinen huoltosuhde`, alue=kuntanimi,aika=vuosi) %>% 
                      filter(!is.na(Service)), aluejako="kuntanimi")

p<-kartta.animaatio(select(demografia$kunta, Loans=`Lainakanta, euroa/asukas` , alue=kuntanimi,aika=vuosi) %>% 
                      filter(!is.na(Loans)), aluejako="kuntanimi")

p<-kartta.animaatio(select(demografia$kunta, Retired=`Eläkeläisten osuus väestöstä, %` , alue=kuntanimi,aika=vuosi) %>% 
                      filter(!is.na(Retired)), aluejako="kuntanimi")

p<-kartta.animaatio(select(demografia$kunta, People=`Väkiluku` , alue=kuntanimi,aika=vuosi) %>% 
                      filter(!is.na(People)), aluejako="kuntanimi")

###  / brewer.pal(7,"YlOrBr")

merkki.cor  <- select(p,-pono.3) %>% 
  cor(., use="na.or.complete") 

dist.cor.plot <- function (c, perplexity=10,max_iter=3000) {
  dist.cor<-as.dist(sqrt((1-c)))
  Z<-tsne(dist.cor, perplexity=perplexity, k=2, whiten=FALSE, max_iter=max_iter)
  return(dist.cor)}

ggplot(data=Z,aes(x=X1,y=X2,label=merkki))+geom_text(size=2)

corrplot(c, order="hclust", hclust.method="ward.D",tl.cex=0.6,tl.col="black",method="shade")

# Karttanimistä vain kunnat ja tyyppi 560 (kulmakunta), kuntanimi enemmistökielellä
# vaikuttaa siltä että kuntanimistöstä puuttuu uusia kuntia?

demog<-filter(demografia$pono.3,vuosi==2016) %>% rename(pono.3=pono)

autot.c<-cut.quantile(autot,names(select(autot,-pono.3,-sum.N)),c(0.01,0.99))

%>% 
  mutate_at(vars(-pono.3,-sum.N), funs(./sum.N)) 

dist.cor<-as.dist(sqrt((1-c)))

Z<-data.frame(merkki.cor,tsne(c, perplexity=10, k=2, whiten=FALSE, max_iter=3000))
Z$merkki<-rownames(Z)
ggplot(data=Z,aes(x=X1,y=X2,label=merkki))+geom_text(size=2)

corrplot(., order="hclust", hclust.method="ward.D",tl.cex=0.6,tl.col="black",method="shade")

# Karttanimistä vain kunnat ja tyyppi 560 (kulmakunta), kuntanimi enemmistökielellä
# vaikuttaa siltä että kuntanimistöstä puuttuu uusia kuntia?

demog<-filter(demografia$pono.3,vuosi==2016) %>% rename(pono.3=pono)

autot.c<-cut.quantile(autot,names(select(autot,-pono.3,-sum.N)),c(0.01,0.99))

%>% 
  mutate_at(vars(-pono.3,-sum.N), funs(./sum.N)) 


