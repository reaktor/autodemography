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
library(ggiraph)

##

trafi.db<- src_sqlite(paste(working.directory,"/trafi.db",sep=""), create=FALSE)

#group_by(henkiloauto.historia %>% filter(ajoneuvonkaytto=="Yksityinen"), data, kuntanimi) %>% summarise(uusi=sum(rekisterivuodet<=0.25,na.rm=TRUE), N=n()) %>% ungroup %>% transmute(alue=kuntanimi, uusi=uusi/N, data) 
#kartta(filter(i, !is.ahvenanmaa(alue) & data=="4.02"), aluejako="kartogrammi.kuntanimi", color.map="BrBG")

henkiloauto <- tbl(trafi.db,"henkiloauto_uniqcombos") %>% 
  collect(n=Inf) %>% 
  mutate(merkki.orig=merkkiSelvakielinen, malli.orig=kaupallinenNimi) %>%
  fix.auto %>%
  fix.merkki.malli %>% 
  fix.kori %>% 
  fix.co2 %>%
  select(combo,  N.combo, kayttoonottoVuosi, ensirekVuosi, ajoneuvoluokka, ajoneuvoryhma, vari, ryhma, korityyppi, kori.orig, kori.est, kori, kayttovoima, polttoaine, omamassa, 
         iskutilavuus, suurinNettoteho, sahkohybridi, merkki.orig, malli.orig,
         mallimerkinta, merkki, malli, l.malli,  merkki.l.malli, Co2.orig, Co2.modelled, Co2)

group_by(henkiloauto, kori.orig=toupper(kori.orig), kori.est) %>% summarise(N=sum(N.combo)) %>% spread(.,key=kori.orig,N) %>% View

filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig<20)) %>% mutate(d=abs(Co2.orig-Co2.modelled)) %>% ggplot(aes(x=Co2.orig,y=d))+geom_smooth()

filter(henkiloauto, !(polttoaine != "Sähkö" & Co2.orig<20)) %>% mutate(d=abs(Co2.orig-Co2.modelled), D=abs(Co2.orig-Co2.modelled)/(Co2.modelled)) %>% .$D  %>% 
  quantile(.,c(.95,.99,1), na.rm=TRUE)

mean(abs(henkiloauto$Co2.orig-henkiloauto$Co2.modelled),na.rm=TRUE)

henkiloauto.historia <-tbl(trafi.db,"henkiloauto_historia_diff") %>% 
  select(-km.diff) %>%
  collect(n=Inf) 

henkiloauto.historia <- left_join(henkiloauto.historia, select(henkiloauto, combo, kayttoonottoVuosi, ensirekVuosi, vari, ryhma, kori, polttoaine, 
                                               suurinNettoteho, merkki, merkki.l.malli, Co2), by="combo")

henkiloauto.historia<-mutate(henkiloauto.historia, ryhma=plyr::mapvalues(ryhma, c("Henkilö","Maasto"), c("Normaali","Normaali"))) 

s<-auto.stat(filter(henkiloauto.historia, ryhma=="Normaali"))

henkiloauto.historia<-select(henkiloauto.historia, 
                             -katsastus.Q, 
                             -matkamittari.date.laatu, 
                             -ensirekisterointipvm, 
                             -kayttoonottopvm,
                             -ensirekVuosi) %>%

mutate(
          uusi=ifelse(kayttovuodet<=0.25,T,F),
          polttoaine=ifelse(polttoaine %in% c("Bensiini","Diesel","Sähkö"), polttoaine, "muupolttoaine"),
          merkki.orig=merkki,
          malli=ifelse(merkki.l.malli %in% s$merkki.l.malli$merkki.l.malli[1:150], merkki.l.malli, "muumalli"),
          merkki=ifelse(merkki %in% s$merkki$merkki[1:80], merkki, "muumerkki"))
  ungroup


henkiloauto.historia<-mutate(henkiloauto.historia, pono.2=str_sub(pono.3,1,2),
                             rekisteriero.v=kayttovuodet-rekisterivuodet)

merkkidata.kunta<-filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi)) %>% 
  count.normalized(., attr="merkki", base=c("data","kuntanimi","ryhma","ajoneuvonkaytto")) %>% 
  transmute(alue=kuntanimi, ryhma, ajoneuvonkaytto, data, merkki, N, sum.N) %>% 
  spread(.,key=merkki,value=N, fill=0)

merkkidata.pono.2<-filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi)) %>% 
  count.normalized(., attr="merkki", base=c("data","pono.2","ryhma","ajoneuvonkaytto")) %>% 
  transmute(alue=pono.2, ryhma, ajoneuvonkaytto, data, merkki, N, sum.N) %>% 
  spread(.,key=merkki,value=N, fill=0)

merkkidata.pono.3<-filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi)) %>% 
  count.normalized(., attr="merkki", base=c("data","pono.3","ryhma","ajoneuvonkaytto")) %>% 
  transmute(alue=pono.3, ryhma, ajoneuvonkaytto, data, merkki, N, sum.N) %>% 
  spread(.,key=merkki,value=N, fill=0)

cont.data<-filter(henkiloauto.historia, !is.ahvenanmaa(kuntanimi)) %>% 
  group_by(kuntanimi, data) %>% summarise(uusi=mean(uusi, na.rm=TRUE),
                                          Co2=mean(Co2, na.rm=TRUE),
                                          km=mean(km.per.kayttovuosi, na.rm=TRUE),
                                          km.diff=mean(km.diff.vuosi, na.rm=TRUE),
                                          auton.ika=mean(kayttovuodet, na.rm=TRUE),
                                          kW=mean(suurinNettoteho,na.rm=TRUE), N=n()) %>% 
  ungroup

z<-select(filter(merkkidata.pono.3,ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali" & data=="4.02"), alue, sum.N, `ALFA ROMEO`:WARTBURG) %>% 
  mutate_at(vars(`ALFA ROMEO`:WARTBURG), function(x) x / .$sum.N)

x<-select(filter(merkkidata.pono.3 %>% 
                   mutate(pono.2=str_sub(alue,1,2)), 
                 ajoneuvonkaytto=="Yksityinen" & ryhma=="Normaali" & data=="4.02"), 
          pono.2,pono.3=alue, sum.N, `ALFA ROMEO`:WARTBURG) %>% filter(!is.na(pono.2))

w<-lme.binom.p2(x,"pono.2","pono.3",N="sum.N")



kartta(select(w,alue=pono.3,TOYOTA),aluejako="pono.3")


p<-left_join(select(w,-aluejako), paavo.prosentit, by="alue") 
names(p)<-gsub(" ","_",names(p))
p<-filter(p,vuosi == 2017)

c<-cor(select(w,is.numeric)) 


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

merkki.cor <- select(merkkidata,-alue,-data) %>% 
  cor(., use="na.or.complete") 

dist.cor <- sqrt(1-merkki.cor)

tsne(dist.cor, perplexity=10, k=2, max_iter=8000, whiten=FALSE) %>%
  data.frame(.) %>% transmute(x=X1,y=X2,merkki=rownames(dist.cor)) %>% ggplot(., aes(x=x,y=y,label=merkki))+geom_text(size=4)

corrplot(merkki.cor, order="hclust", hclust.method="ward.D",tl.cex=0.6,tl.col="black",method="shade")

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


