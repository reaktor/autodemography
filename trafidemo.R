library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)
library(RColorBrewer)
library(lme4)
library(corrplot)
library()

##

attribute.stat <- function(autot, attr="merkki", base="kunta")
{
    group_by_(autot,base,attr) %>% 
    summarise(N.attr=n()) %>%
    ungroup %>% 
    group_by_(base) %>% mutate(N=sum(N.attr)) %>% ungroup
}

auto.rank <-function(autot, attr="merkki", cf.limit=Inf, r.limit=Inf)
{
    group_by_(autot,attr) %>% summarise(N=n()) %>% 
    ungroup %>% mutate(r=rank(-N,ties.method="max")) %>% 
    arrange(r) %>% mutate(cf=cumsum(N)/sum(N),df=N/sum(N))
}  

## Hae Postinumeroaluedata!

load("vaesto.RData")
trafi.db<- src_sqlite("trafi.db")

autot<-tbl(trafi.db, "henkiloautot") %>% select(-ends_with(".orig")) %>%
  filter(ryhma %in% c("Henkilö","Maasto") & data=="4.7") %>% 
  collect(n=Inf) 

autot<-mutate(autot,l.malli=k.malli,
              l.malli=ifelse(merkki=="BMW",substring(l.malli,1,1),l.malli),
              l.malli=ifelse(merkki=="MERCEDES-BENZ", str_match(l.malli,"^[0-9]{1,3}|^[A-Za-z]+"),l.malli),
              merkki.malli=paste(merkki,l.malli),
              pono.3 = str_pad(pono.3, 3, side="left", pad="0"),
              pono.2 = str_sub(pono.3, 1, 2)) 

m.pono.3<-mutate(autot,ika=as.numeric(difftime(date,as.Date(kayttoonottopvm),units="day")/365.25)) %>% 
  select(pono.3,ika,paikat,pituus,omamassa,leveys,iskutilavuus,leveys,kW,Co2,matkamittarilukema) %>% 
  group_by(pono.3) %>% summarise_all(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

m.pono.2<-mutate(autot,ika=as.numeric(difftime(date,as.Date(kayttoonottopvm),units="day")/365.25)) %>% 
  select(pono.2,ika,paikat,pituus,omamassa,leveys,iskutilavuus,leveys,kW,Co2,matkamittarilukema) %>% 
  group_by(pono.2) %>% summarise_all(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

m.kunta<-mutate(autot,ika=as.numeric(difftime(date,as.Date(kayttoonottopvm),units="day")/365.25)) %>% 
  select(kunta,ika,paikat,pituus,omamassa,leveys,iskutilavuus,leveys,kW,Co2,matkamittarilukema) %>% 
  group_by(kunta) %>% summarise_all(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

year.limit=2009

auto.statistic<-function(autot,attrs=c("merkki","merkki.malli","kayttovoima","kori","vari"))
{
  auto.stats<-list()
  for (a in attrs) auto.stats[[a]]<-auto.rank(autot,a) 
  return(auto.stats)
}

auto.stats <- auto.statistic(autot)
kartta.pono <- 
  
autot.data<-rbind(
  attribute.stat(filter(autot, kaytto=="Yksityinen" & kayttoonottoVuosi>year.limit) %>% 
                mutate(merkki=ifelse(merkki %in% auto.stats[["merkki"]][["merkki"]][1:80], merkki,"muumerkki")),
                         attr="merkki",
                         base="kunta") %>% rename(attr=merkki), 
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                              mutate(merkki.malli=ifelse(merkki.malli %in% auto.stats[["merkki.malli"]][["merkki.malli"]][1:300], merkki.malli,"muumalli")),
                            attr="merkki.malli",
                            base="kunta") %>% rename(attr=merkki.malli),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                   mutate(vari=ifelse(is.na(vari),"muuvari",vari)),
                 attr="vari",
                 base="kunta") %>% rename(attr=vari),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>%
                   mutate(kayttovoima=ifelse(is.na(kayttovoima),"muukayttovoima",kayttovoima)),
                 attr="kayttovoima",
                 base="kunta") %>% rename(attr=kayttovoima)
  
  ) %>% 
  
  mutate(r=N.attr/N)

pono.data.2<-rbind(
  attribute.stat(filter(autot, kaytto=="Yksityinen" & kayttoonottoVuosi>year.limit) %>% 
                   mutate(merkki=ifelse(merkki %in% auto.stats[["merkki"]][["merkki"]][1:80], merkki,"muumerkki")),
                 attr="merkki",
                 base="pono.2") %>% rename(attr=merkki), 
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                   mutate(merkki.malli=ifelse(merkki.malli %in% auto.stats[["merkki.malli"]][["merkki.malli"]][1:300], merkki.malli,"muumalli")),
                 attr="merkki.malli",
                 base="pono.2") %>% rename(attr=merkki.malli),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                   mutate(vari=ifelse(is.na(vari),"muuvari",vari)),
                 attr="vari",
                 base="pono.2") %>% rename(attr=vari),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>%
                   mutate(kayttovoima=ifelse(is.na(kayttovoima),"muukayttovoima",kayttovoima)),
                 attr="kayttovoima",
                 base="pono.2") %>% rename(attr=kayttovoima)
) %>% 
  mutate(r=N.attr/N)

pono.data.3<-rbind(
  attribute.stat(filter(autot, kaytto=="Yksityinen" & kayttoonottoVuosi>year.limit) %>% 
                   mutate(merkki=ifelse(merkki %in% auto.stats[["merkki"]][["merkki"]][1:80], merkki,"muumerkki")),
                 attr="merkki",
                 base="pono.3") %>% rename(attr=merkki), 
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                   mutate(merkki.malli=ifelse(merkki.malli %in% auto.stats[["merkki.malli"]][["merkki.malli"]][1:300], merkki.malli,"muumalli")),
                 attr="merkki.malli",
                 base="pono.3") %>% rename(attr=merkki.malli),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>% 
                   mutate(vari=ifelse(is.na(vari),"muuvari",vari)),
                 attr="vari",
                 base="pono.3") %>% rename(attr=vari),
  attribute.stat(filter(autot, kaytto=="Yksityinen"  & kayttoonottoVuosi>year.limit) %>%
                   mutate(kayttovoima=ifelse(is.na(kayttovoima),"muukayttovoima",kayttovoima)),
                 attr="kayttovoima",
                 base="pono.3") %>% rename(attr=kayttovoima)
) %>% 
  mutate(r=N.attr/N)


kunnat=unique(autot$kunta) 
ponot=filter(pono,kunta %in% kunnat) %>% .$pono %>% unique

z<-list()
for ( i in  auto.stats[["merkki"]][["merkki"]][1:50]){
 m <- glmer(N.attr ~ (1|kunta) + 1, offset=log(N), data=filter(kunta.merkki, merkki==i), family=poisson)
 z[[i]]<-exp(ranef(m)$kunta+summary(m)$coefficients[1] )
}

### kunta.merkki // 

ggplot(data=arrange(merge(filter(kartta.kunta, kunta %in% kunnat), 
                          dcast(kunta.data, kunta~attr,value.var="r",fill=0), by="kunta"),order),aes(x=long, y=lat))+
  geom_polygon(aes(fill=`Diesel`, group=kunta.id),colour=NA)+
scale_fill_gradientn(colours=brewer.pal(7,"YlOrBr"), values = NULL, space = "Lab", na.value = "white", 
                   guide = "colourbar")

###  / 
ggplot(data=arrange(merge(filter(kartta.pono, pono %in% ponot), dcast(pono.data.3, pono.3~attr,value.var="r",fill=0), by="pono.3"), order), aes(x=long, y=lat))+
  geom_polygon(aes(fill=`SAAB`, group=pono),colour=NA)+
  scale_fill_gradientn(colours=brewer.pal(7,"YlOrBr"), values = NULL, space = "Lab", na.value = "white", 
                       guide = "colourbar")

ggplot(data=arrange(merge(filter(kartta.pono, pono %in% ponot), dcast(pono.data, pono.2~attr,value.var="r",fill=0), by="pono.2"), order), aes(x=long, y=lat))+
  geom_polygon(aes(fill=`Valkoinen`, group=pono),colour=NA)+
  scale_fill_gradientn(colours=brewer.pal(7,"YlOrBr"), values = NULL, space = "Lab", na.value = "white", 
                       guide = "colourbar")

dcast(kunta.merkki,kunta~merkki,value.var="r",fill=0) %>% select(-kunta) %>% 
  cor(.,use="na.or.") %>%
  corrplot(., order="hclust", hclust.method="ward.D2",tl.cex=0.6,tl.col="black",method="shade")

X<-dcast(kunta.merkki,kunta~merkki,value.var="r",fill=0)
X.cor<-dcast(kunta.merkki,kunta~merkki,value.var="r",fill=0) %>% select(-kunta) %>% 
  cor(.,use="na.or.") 
dist.cor<-as.dist(sqrt((1-dcast(kunta.merkki,kunta~merkki,value.var="r",fill=0) %>% select(-kunta) %>% 
               cor(.,use="na.or."))))
Z<-data.frame(X.cor,tsne(dist.cor, perplexity=12, k=2, whiten=FALSE, max_iter=5000))
Z$merkki<-rownames(Z)
ggplot(data=Z,aes(x=X1,y=X2,label=merkki))+geom_text(size=2)

X<-dcast(.malli,kunta~merkki.malli,value.var="r",fill=0)
X.cor<-dcast(kunta.malli,kunta~merkki.malli,value.var="r",fill=0) %>% select(-kunta) %>% 
  cor(.,use="na.or.") 
dist.cor<-as.dist(sqrt((1-dcast(kunta.malli,kunta~merkki.malli,value.var="r",fill=0) %>% select(-kunta) %>% 
                          cor(.,use="na.or."))))
Z<-data.frame(X.cor,tsne(dist.cor, perplexity=20, k=2, whiten=FALSE, max_iter=5000))
Z$merkki<-rownames(Z)
ggplot(data=Z,aes(x=X1,y=X2,label=merkki))+geom_text(size=2)




corrplot(., order="hclust", hclust.method="ward.D",tl.cex=0.6,tl.col="black",method="shade")


         # Karttanimistä vain kunnat ja tyyppi 560 (kulmakunta), kuntanimi enemmistökielellä
         # vaikuttaa siltä että kuntanimistöstä puuttuu uusia kuntia?
         
         kartta.kuntanimet<-kartta.kuntanimet %>% 
           filter(paikkaTyyp %in% c(540,550,560)) %>% 
           group_by(paikkaID) %>% 
           arrange(kieliEnem) %>% slice(1)
         
         kuva<-arrange(merge(kartta.kunnat,kunta.data,by="kunta",all.x=TRUE),order)
         
         ## Piirretään kartta
         
         ggplot(data=kuva,aes(x=long, y=lat))+
           geom_polygon(aes(fill=N/Asukkaat, group=Kunta),colour=NA)+
           scale_fill_gradientn(colours=brewer.pal(9,"YlOrBr"), values = NULL, space = "Lab", na.value = "white", 
                                guide = "colourbar")+
           geom_text(data=kartta.kuntanimet,aes(x=paikkaYI,y=paikkaXP,label=knimiTekst),size=0.5)
         
         
         filter(kunta.m, !(kunta %in% c("x",NA))) %>% 
           ggplot(aes(x=ika_median,
                      y=Co2_mean,
                      color=kW_mean,label=kunta))+
           geom_point(size=3)+scale_colour_continuous(low="white",high="red")+geom_text(size=2,color="black")
         
         
         count(filter(autot,kayttoonottoVuosi>1999 & merkki=="PEUGEOT"),kunta,kayttoonottoVuosi,vari) %>% group_by(kunta,kayttoonottoVuosi) %>% mutate(N=sum(n),r=n/N) %>% filter(vari %in% c("Sininen","Ruskea","Keltainen","Vihreä","Musta","Valkoinen","Harmaa","Punainen","Hopea") & kunta %in% c("Helsinki","Turku","Espoo")) %>% ggplot(aes(x=kayttoonottoVuosi,y=r,color=kunta,group=kunta))+geom_line()+facet_wrap(~vari,scales="free")
         