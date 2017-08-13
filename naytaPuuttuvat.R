
trafi.db<- src_sqlite("trafi.db") 
autot.data <-tbl(trafi.db,"henkiloautot") %>% filter(data %in% c("4.2","4.7")) %>%
  collect(n=Inf)


n<-notmissing(a %>% filter(kayttoonottoVuosi<2017),
              misvalues=c("",NA,"x"),
              c("data","kayttoonottoVuosi")) %>% 
  mutate(N=1) %>% 
  mutate(kayttoonottoVuosi=ifelse(kayttoonottoVuosi<1960, 1959,
                                  kayttoonottoVuosi)) %>%
  group_by(kayttoonottoVuosi,data) %>% 
  summarise_each(funs(sum)) %>%
  mutate_each(funs(./N), -N, -kayttoonottoVuosi, -data) 

ggplot(data=melt(select(n,-N,-date),id.vars=c("data","kayttoonottoVuosi")), 
       aes(x=kayttoonottoVuosi,y=value,color="data"))+geom_line()+facet_wrap(~variable,scales="fixed")


