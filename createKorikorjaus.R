#source("initTrafi.R")

source("korjaa.R")

library(tidyr)
library(Matrix)
library(tidytext)

trafi.db<- src_sqlite("trafi.db") 

autot <-tbl(trafi.db,"henkiloauto_viimeisin") %>% 
  select(data,jarnro,merkkiSelvakielinen,kaupallinenNimi,mallimerkinta,variantti,versio,korityyppi,N.id) %>%
  collect(n=Inf) %>%
  fix.merkit.mallit

###

malli<-group_by(autot,merkki,malli,mallimerkinta,korityyppi) %>% 
  summarise(n=n()) %>%
  dcast(merkki+malli+mallimerkinta~korityyppi,fun.aggregate=sum,value.var="n") 

malli$N<-select(malli,-merkki,-mallimerkinta,-malli,-`NA`) %>% rowSums
malli$N.tot<-select(malli,-merkki,-malli,-mallimerkinta,-N) %>% rowSums

malli<-rename(malli,na=`NA`) %>%
  mutate_at(names(select(malli,Avo:Viistoperä)), funs("p"=./N)) %>% 
  mutate(ix=row_number())

q<-select(malli,ends_with("_p"),ix) %>% 
  gather(.,korityyppi,p,`Avo_p`:`Viistoperä_p`,-ix) %>% 
  group_by(ix) %>% arrange(-p) %>% dplyr::slice(1) %>% 
  mutate(kori=gsub("_p$","",korityyppi), 
         kori=ifelse(is.nan(p),NA,kori), 
         p=ifelse(is.nan(p),0,p)) %>% 
  select(-korityyppi)

malli<-left_join(malli,q,by="ix") %>% 
  mutate_at(ends_with("_p"), function(x) ifelse(is.nan(x),0,x))

malli<-mutate(malli,key=mallimerkinta,
       key=mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
         ifelse(is.na(merkki),"",merkki), key, USE.NAMES=FALSE),
       key=mapply(function(x,y) gsub(x,"",y,ignore.case=TRUE), 
         ifelse(is.na(malli),"",malli), key, USE.NAMES=FALSE),
       key=toupper(key))

x=cast_sparse(s.tab(malli$ix, 
                    paste(malli$merkki,malli$malli,malli$key),
                    N=malli$N.tot, 
                    sep=" |-|,|\)|\(|") %>%
                rename(ix=id) %>%
                left_join(., select(malli,ix,kori,p,merkki),by="ix") %>% 
                mutate(word=as.character(word),
                       word=ifelse(is.na(word),"NA",word)),ix,word) 

param <- list("booster" = "gbtree", # tree
              "objective" = "multi:softprob",    # multiclass classification 
              "num_class" = length(unique(xclass)),    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              #"nthread" = 4,   # number of threads to be used 
              "max_depth" = 25,    # maximum depth of tree 
              "silent" = 0,
              "eta" = 0.1,    # step size shrinkage 
              #"gamma" = 0,    # minimum loss reduction 
              "subsample" = 0.7    # part of data instances to grow tree 
              #"colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              #"min_child_weight" = 1  # minimum sum of instance weight needed in a child 
)

malli.kori <- xgboost(param=param, 
                  data = x[!is.na(malli$kori), colSums(x)>1], 
                  label = as.numeric(as.factor(malli$kori[!is.na(malli$kori)]))-1,
                  sample_weight = malli$N[!is.na(malli$kori)],
                  nrounds=200, 
                  prediction=TRUE, 
                  verbose=TRUE)

# compute feature importance matrix
importance_matrix = xgb.importance(colnames(x), model=malli.kori)

p.est<-predict(malli.kori, x) %>%
  matrix(., nrow=length(unique(xclass)),
         ncol=length(.)/length(unique(xclass))) %>% 
  t(.) 

malli$kori.tree <- levels(as.factor(k))[max.col(p.est)]
malli$p.kori.tree <- apply(p.est,1,max)

kori.korjaus<-select(malli,-ends_with("_p"),-ix,-N,-key) %>% 
  rename(Puuttuu=na,p.kori.nn=p,kori.nn=kori)

## tallenna 

write.table(kori.korjaus, file="korikorjaus.csv",quote=FALSE,
            sep="\t",row.names=FALSE,fileEncoding="UTF-8",append=FALSE)



