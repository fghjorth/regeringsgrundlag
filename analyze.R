setwd("~/Documents/GitHub/regeringsgrundlag/")

require(readtext)
require(quanteda)
require(dplyr)
require(ggplot2)
require(magrittr)

#import texts
rgtxts <- readtext(list.files("txt",full.names = T))
rgcorpus<-corpus(rgtxts)

#clean
rgtoks<-tokens(rgcorpus,remove_punct = T,remove_numbers = T)

#summarize
rgsum<-rgtoks %>% 
  summary() %>% 
  as.data.frame() %>% 
  extract(1:15,) %>% 
  ungroup()

#get year
rgsum2<-rgsum %>% 
  transmute(year=as.numeric(gsub("\\D","",Var1)),
            words=as.numeric(as.character(Freq)),
            blaablok=ifelse(year %in% c(2001:2010,2015:2016),1,0),
            blaablok2=ifelse(year==2022,2,blaablok))

#plot
ggplot(rgsum2,aes(x=year,y=words)) +
  geom_line() +
  geom_point(aes(color=factor(blaablok2)),size=3) +
  theme_bw() +
  labs(x="År",y="Antal ord") +
  scale_color_manual(values=c("red","blue","purple")) +
  theme(legend.position="none")

ggsave("wordplot.png",width=8,height=7)
