setwd("~/GitHub/regeringsgrundlag")

require(quanteda)
require(dplyr)
require(ggplot2)
require(magrittr)
library(readtext)

#import texts
rgcorpus <- corpus(readtext("txt/*.txt"))

#clean
rgcorpus <- tokens(rgcorpus,what="word",remove_punct=T,remove_separator=T) 

#summarize
rgsum<-rgcorpus %>% 
  summary() %>% 
  as.data.frame() %>% 
  extract(1:14,) %>%
  ungroup()

#get year
rgsum<-rgsum %>% 
  transmute(year=as.numeric(gsub("\\D","",Var1)),
            words=as.numeric(as.character(Freq)),
            blaablok=ifelse(year %in% c(2001:2010,2015:2016),1,0))

#plot
ggplot(rgsum,aes(x=year,y=words)) +
  geom_line() +
  geom_point(aes(color=factor(blaablok)),size=3) +
  theme_bw() +
  labs(x="År",y="Antal ord") +
  scale_color_manual(values=c("red","blue")) +
  theme(legend.position="none")

ggsave("wordplot.png",width=10,height=8)
