setwd("C:\\Users\\Sophia Santos\\Dropbox\\Biomass Task\\_Artigo\\Ficheiros_R")
library(reshape2)
library(lattice)
library(ggplot2)

data_lit<-read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/macro_exp.csv", sep=";", header=T)
data_lit=data_lit[,-1]

dfmelt=melt(data_lit, measure.vars = 1:5)
dfmelt_2=melt(data_lit, measure.vars = 6:10)
dfmelt_3=melt(data_lit, measure.vars = 11:15)
dfmelt_4=melt(data_lit, measure.vars = 16:20)




p1 = ggplot(data, aes(x=Species, y=value, fill=variable), cex=0.3)+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="Macromolecules", y="Percentage")+
  scale_y_continuous(limits=c(0,2.5))+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1),legend.position="none")
p1

p2 = ggplot(dfmelt_2, aes(x=family, y=value, fill=variable), cex=1)+
  geom_boxplot()+
  facet_grid(.~variable)+
  scale_y_continuous(limits=c(0,0.3))+
  labs(x="Amino Acids", y="Percentage")+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1),legend.position="none")
  p2+theme_bw()

p3 = ggplot(dfmelt_3, aes(x=family, y=value, fill=variable), cex=1)+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="Amino Acids", y="Percentage")+
  scale_y_continuous(limits=c(0,0.3))+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1),legend.position="none")
  p3+theme_bw()

p4 = ggplot(dfmelt_4, aes(x=family, y=value, fill=variable), cex=1)+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="Amino Acids", y="Percentage")+
  scale_y_continuous(limits=c(0,0.3))+
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1),legend.position="none")
  p4+theme_bw()


multiplot(p1+theme_bw(), p2+theme_bw(), p3+theme_bw(), p4+theme_bw(), cols=2)



#==========================
if(!require(dplyr)){install.packages("dplyr")}
if(!require(FSA)){install.packages("FSA")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}

data_lit<-read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/macro_lit.csv", sep=";", header=T)



dfmelt=melt(data, measure.vars = 1:6)
data=na.omit(dfmelt)

with(data,correlations_dataset(data))
ped.stats <- statsBy(dfmelt,"variable", cor=TRUE)
print(ped.stats, short=FALSE)
lowerMat(ped.stats$pbg)

