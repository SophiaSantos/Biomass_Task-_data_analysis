library(plyr)
install.packages("ggplot2")
install.packages("ggplot")
library(scales)
install.packages("devtools")
library(reshape)
library(phenotypicForest)
library(ggplot2)

#import and process data - NÃO ESQUECER de colocar "family" e "item" como cabeçalhos no data frame
data = read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/macro_average.csv", sep=";", header=T)
data = na.omit(data)


df<-melt.data.frame(data,c("family","item"),variable_name="score", direction = "outwards", familyLabels = TRUE,  spaceFamily = 0.5) # from wide to long
p<-polarHistogram(df,familyLabels=FALSE, binSize = 1,spaceItem = 0.2, spaceFamily = 1.2, innerRadius = 0.3,outerRadius = 1, guides = c(10, 20, 40, 80), alphaStart = -0.3,circleProportion = 0.8, direction = "inwards", normalised = TRUE)
print(p, main="Literature")

#=====Change colors=====

p<-polarHistogram(myData)
p+scale_fill_manual(values = c('V1'='red','V2'='blue', 'V3'='green', 'v4'='yellow', 'v5'='orange'))

