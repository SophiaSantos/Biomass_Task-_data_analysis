# fake data for polarHistogram()
# Christophe Ladroue
library(plyr)
library(ggplot2)
devtools::install_github("chrislad/phenotypicForest")
library(reshape)
library(phenotypicForest)

# a little helper that generates random names for families and items.
randomName<-function(n=1,syllables=3){
  vowels<-c("a","e","i","o","u","y")
  consonants<-setdiff(letters,vowels)
  replicate(n,
            paste(
              rbind(sample(consonants,syllables,replace=TRUE),
                    sample(vowels,syllables,replace=TRUE)),
              sep='',collapse='')
  )
}

set.seed(42)

nFamily<-20
nItemPerFamily<-sample(1:6,nFamily,replace=TRUE)
nValues<-3

df<-data.frame(
  family=rep(randomName(nFamily),nItemPerFamily),
  item=randomName(sum(nItemPerFamily),2))

df<-cbind(df,as.data.frame(matrix(runif(nrow(df)*nValues),nrow=nrow(df),ncol=nValues)))

df<-melt(data,c("Family","Item"),variable_name="score") # from wide to long
p<-polarHistogram(df,familyLabel=FALSE)
print(p)
