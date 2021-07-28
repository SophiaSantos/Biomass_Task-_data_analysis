setwd("C:/Users/Sophia/Desktop/Biomass Task/Artigo_Biomassa/Ficheiros_R")
install.packages("devtools",dependencies = TRUE)
install.packages("ggbiplot", "vqv")
install.packages("ggbiplot",dependencies = TRUE)
install.packages("ggplot2",dependencies = TRUE)
library("metabolomicsUM")
install.packages("specmine",dependencies = TRUE)

install.packages("MASS",dependencies = TRUE)
install.packages("lattice",dependencies = TRUE)
install.packages("plotrix",dependencies = TRUE)
library(plotrix)

#import and process data

data = read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/macro_lit.csv", sep=";", header=T)
rownames(data) = data[,1] #identificar as amostras com os dados da coluna 1
data = data[,-1]
maxIndex = length(colnames(data))
ds.dados=as.matrix(data[,1:maxIndex-1])
ds.dados = apply(ds.dados,2,function (x) (x-mean(x)))
ds.dados <- abs(cor(t(ds.dados))) #cor Pearson
ds.dados = t(ds.dados)
ds.dados=as.numeric(ds.dados)
ds.metadados=as.vector(data[maxIndex])
ds=create_dataset(ds.dados,type = "undefined", metadata=ds.metadados, description="family")


#plot code
data = read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/AA_comparison_plots.csv", sep=";", header=T)
data = na.omit(data)
rownames(data) = data[,1] #identificar as amostras com os dados da coluna 1
data = data[,-1]
maxIndex = length(names(data))
data=data.matrix(data)
maxIndex2 = length(names(data))
for(i in 1:30){
  #numCol = (maxIndex+0.1)/2
  
  png(filename=paste("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/comp_",colnames(data)[i],".png", sep="_" ))
  #par(mfrow=c(numCol,numCol))
  par(mfrow=c(2,2))
  for(j in 31:60){
 #   if(i==j-22){
    if(i!=j){
     drawplot(data,i,j)
    }
  }
  dev.off()
}
  png(filename=paste("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/comp__",colnames(data)[i],".png", sep="_" ))
  #par(mfrow=c(numCol,numCol))
  par(mfrow=c(1,1))
  for(j in 41:60){
    if(i!=j){
    #if(i==j-4){ 
      drawplot(data,i,j)
    }
  }
  dev.off()
}
  
drawplot <- function(data, j,i){
  mod1=lm(data[,j]~data[,i])
  r2 = round(cor(data[,j],data[,i])^2,4)
  mylabel = bquote(italic(R)^2 == .(format(r2, digits = 4)))
  
  plot(data[,i], data[,j], main = paste(colnames(data)[i],"vs", colnames(data)[j], sep=" "),
       xlab = colnames(data)[i],
       ylab = colnames(data)[j], cex.lab=0.8)
       #xlab="", ylab="",
       
  mtext(mylabel, cex = 0.8)
  abline(mod1 , col="red")

}

#line information code

for(i in 1:22){
  for(j in 23:44){
    if(i==j-22){
      mod1=lm(data[,j]~data[,i])
      linearfit=coef(mod1)
      r2 = round(cor(data[,j],data[,i])^2,4)
      Line[i,]=c(linearfit[1],linearfit[2],r2)
      
    }
    
  }
  
}


#pca code
pca.ds = pca.analysis.dataset(ds)
data_macro_pca <- pca.scoresplot2D(ds, pca.ds, "family", pcas=c(1,2), ellipses = T, labels = T)
data_macro_pca <- data_macro_pca + theme_bw()
data_macro_pca

library(RColorBrewer)
display.brewer.all()
myColours <- brewer.pal(3,"Pastel2")

#hierarquical clustering code
cl.ds2 = clustering(ds, distance="euclidean", clustMethod="average")
dendcompletem <- dendrogram_plot_col2(ds, cl.ds2, classes.col="family", title=c(expression(italic('In silico')* 'Data')), lab.cex=1.2)


par()

#bar plots code
data = read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/JD_macro.csv", sep=";", header=T)
rownames(data) = data[,1] #identificar as amostras com os dados da coluna 1
data = data[,-1]
ds.dados=as.matrix(data)
barplot(ds.dados, main= "1-Jaccard Differences", ylab=expression(paste("1-Jaccard Differences" )), beside=TRUE, ylim=c(0,0.8),
        col=terrain.colors(4), cex.main=1.5, xpd=FALSE)
text(c(64.5,63.5,66.5),0.7,"*",cex=2)

#barplot(ds.dados, main= expression(paste("Specific growth rate, ", mu,  (h^-1) )) , ylab=expression(paste("Specific growth rate, ", mu,  (h^-1) )), beside=TRUE, 
        #col=terrain.colors(5), ylim=c(0,1), cex.main=1.5)
#legend(20.3, 210, c(expression(italic('In silico')* 'Amino Acids'),"Experimental Macromolecules","Experimental Amino Acids","Experimental"), cex=0.6, 
       #fill=terrain.colors(4))
barplot(ds.dados, main= expression(paste("Specific growth rate, ", mu,  (h^-1) )), ylab="Differences to Experimental data (%)", beside=TRUE, 
        col=terrain.colors(4))
legend(21, 200, c(expression(italic('In silico')* 'Amino Acids',"Experimental Amino Acids","Experimental Macromolecules","All experimental")), cex=0.7, 
       fill=terrain.colors(4))


#barplot with gap
newdata<-ds.dados
newdata[newdata>50]<-newdata[newdata>50]-51
barpos<-barplot(newdata,names.arg=colnames(newdata),main= "Building Block Sensitivity Analysis", ylab="Sum Squared Differences",
                ylim=c(0,300),beside=TRUE,col=terrain.colors(6),axes=FALSE)
axis(2,at=c(0,1,5,10,15),
     labels=c(0,1,5,130,135))
box()
axis.break(2,6,style="gap")
