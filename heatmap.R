require(graphics); require(grDevices)
library(gplots)

data = read.csv("C:/Users/Sophia/Dropbox/DeYeast/Reactions_Yeast.csv", sep=";", header=T)
rownames(data) = data[,1] #identificar as amostras com os dados da coluna 1
data = data[,-1]
data=as.matrix(data)
distancet <- dist(data)
hclust_complete <- hclust(distancet, method = "complete")
dendcomplete <- as.dendrogram(hclust_complete)
plot(hclust_complete)
heatmap.2(data, col=rainbow(10, start=0, end=0.35), cexCol = 1.5, cexRow = 0.75, density.info = "none")
heatmap.2(data, col="heat.colors")

require(graphics); require(grDevices)
x  <- as.matrix(data)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
utils::str(hv) # the two re-ordering index vectors