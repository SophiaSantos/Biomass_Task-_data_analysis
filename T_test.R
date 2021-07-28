data<-read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/AA_comparison_plots_2.csv", sep=";", header=T)
rownames(data) = data[,1]#identificar as amostras com os dados da coluna 1
data=data[,-1]
data=as.data.frame(data)
data=as.numeric(data)

install.packages("nortest", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("genefilter", dependencies = TRUE)
library(nortest)
library(Hmisc)
library(genefilter)
#==================== T-test Fluxes =========================================#

Bsu_L <- data[,1]
Bsu_IS <- data[,9]
Bsu_T <- data[,5]
Bsu_L <- na.omit(Bsu_L)
Bsu_IS <- na.omit(Bsu_IS)
Bsu_T<- na.omit(Bsu_T)
Bsu_L <- as.numeric(Bsu_L)
Bsu_IS <- as.numeric(Bsu_IS)
Bsu_T <- as.numeric(Bsu_T)
a_Bsu <- table (Bsu)

Bsu_IS <- data[,3]
Bsu_IS <- na.omit(Bsu_IS)
Bsu_IS <- as.numeric(Bsu_IS)
a_Bsu_IS <- table (Bsu_IS)

Bsu_IST <- data[,4]
Bsu_IST <- na.omit(Bsu_IST)
Bsu_IST <- as.numeric(Bsu_IST)
a_Bsu_IST <- table (Bsu_IST)

#=============== NORMALITY TEST =====================

#Shapiro.test and ad.test ======== If p-value < 0.05 the samples do not follow a normal distribuition

shapiro.test(Bsu_L)
shapiro.test(Bsu_IS)
shapiro.test(Bsu_T)

ad.test(Bsu)
ad.test(Bsu_IS)
ad.test(Bsu_IST)

boxplot(Bsu, Bsu_IST)

#=================== Samples with same distribuition ( p < 0.05 implies x and y from different distributions) ====================

ks.test(Bsu, Bsu_IS)
ks.test(Bsu, Bsu_IST)

#================== Correlation test ( p < 0.05 implies x and y are independent) ======================================

cor.test(Bsu, Bsu_IS)
cor.test(Bsu, Bsu_IST)

# paired t-test ======== For normal samples
t.test(Bsu,Bsu_IS,alt="two.sided",paired=TRUE)
t.test(Bsu,Bsu_IST,alt="two.sided",paired=TRUE)

# wilcox.test ============ For no normal samples
#Conventionally, If the p-Value is less than significance level (ideally 0.05), reject the null hypothesis that both means are equal.
wilcox.test(Bsu_L, Bsu_IS, paired=TRUE) # both x and y are assumed to have similar shapes
wilcox.test(Bsu_L, Bsu_T, paired=TRUE) # both x and y are assumed to have similar shapes

#friedman.test

friedman.test(cbind(Bsu, Bsu_IS, Bsu_IST))

#==================== Biomass Article =========================================#

data = read.csv("C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/Model_comp.csv", sep=";", header=T)
rownames(data) = data[,1] #identificar as amostras com os dados da coluna 1
data = data[,-1]


# matrix of t-test
# mat : data.frame or matrix
# ... : further arguments to pass to the t.test function
multi.ttest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      ttest <- t.test(mat[,i], mat[, j], alternative = "two.sided",paired=TRUE)
      p.mat[i, j] <- p.mat[j, i] <- ttest$p.value
      
      
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  signif(p.mat,4)
}

result<-multi.ttest(data)
result

# matrix of wilcox-test
# mat : data.frame or matrix
# ... : further arguments to pass to the t.test function
multi.wtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  w.mat<- rep(NA, n)
    for (i in 2:n) {
      wtest<-wilcox.test(as.numeric(unlist(data[,25])), as.numeric(unlist(data[,i])), alternative = "two.sided", conf.level = 0.99 )
      w.mat[i] <- wtest$p.value
      
  }
  signif(w.mat,4)
}

result_w<-multi.wtest(data)
result_w


# matrix of correlation
# mat : data.frame or matrix
# ... : further arguments to pass to the t.test function
multi.ctest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  c.mat<- rep(NA, n)
  for (i in 1:n) {
    ctest<-cor(as.numeric(unlist(data[,1])), as.numeric(unlist(data[,i])), use="complete.obs")
    c.mat[i] <- ctest
  }
  signif(c.mat,4)
}

result_c<-multi.ctest(data)
result_c


y=colnames(data)
x_w=cbind(y, result_w)
x_c=cbind(y,result_c)
write.table(x_w, "C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/wtest_FD.csv", sep=";")
write.table(x_c, "C:/Users/Sophia Santos/Dropbox/Biomass Task/_Artigo/Ficheiros_R/cortest_FD.csv", sep=";")

t.test(as.numeric(unlist(data[,1])), as.numeric(unlist(data[,2])), alternative = "two.sided",paired=TRUE)

#correlation matrix

cor(as.numeric(unlist(data[,1])), as.numeric(unlist(data[,2])))


