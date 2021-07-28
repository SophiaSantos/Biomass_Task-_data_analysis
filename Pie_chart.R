plot(1:5,type="n",main="Biomass Composition",xlab="",ylab="",axes=FALSE)#empty plot
require(plotrix)
big_lab <- c("Protein","DNA","RNA","Lipids","Carbohydrates")
main_pie <- floating.pie(3,3,c(58,42), col=heat.colors(2),radius=1)#your big pie
text(c(2.9,3.2),c(3.8,2.2),c("Adopted","Experimental"), cex=0.9)
#here are your small pies with labels using plotrix functions

small_lab <- c("Amino Acids")
A <- floating.pie(4.3,4.5,c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), radius=0.2)
pie.labels(3.7,5,A,small_lab,border=F,radius=0.3,cex=0.8)


plot(1:5,type="n",main="Floating Pie test",xlab="",ylab="",axes=FALSE)
box()
polygon(c(0,0,5.5,5.5),c(0,3,3,0),border="#44aaff",col="#44aaff")
floating.pie(1.7,3,c(2,4,4,2,8),radius=0.5,
             col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
floating.pie(3.1,3,c(1,4,5,2,8),radius=0.5,
             col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
floating.pie(4,1.5,c(3,4,6,7),radius=0.5,
             col=c("#ff0066","#00cc88","#44bbff","#8000ff"))
draw.circle(3.9,2.1,radius=0.04,col="white")
draw.circle(3.9,2.1,radius=0.04,col="white")
draw.circle(3.9,2.1,radius=0.04,col="white")
draw.circle(4,2.3,radius=0.04,col="white")
draw.circle(4.07,2.55,radius=0.04,col="white")
draw.circle(4.03,2.85,radius=0.04,col="white")
text(c(1.7,3.1,4),c(3.7,3.7,3.7),c("Pass","Pass","Fail"))
# type of bullying by sex as superimposed pies
plot(0,xlim=c(-1,1),ylim=c(-1,1),type="n",axes=FALSE,
     main="Type of bullying by sex (Li, 2006)")
bc<-floating.pie(0,0,c(90,129,45,67),radius=1,col=2:5)
pie.labels(0,0,bc,c("Physical\nbully","Physical\nvictim",
                    "Cyber\nbully","Cyber\nvictim"))
bc<-floating.pie(0,0,c(53,37,70,59,29,16,33,34),
                 radius=0.7,col=c("lightblue","pink"))
pie.labels(0,0,bc,radius=0.5,labels=rep(c("M","F"),4))

