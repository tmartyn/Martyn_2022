############ Current - DF and merging #############
Sorted.Current<-Stepwat.Sorted.Current
#Sorted.Current$Bmass.median.C<-(Sorted.Current$Bmass.med1+Sorted.Current$Bmass.med2)/2

##### perennial cool and warm season grasses
#PCG.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="p.cool.grass")]
#PWG.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="p.warm.grass")]
#PG.min.C<-PCG.min.C+PWG.min.C

#PCG.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="p.cool.grass")]
#PWG.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="p.warm.grass")]
#PG.max.C<-PCG.max.C+PWG.max.C

PCG.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="p.cool.grass")]
PWG.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="p.warm.grass")]
#PG.median.C<-PCG.median.C+PWG.median.C


##### merge perennial forbs
#PCF.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="p.cool.forb")]
#PWF.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="p.warm.forb")]
#PF.min.C<-PCF.min.C+PWF.min.C

#PCF.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="p.cool.forb")]
#PWF.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="p.warm.forb")]
#PF.max.C<-PCF.max.C+PWF.max.C

PCF.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="p.cool.forb")]
PWF.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="p.warm.forb")]
PF.mean.C<-PCF.mean.C+PWF.mean.C

##### merge annual forbs
#ACF.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="a.cool.forb")]
#AWF.min.C<-Sorted.Current$Bmass.min[which(Sorted.Current$RGroup=="a.warm.forb")]
#AF.min.C<-ACF.min.C+AWF.min.C

#ACF.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="a.cool.forb")]
#AWF.max.C<-Sorted.Current$Bmass.max[which(Sorted.Current$RGroup=="a.warm.forb")]
#AF.max.C<-ACF.max.C+AWF.max.C

ACF.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="a.cool.forb")]
AWF.mean.C<-Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="a.warm.forb")]
AF.mean.C<-ACF.mean.C+AWF.mean.C

############## 45.50 - DF and merging #############
Sorted.45.50<-Stepwat.Sorted[which(Stepwat.Sorted$Period=="d50yrs" & Stepwat.Sorted$RCP=="RCP45"),]
#Sorted.45.50$Bmass.median<-(Sorted.45.50$Bmass.med1+Sorted.45.50$Bmass.med2)/2

##### merge into larger groups
 PCG.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="p.cool.grass")]
 PWG.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="p.warm.grass")]
# PG.min.45.50<-PCG.min.45.50+PWG.min.45.50
# 
 PCG.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="p.cool.grass")]
 PWG.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="p.warm.grass")]
# PG.max.45.50<-PCG.max.45.50+PWG.max.45.50
# 
 PCG.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.cool.grass")]
 PWG.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.warm.grass")]
# PG.median.45.50<-PCG.median.45.50+PWG.median.45.50

##### perennial forbs
PCF.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="p.cool.forb")]
PWF.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="p.warm.forb")]
PF.min.45.50<-PCF.min.45.50+PWF.min.45.50

PCF.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="p.cool.forb")]
PWF.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="p.warm.forb")]
PF.max.45.50<-PCF.max.45.50+PWF.max.45.50

PCF.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.cool.forb")]
PWF.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.warm.forb")]
PF.mean.45.50<-PCF.mean.45.50+PWF.mean.45.50

##### annual forbs
ACF.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="a.cool.forb")]
AWF.min.45.50<-Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="a.warm.forb")]
AF.min.45.50<-ACF.min.45.50+AWF.min.45.50

ACF.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="a.cool.forb")]
AWF.max.45.50<-Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="a.warm.forb")]
AF.max.45.50<-ACF.max.45.50+AWF.max.45.50

ACF.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="a.cool.forb")]
AWF.mean.45.50<-Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="a.warm.forb")]
AF.mean.45.50<-ACF.mean.45.50+AWF.mean.45.50






####### make figures ######
## sagebrush
par(mar=c(4,4.5,4,1)+0.1)
plot(Sorted.45.50$Year[1:300],Sorted.45.50$Bmass.median[which(Sorted.45.50$RGroup=="sagebrush")],
     lwd=2,xlab="",ylim=c(0,940),xlim=c(0,300),ylab=expression("Biomass (g/m"^2*")"),
     type="l",cex.lab=1.2,cex.axis=1.2,col="white",bty="l",lab=c(12,5,7))
x=c(1:300,rev(1:300))
y1=c(Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="sagebrush")],
     rev(Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="sagebrush")]))
cols="gray60"
polygon(x,y1,col=alpha(cols,0.75),border=NA)

points(Sorted.45.50$Year[1:300],
       Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="sagebrush")],lwd=2,type='l',col='black')
points(Sorted.Current$Year[1:300],
       Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="sagebrush")],lwd=1,type='l',lty=1,col='black')
polygon(sage.coords,col="#eeeeeeaa",border="NA")
# legend(-3,501,
#        c("Future sagebrush mean","Current sagebrush mean","Future sagebrush range",
#          "Future other shrubs mean","Current other shrubs mean","Future other shrubs range"),
#        lwd=c(3,1.5,10,3,1.5,10),
#        col=c("black","black","gray60","blue","blue","skyblue"),
#        bty="n",ncol=2,cex=1)
par(xpd=NA)
text(45,1040, "a) Mid-century: Sagebrush",cex=1.8)



#### Grasses ####
par(mar=c(4,4.5,4,1)+0.1)
plot(Sorted.45.50$Year[1:300],Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.cool.grass")],lwd=2,xlab="",
     ylim=c(0,85),xlim=c(0,300),ylab=expression("Biomass (g/m"^2*")"),type="l",cex.lab=1.2,cex.axis=1.2,col="white",bty="l",lab=c(12,5,7))
x<-c(1:300,rev(1:300))
y2<-c(PCG.min.45.50,rev(PCG.max.45.50))
y3<-c(PWG.min.45.50,rev(PWG.max.45.50))
y4<-c(Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="a.cool.grass")],rev(Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="a.cool.grass")]))
cols2<-"palegreen"
cols3<-"lightgoldenrod1"
cols4<-"plum1"
polygon(x,y2,col=alpha(cols2,0.75),border=NA)
polygon(x,y3,col=alpha(cols3,0.75),border=NA)
polygon(x,y4,col=alpha(cols4,0.75),border=NA)
points(Sorted.45.50$Year[1:300],PCG.mean.45.50,lwd=2,type='l',col='seagreen4')
points(Sorted.45.50$Year[1:300],PWG.mean.45.50,lwd=2,type='l',col='darkgoldenrod')
points(Sorted.45.50$Year[1:300],Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="a.cool.grass")],lwd=2,type='l',col='mediumpurple4')
points(Sorted.Current$Year[1:300],PCG.mean.C,lwd=1,type='l',lty=1,col="seagreen4")
points(Sorted.Current$Year[1:300],PWG.mean.C,lwd=1,type='l',lty=1,col='darkgoldenrod')
points(Sorted.Current$Year[1:300],Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="a.cool.grass")],lwd=1,type='l',lty=1,col='mediumpurple4')
polygon(grass.coords,col="#eeeeeeaa",border="NA")
# legend(-4,160,c("Future C3PG mean","Current C3PG mean","Future C3PG range",
#                 "Future C4PG mean","Current C4PG mean","Future C4PG range",
#                 "Future C3AG mean","Current C3AG mean","Future C3AG range"),
#        lwd=c(3,1.5,10,3,1.5,10,3,1.5,10),
#        col=c("seagreen4","seagreen4","seagreen1","darkgoldenrod","darkgoldenrod","lightgoldenrod1","mediumpurple4","mediumpurple4","plum1"),
#        bty="n",ncol=3,cex=1)
par(xpd=NA)
text(45, 95, "b) Mid-century: Grasses",cex=1.8)

#### Forbs ####
par(mar=c(4,4.5,4,1)+0.1)
plot(Sorted.45.50$Year[1:300],Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="p.cool.forb")],lwd=2,xlab="",
     ylim=c(0,50),xlim=c(0,300),ylab=expression("Biomass (g/m"^2*")"),type="l",cex.lab=1.2,cex.axis=1.2,col="white",bty="l",lab=c(12,5,7))
x=c(1:300,rev(1:300))
y2=c(PF.min.45.50,rev(PF.max.45.50))
y3=c(AF.min.45.50,rev(AF.max.45.50))
y4=c(Sorted.45.50$Bmass.min[which(Sorted.45.50$RGroup=="shrub")],
     rev(Sorted.45.50$Bmass.max[which(Sorted.45.50$RGroup=="shrub")]))
cols2="indianred1"
cols3="orange"
cols4="skyblue"
polygon(x,y2,col=alpha(cols2,0.75),border=NA)
polygon(x,y3,col=alpha(cols3,0.75),border=NA)
polygon(x,y4,col=alpha(cols4,0.75),border=NA)
points(Sorted.45.50$Year[1:300],PF.mean.45.50,lwd=2,type='l',col='darkred')
points(Sorted.45.50$Year[1:300],AF.mean.45.50,lwd=2,type='l',col='chocolate3')
points(Sorted.45.50$Year[1:300],
       Sorted.45.50$Bmass.mean[which(Sorted.45.50$RGroup=="shrub")],lwd=2,type='l',col='navyblue')
points(Sorted.Current$Year[1:300],PF.mean.C,lwd=1,type='l',lty=1,col='darkred')
points(Sorted.Current$Year[1:300],AF.mean.C,lwd=1,type='l',lty=1,col='chocolate3')
points(Sorted.Current$Year[1:300],
       Sorted.Current$Bmass.mean[which(Sorted.Current$RGroup=="shrub")],lwd=1,type='l',lty=1,col='navyblue')

polygon(forb.coords,col="#eeeeeeaa",border="NA")
# legend(-4,107,c("Future PF mean","Current PF mean", "Future PF range",
#                 "Future AF mean","Current AF mean","Future PF range"),
#        lwd=c(3,1.5,10,3,1.5,10),
#        col=c("navyblue","navyblue","royalblue1","chocolate3","chocolate3","orange"), bty="n",ncol=2,cex=1)
par(xpd=NA)
text(90,57, "c) Mid-century: Forbs and other shrubs",cex=1.8)
################################################


