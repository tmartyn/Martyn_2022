version<-"21.10.2022"
#############################################################3
####### Code for Stepwat Figures
####### Trace Martyn 14.07.2017- Version 3
################################################################
########### average last 100 years of data - Site & Current ########
########### average last 100 years of data - Site & Current ########
Stepwat.DF<-read.csv("data/StepWat.300yrs.AllSites.2.csv")

Site<- unique(Stepwat.DF$site)
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb",
          "p.warm.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")
Means<-data.frame(ID=1:360)
#SITE<-1:10
RCP<-c("RCP45","RCP85")
Period<-c("d50yrs","d90yrs")
GCM<-c("CanESM2","CESM1-CAM5","CSIRO-Mk3-6-0","FGOALS-g2","FGOALS-s2",
       "GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-MR",
       "MIROC5","MIROC-ESM","MRI-CGCM3","Current")
Years<-201:300
############################
########### average the late 100 years of bmass values - Site ##############
Stepwat.Sorted.Site<-read.csv(paste0("output/Stepwat.Sorted.Site.Means.",version,".csv"))

Site<- unique(Stepwat.DF$site)
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb",
          "p.warm.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")
#Means<-data.frame(ID=1:360)
#SITE<-1:10
RCP<-c("RCP45","RCP85")
Period<-c("d50yrs","d90yrs")
GCM<-c("CanESM2","CESM1-CAM5","CSIRO-Mk3-6-0","FGOALS-g2","FGOALS-s2",
       "GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-MR",
       "MIROC5","MIROC-ESM","MRI-CGCM3","Current")
Years<-201:300


Stepwat.Sorted.Site<-Stepwat.Sorted.Site[Stepwat.Sorted.Site$Year %in% Years,]
#Stepwat.Sorted.Site$Bmass.median<-(Stepwat.Sorted.Site$Bmass.med1+Stepwat.Sorted.Site$Bmass.med2)/2

Stepwat.Sorted.Site.Current<-read.csv(paste0("output/Stepwat.Sorted.Site.Current.Means.",version,".csv"))


Stepwat.Sorted.Site.C<- Stepwat.Sorted.Site.Current[Stepwat.Sorted.Site.Current$Year %in% Years,]
#Stepwat.Sorted.Site.C$Bmass.median<-(Stepwat.Sorted.Site.C$Bmass.med1 + Stepwat.Sorted.Site.C$Bmass.med2)/2
Stepwat.Sorted.Site.C$RCP<-as.factor(paste(Stepwat.Sorted.Site.C$RCP))
Stepwat.Sorted.Site.C$Period<-as.factor(paste(Stepwat.Sorted.Site.C$Period))

Stepwat.Sorted.Site<-rbind(Stepwat.Sorted.Site, Stepwat.Sorted.Site.C)
#Stepwat.Sorted.Site.agg<-aggregate(cbind(Stepwat.Sorted.Site$Bmass.median,Stepwat.Sorted.Site$Bmass.min,Stepwat.Sorted.Site$Bmass.max), by=list(Stepwat.Sorted.Site$RCP, Stepwat.Sorted.Site$Period, Stepwat.Sorted.Site$RGroup), mean, na.rm=T)

#Stepwat.Sorted.Site<-Stepwat.Sorted.Site[-which(Stepwat.Sorted.Site$RGroup=="a.warm.grass"),]
Stepwat.Sorted.Site$RGroup<-as.character(Stepwat.Sorted.Site$RGroup)


a.forbs<-c("a.cool.forb","a.warm.forb")
p.forbs<-c("p.cool.forb","p.warm.forb")
#groups<-factor(unique(Stepwat.Sorted.Site$RGroup))
#replace.groups<-c("a.forb","a.cool.grass","a.forb","P.forb","p.cool.grass","p.forb","p.warm.grass","sagebrush","shrub")
Stepwat.Sorted.Site$RGroup[which(Stepwat.Sorted.Site$RGroup%in%a.forbs)]<-"a.forb"
Stepwat.Sorted.Site$RGroup[which(Stepwat.Sorted.Site$RGroup%in%p.forbs)]<-"p.forb"



new2<-data.frame(ID=c(1:28))
b<-1

RG<-unique(Stepwat.Sorted.Site$RGroup)


for (r in RG) {
  for (s in c("RCP45","RCP85")) {
    for (i in Period) {
      x.0<-mean(Stepwat.Sorted.Site$Bmass.mean[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period=="NONE" & Stepwat.Sorted.Site$RCP=="NONE")], na.rm=T)
      x.0.all<-Stepwat.Sorted.Site$Bmass.mean[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period=="NONE" & Stepwat.Sorted.Site$RCP=="NONE")]
      median<-mean(Stepwat.Sorted.Site$Bmass.mean[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period==i & Stepwat.Sorted.Site$RCP==s)],na.rm=T)
      # the above was mean
      med.all<-Stepwat.Sorted.Site$Bmass.mean[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period==i & Stepwat.Sorted.Site$RCP==s)]
      # the above was mean not med1
      max<-mean(Stepwat.Sorted.Site$Bmass.max[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period==i & Stepwat.Sorted.Site$RCP==s)],na.rm=T)
      min<-mean(Stepwat.Sorted.Site$Bmass.min[which(Stepwat.Sorted.Site$RGroup==r & Stepwat.Sorted.Site$Period==i & Stepwat.Sorted.Site$RCP==s)],na.rm=T)
      coef.var<-(sd((med.all-x.0.all),na.rm=T))/(mean((med.all-x.0.all),na.rm=T))
      mean.diff<-mean((med.all-x.0.all),na.rm=T)
      sdd<-sd((med.all-x.0.all),na.rm=T)
      new2$Rgroup[b]<-r
      new2$RCP[b]<-s
      new2$Period[b]<-i
      new2$med.diff[b]<-median-x.0
      new2$min.diff[b]<-min-x.0
      new2$max.diff[b]<-max-x.0
      new2$coef.var[b]<-coef.var
      new2$sd[b]<-sdd
      new2$mean.diff[b]<-mean.diff
      new2$current[b]<-x.0
      new2$max[b]<-max
      new2$min[b]<-min
      new2$mean[b]<-median
      b<-b+1
    }
  }
  }


write.csv(new2,paste0("output/barplot.calcs.",version,".csv"))
