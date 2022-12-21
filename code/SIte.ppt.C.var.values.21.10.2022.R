

Stepwat.DF<-read.csv("data/StepWat.300yrs.AllSites.2.csv")

cols.keep<-c("Year","PPT","StdDev","Temp","StdDev.1","site","RCP","GCM","YEARS")
Weath<-Stepwat.DF[,names(Stepwat.DF)%in%cols.keep]

#Site.weath<-read.csv(file.choose())
names(Weath)
#Site.weath<-Weath
years<-201:300
Weath<-Weath[which(Weath$Year%in%years),]

Site.weath0<-with(Weath,aggregate(cbind(PPT,Temp),by=list(RCP,site,YEARS),summary))
names(Site.weath0)[1:3]<-c("RCP","Site","Period")

RCP<-c("RCP45","RCP85")
period<-c("d90yrs","d50yrs")

for (r in RCP){
  for (p in period){
    site.list.ppt<-c()
    site.list.temp<-c()
    print (paste0(p,"_",r,"_PPT"))
    print(mean(Site.weath0$PPT[which(Site.weath0$RCP==r& Site.weath0$Period==p),1]))
    print(mean(Site.weath0$PPT[which(Site.weath0$RCP==r& Site.weath0$Period==p),6]))
    print(mean(Site.weath0$PPT[which(Site.weath0$RCP==r& Site.weath0$Period==p),4]))
    print (paste0(p,"_",r,"_Temp"))
    print(mean(Site.weath0$Temp[which(Site.weath0$RCP==r& Site.weath0$Period==p),1]))
    print(mean(Site.weath0$Temp[which(Site.weath0$RCP==r& Site.weath0$Period==p),6]))
    print(mean(Site.weath0$Temp[which(Site.weath0$RCP==r& Site.weath0$Period==p),4]))
    for (s in unique(Site.weath0$Site)){
      diff.ppt<-mean(Site.weath0$PPT[which(Site.weath0$RCP==r& Site.weath0$Period==p),4])-
        mean(Site.weath0$PPT[which(Site.weath0$RCP=="NONE"& Site.weath0$Period=="NONE"),4])
      site.list.ppt<-c(site.list.ppt,diff.ppt)
      diff.temp<-mean(Site.weath0$Temp[which(Site.weath0$RCP==r& Site.weath0$Period==p),4])-
        mean(Site.weath0$Temp[which(Site.weath0$RCP=="NONE"& Site.weath0$Period=="NONE"),4])
      site.list.temp<-c(site.list.temp,diff.temp)
      
      
      
    }
    print(site.list.ppt>0)
    print(site.list.temp>0)
  }
}
