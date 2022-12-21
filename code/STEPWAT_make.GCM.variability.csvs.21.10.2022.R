rm(list=ls())
version<-"21.10.2022"
########### GCM Variability ####################################
########## make a new dataframe of median max and min values ###
Stepwat.DF<-read.csv("data/StepWat.300yrs.AllSites.2.csv")

GCM<-unique(Stepwat.DF$GCM) 
GCM<-GCM[-which(GCM=="Current")]
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")
Site<-unique(Stepwat.DF$site)
Period<-unique(Stepwat.DF$YEARS)
Period<-Period[-which(Period=="NONE")]
RCP<-unique(Stepwat.DF$RCP)
RCP<-RCP[-which(RCP=="NONE")]
Year<-1:300
Stepwat.Sorted<-data.frame(ID=1:10800)
c<-1
x<-1

for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) {
      print(y)# for each year
      for (r in Rgroup) { # for each group
        
        # DF.temp2<-data.frame(ID=1:10)
        # d<-1
        DF.new<-data.frame(ID=1:length(GCM))
        b<-1
        for( g in GCM) { # for each GCM
          
          #for( j in Site) { #for each site
          yearly.ppt<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),"PPT"]
          m.yearly.ppt<-mean(yearly.ppt,na.rm=T) # mean across sites
          DF.new$PPT[b]<-m.yearly.ppt
          yearly.tmp<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),"Temp"]
          m.yearly.tmp<-mean(yearly.tmp,na.rm=T) # mean across sites
          DF.new$TMP[b]<-m.yearly.tmp
          yearly.r<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),r]
          m.yearly.r<-mean(yearly.r,na.rm=T) # mean across sites
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          #DF.new$Site[b]<-j
          DF.new$RCP[b]<-s
          DF.new$Period[b]<-p
          DF.new$GCM[b]<-g
          DF.new$Bmass[b]<-m.yearly.r
          b<-b+1
        }
        
        # DF.temp<-DF.new[order(DF.new$Bmass),]
        # DF.temp2$Year[d]<-y
        # DF.temp2$RGroup[d]<-r
        # DF.temp2$GCM[d]<-g
        # DF.temp2$Bmass[d]<-mean(DF.temp$Bmass)
        # d<-d+1
        
        if(x==1){
          DF.fin.full<-DF.new
        } else {DF.fin.full<-rbind(DF.fin.full,DF.new) }
        x<-x+1
        
        DF.fin<-DF.new[order(DF.new$Bmass),]
        
        Stepwat.Sorted$RCP[c]<-s
        Stepwat.Sorted$Period[c]<-p
        Stepwat.Sorted$Year[c]<-y
        Stepwat.Sorted$RGroup[c]<-r
        Stepwat.Sorted$GCM.min[c]<-DF.fin$GCM[1]
        Stepwat.Sorted$Bmass.min[c]<-DF.fin$Bmass[1]
        Stepwat.Sorted$GCM.max[c]<-DF.fin$GCM[10]
        Stepwat.Sorted$Bmass.max[c]<-DF.fin$Bmass[10]
        Stepwat.Sorted$GCM.med1[c]<-DF.fin$GCM[7]
        Stepwat.Sorted$Bmass.med1[c]<-DF.fin$Bmass[7]
        Stepwat.Sorted$GCM.med2[c]<-DF.fin$GCM[6]
        Stepwat.Sorted$Bmass.med2[c]<-DF.fin$Bmass[6]
        Stepwat.Sorted$Bmass.mean[c]<-mean(DF.fin$Bmass)
        #Stepwat.Sorted$Bmass.PPT[c]<-mean(DF.fin$Bmass)
        #Stepwat.Sorted$Bmass.Temp[c]<-mean(DF.fin$Bmass)
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted<-Stepwat.Sorted[order(Stepwat.Sorted$RCP,Stepwat.Sorted$Period,Stepwat.Sorted$RGroup,Stepwat.Sorted$Year),]

#write csv
write.csv(Stepwat.Sorted,paste0("output/Stepwat.Sorted.GCM.Means.",version,".csv"))
write.csv(DF.fin.full,paste0("output/Stepwat.GCM.PPT.Temp",version,".csv"))


############# Current GCM ####################################
#Stepwat.DF<-read.csv("StepWat.300yrs.AllSites.csv")
GCM<-"Current"
#Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Site<-unique(Stepwat.DF$Site)
Period<-unique(Stepwat.DF$Period)
Period<-"NONE"
#RCP<-unique(Stepwat.DF$RCP)
RCP<-"NONE"
Year<-unique(Stepwat.DF$Year)
Stepwat.Sorted.Current<-data.frame(ID=1:2700)
c<-1
x<-1
for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      print(y)
      for (r in Rgroup) { # for each group
        # DF.temp2<-data.frame(ID=1:10)
        # d<-1
        DF.new<-data.frame(ID=1:10)
        b<-1
        for( g in GCM) {
          
          #for( j in Site) { #for each site
          yearly.ppt<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),"PPT"]
          m.yearly.ppt<-mean(yearly.ppt,na.rm=T) # mean across sites
          DF.new$PPT[b]<-m.yearly.ppt
          yearly.tmp<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),"Temp"]
          m.yearly.tmp<-mean(yearly.tmp,na.rm=T) # mean across sites
          DF.new$TMP[b]<-m.yearly.tmp
          yearly.r<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),r]
          m.yearly.r<-mean(yearly.r,na.rm=T)
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          #DF.new$Site[b]<-j
          DF.new$GCM[b]<-g
          DF.new$Bmass[b]<-m.yearly.r
          b<-b+1
        }
        
        if(x==1){
          DF.fin.full.C<-DF.new
        } else {DF.fin.full.C<-rbind(DF.fin.full.C,DF.new) }
        x<-x+1
        # DF.temp<-DF.new[order(DF.new$Bmass),]
        # DF.temp2$Year[d]<-y
        # DF.temp2$RGroup[d]<-r
        # DF.temp2$GCM[d]<-g
        # DF.temp2$Bmass.med[d]<-(DF.temp$Bmass[5]+DF.temp$Bmass[6])/2
        # DF.temp2$Bmass.mean[d]<-mean(DF.temp$Bmass)
        # d<-d+1
        
        
        
        #DF.fin<-DF.temp2[order(DF.temp2$Bmass),]
        
        Stepwat.Sorted.Current$RCP[c]<-s
        Stepwat.Sorted.Current$Period[c]<-p
        Stepwat.Sorted.Current$Year[c]<-y
        Stepwat.Sorted.Current$RGroup[c]<-r
        Stepwat.Sorted.Current$GCM.mean[c]<-DF.new$GCM[1]
        Stepwat.Sorted.Current$Bmass.mean[c]<-DF.new$Bmass[1]
        # Stepwat.Sorted.Current$GCM.max[c]<-DF.fin$GCM[10]
        # Stepwat.Sorted.Current$Bmass.max[c]<-DF.fin$Bmass[10]
        # Stepwat.Sorted.Current$GCM.med1[c]<-DF.fin$GCM[5]
        # Stepwat.Sorted.Current$Bmass.med1[c]<-DF.fin$Bmass[5]
        # Stepwat.Sorted.Current$GCM.med2[c]<-DF.fin$GCM[6]
        # Stepwat.Sorted.Current$Bmass.med2[c]<-DF.fin$Bmass[6]
        # Stepwat.Sorted.Current$Bmass.mean[c]<-mean(DF.fin$Bmass)
        c<-c+1}
    }
  }
}


Stepwat.Sorted.Current<-unique(Stepwat.Sorted.Current)
#Stepwat.Sorted.Current<-Stepwat.Sorted.Current[(dim(Stepwat.Sorted.Current)[2]-1),]
#reorder to make it easier to make figures 
Stepwat.Sorted.Current<-Stepwat.Sorted.Current[order(Stepwat.Sorted.Current$RGroup,Stepwat.Sorted.Current$Year),]

#write csv
write.csv(Stepwat.Sorted.Current,paste0("output/Stepwat.Sorted.GCM.Current.Means.",version,".csv"))
write.csv(DF.fin.full.C,paste0("output/Stepwat.GCM.PPT.Temp.Current.",version,".csv"))

