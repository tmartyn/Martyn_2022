rm(list=ls())
version<-"21.10.2022"
########### Site Variability ####################################
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
Stepwat.Sorted.Site<-data.frame(ID=1:10800)
c<-1
for (p in Period) { # for each period 2060 (50) or 2100 (90)
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      print(y)
      # DF.temp2<-data.frame(ID=1:10)
      # d<-1
      for (r in Rgroup) { # for each group
      
        #print(r)
        DF.new<-data.frame(ID=1:length(Site))
        b<-1
        for( j in Site) {
          
          #for( g in GCM) { #for each site
            yearly.r<-Stepwat.DF[which(Stepwat.DF$site==j & Stepwat.DF$Year==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),r]
            m.yearly.r<-mean(yearly.r,na.rm=T) # mean across GCMs
            DF.new$Year[b]<-y
            DF.new$RGroup[b]<-r
            DF.new$Site[b]<-j
            #DF.new$GCM[b]<-g
            DF.new$Bmass[b]<-m.yearly.r
            b<-b+1
            }
          
        #  DF.temp<-
          #DF.temp2$Year[d]<-y
          #DF.temp2$RGroup[d]<-r
          #DF.temp2$Site[d]<-j
          #DF.temp2$Bmass.mean[d]<-mean(DF.temp$Bmass)
          #d<-d+1
        
        DF.fin<-DF.new[order(DF.new$Bmass),]
        
        Stepwat.Sorted.Site$RCP[c]<-s
        Stepwat.Sorted.Site$Period[c]<-p
        Stepwat.Sorted.Site$Year[c]<-y
        Stepwat.Sorted.Site$RGroup[c]<-r
        Stepwat.Sorted.Site$Site.min[c]<-DF.fin$Site[1]
        Stepwat.Sorted.Site$Bmass.min[c]<-DF.fin$Bmass[1]
        Stepwat.Sorted.Site$Site.max[c]<-DF.fin$Site[10]
        Stepwat.Sorted.Site$Bmass.max[c]<-DF.fin$Bmass[10]
        Stepwat.Sorted.Site$Site.med1[c]<-DF.fin$Site[5]
        Stepwat.Sorted.Site$Bmass.med1[c]<-DF.fin$Bmass[5]
        Stepwat.Sorted.Site$Site.med2[c]<-DF.fin$Site[6]
        Stepwat.Sorted.Site$Bmass.med2[c]<-DF.fin$Bmass[6]
        Stepwat.Sorted.Site$Bmass.mean[c]<-mean(DF.fin$Bmass)
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted.Site<-Stepwat.Sorted.Site[order(Stepwat.Sorted.Site$RCP,Stepwat.Sorted.Site$Period,Stepwat.Sorted.Site$RGroup,Stepwat.Sorted.Site$Year),]
Stepwat.Sorted.Site<-unique(Stepwat.Sorted.Site)
#write csv
write.csv(Stepwat.Sorted.Site,paste0("output/Stepwat.Sorted.Site.Means.",version,".csv"))

######## Current Site Variability #######################

########## make a new dataframe of median max and min values ###
GCM<-"Current"
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")
#Site<-1:10
Period<-"NONE"
RCP<-"NONE"
Year<-1:300
Stepwat.Sorted.Site.Current<-data.frame(ID=1:2700)
c<-1
for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      print(y)
      for (r in Rgroup) {
        # DF.temp2<-data.frame(ID=1:10)
        # d<-1
        #print(r)
        DF.new<-data.frame(ID=1:10)
        b<-1
        for( j in Site) {
          
          
          
          #for( g in GCM) { #for each site
            yearly.r<-Stepwat.DF[which(Stepwat.DF$site==j & Stepwat.DF$Year==y & Stepwat.DF$RCP==s & Stepwat.DF$YEARS==p),r]
            m.yearly.r<-mean(yearly.r,na.rm=T)
            DF.new$Year[b]<-y
            DF.new$RGroup[b]<-r
            DF.new$Site[b]<-j
            #DF.new$GCM[b]<-g
            DF.new$Bmass[b]<-m.yearly.r
            b<-b+1
        }
          
          # DF.temp<-DF.new[order(DF.new$Bmass),]
          # DF.temp2$Year[d]<-y
          # DF.temp2$RGroup[d]<-r
          # DF.temp2$Site[d]<-j
          # DF.temp2$Bmass.med[d]<-(DF.temp$Bmass[5]+DF.temp$Bmass[6])/2
          # DF.temp2$Bmass.mean[d]<-mean(DF.temp$Bmass)
          # d<-d+1
        
        DF.fin<-DF.new[order(DF.new$Bmass),]
        
        Stepwat.Sorted.Site.Current$RCP[c]<-s
        Stepwat.Sorted.Site.Current$Period[c]<-p
        Stepwat.Sorted.Site.Current$Year[c]<-y
        Stepwat.Sorted.Site.Current$RGroup[c]<-r
        Stepwat.Sorted.Site.Current$Site.min[c]<-DF.fin$Site[1]
        Stepwat.Sorted.Site.Current$Bmass.min[c]<-DF.fin$Bmass[1]
        Stepwat.Sorted.Site.Current$Site.max[c]<-DF.fin$Site[10]
        Stepwat.Sorted.Site.Current$Bmass.max[c]<-DF.fin$Bmass[10]
        Stepwat.Sorted.Site.Current$Site.med1[c]<-DF.fin$Site[5]
        Stepwat.Sorted.Site.Current$Bmass.med1[c]<-DF.fin$Bmass[5]
        Stepwat.Sorted.Site.Current$Site.med2[c]<-DF.fin$Site[6]
        Stepwat.Sorted.Site.Current$Bmass.med2[c]<-DF.fin$Bmass[6]
        Stepwat.Sorted.Site.Current$Bmass.mean[c]<-mean(DF.fin$Bmass)
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted.Site.Current<-Stepwat.Sorted.Site.Current[order(Stepwat.Sorted.Site.Current$RGroup,Stepwat.Sorted.Site.Current$Year),]

#write csv
write.csv(Stepwat.Sorted.Site.Current,paste0("output/Stepwat.Sorted.Site.Current.Means.",version,".csv"))

