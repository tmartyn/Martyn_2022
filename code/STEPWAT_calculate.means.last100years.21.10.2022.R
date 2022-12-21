version<-"21.10.2022"
########### Site Variability ####################################
########## make a new dataframe of median max and min values ###
Stepwat.DF<-read.csv("data/StepWat.300yrs.AllSites.2.csv")
Stepwat.DF$YEARS<-factor(Stepwat.DF$YEARS,levels=c("d50yrs","d90yrs","NONE"),
                         labels=c("50years","90years","Current"))
############ read in CSVs#########################
########### average last 100 years of data - Site & Current ########
Site<- unique(Stepwat.DF$site)
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb",
          "p.warm.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")
Means<-data.frame(ID=1:360)
#SITE<-1:10
RCP<-c("RCP45","RCP85")
Period<-c("50years","90years")
GCM<-c("CanESM2","CESM1-CAM5","CSIRO-Mk3-6-0","FGOALS-g2","FGOALS-s2",
       "GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-MR",
       "MIROC5","MIROC-ESM","MRI-CGCM3","Current")
Years<-201:300

b<-1

#setwd(data.dir)
########### Site #####################
for (p in Period) { # for each period 2060 or 2100
        for (s in RCP) {
                for (i in Site) {
                         for (r in Rgroup) {
                                
                bm<-Stepwat.DF[Stepwat.DF$site==i & Stepwat.DF$YEARS==p & Stepwat.DF$RCP==s & Stepwat.DF$Year %in% Years,r]
                m.bm<-mean(bm)
                sd.bm<-sd(bm)
                Means$Site[b]<-i
                Means$RCP[b]<-s
                Means$Period[b]<-p
                Means$Rgroup[b]<-r
                Means$M.Bmass[b]<-m.bm
                Means$SD.Bmass[b]<-m.bm
                b<-b+1
        }
                         }}}

##### Site  Current
Means.C<-data.frame(ID=1:90)
#Period<-"Current"
#RCP<-"NONE"
b<-1
for (i in Site) {
        for (r in Rgroup) {
                        bm<-Stepwat.DF[Stepwat.DF$site==i & Stepwat.DF$YEARS=="Current" & Stepwat.DF$RCP=="NONE" & Stepwat.DF$Year %in% Years,r]
                        m.bm<-mean(bm)
                        sd.bm<-sd(bm)
                        Means.C$Site[b]<-i
                        Means.C$RCP[b]<-0
                        Means.C$Period[b]<-0
                        Means.C$Rgroup[b]<-r
                        Means.C$M.Bmass[b]<-m.bm 
                        Means.C$SD.Bmass[b]<-sd.bm
                        b<-b+1
                                }
                        }


Means.Site<-rbind(Means,Means.C) 


write.csv(Means.Site,paste0("output/Means.Site.",version,".csv"))




########### average last 100 years of data - GCM ########
#Site<-1:10
#GCM<-unique(Stepwat.DF$GCM)
#GCM<-GCM[1:10]
GCM<-GCM[-which(GCM=="Current")]
#Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Means.G<-data.frame(ID=1:468)
Years<-201:300
RCP<-c("RCP45","RCP85")
Period<-c("50years","90years")

b<-1
for (p in Period) { # for each period 2060 or 2100
        for (s in RCP) {
                for (g in GCM) {
                        for (r in Rgroup) {
                                
                                bm<-Stepwat.DF[Stepwat.DF$GCM==g & Stepwat.DF$YEARS==p & Stepwat.DF$RCP==s & Stepwat.DF$Year %in% Years,r]
                                m.bm<-mean(bm)
                                Means.G$GCM[b]<-g
                                Means.G$RCP[b]<-s
                                Means.G$Period[b]<-p
                                Means.G$Rgroup[b]<-r
                                Means.G$M.Bmass[b]<-m.bm
                                b<-b+1
                        }
                }}}

Means.C.2<-data.frame(ID=1:9)
b<-1

  for (r in Rgroup) {
    bm<-Stepwat.DF[Stepwat.DF$YEARS=="Current" & Stepwat.DF$RCP=="NONE" & Stepwat.DF$Year %in% Years,r]
    m.bm<-mean(bm)
    Means.C.2$GCM[b]<-0
    Means.C.2$RCP[b]<-0
    Means.C.2$Period[b]<-0
    Means.C.2$Rgroup[b]<-r
    Means.C.2$M.Bmass[b]<-m.bm
    b<-b+1
  }




colnames(Means.C.2)<-c("ID","GCM","RCP","Period","Rgroup","M.Bmass")
Means.GCM<-rbind(Means.G,Means.C.2)

write.csv(Means.GCM,paste0("output/Means.GCM.",version,".csv"))

