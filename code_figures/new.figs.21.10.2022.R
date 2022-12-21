version<-"21.10.2022"
library(tidyverse)
#############################################################3
####### Code for Stepwat Figures
####### Trace Martyn 14.07.2017- Version 3
################################################################
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
GCM<-GCM[-which(GCM=="Current")]
Years<-201:300
############################
#############################################################################
########### average the late 100 years of bmass values - GCM ##############
Stepwat.Sorted<-read.csv(paste0("output/Stepwat.Sorted.GCM.Means.",version,".csv"))
Stepwat.GCM<-read.csv(paste0("output/Stepwat.GCM.PPT.Temp",version,".csv"))

ggplot(Stepwat.GCM,aes(x=PPT,y=log(Bmass),col=RGroup))+geom_point()+
  geom_smooth(method="lm")+facet_grid(RCP~Period)+
  theme_minimal()



test<-with(Stepwat.GCM,aggregate(cbind(PPT,TMP,Bmass),by=list(RGroup,GCM,RCP,Period),mean))
head(test)
names(test)[1:4]<-c("RGroup","GCM","RCP","Period")
test$RGroup<-as.character(test$RGroup)

for (g in GCM){
  for (r in RCP){
    for (p in Period){
      a.forb<-test$Bmass[which(test$RGroup=="a.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]+
        test$Bmass[which(test$RGroup=="a.warm.forb" &test$Period==p &test$RCP==r &test$GCM==g)]
      test$Bmass[which(test$RGroup=="a.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]<-a.forb
      test$RGroup[which(test$RGroup=="a.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]<-"a.forb"
      p.forb<-test$Bmass[which(test$RGroup=="p.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]+
        test$Bmass[which(test$RGroup=="p.warm.forb" &test$Period==p &test$RCP==r &test$GCM==g)]
      test$Bmass[which(test$RGroup=="p.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]<-p.forb
      test$RGroup[which(test$RGroup=="p.cool.forb" &test$Period==p &test$RCP==r &test$GCM==g)]<-"p.forb"
    }
  }
}

test<-test[-which(test$RGroup=="a.warm.forb" | test$RGroup=="p.warm.forb"),]

#test$RGroup<-factor(test$RGroup,
#                    levels=c("sagebrush","shrub","p.cool.grass","p.warm.grass", "a.cool.grass","p.forb","a.forb"), 
#                    labels=c("Big Sagebrush","Other Shrubs","C3 Perennial Grasses",
#                    "C4 Perennial Grasses","C3 Annual Grasses","Perennial Forbs","Annual Forbs"))

#test$Period<-factor(test$Period,labels=c("Mid-century","Late-century"))



cols<-c("black","blue","darkgreen","darkgoldenrod","purple","red","orange")
cols<-c("black","navyblue","darkgreen","darkgoldenrod4","mediumpurple4","darkred","chocolate3")

test85<-test[which(test$RCP=="RCP85"),]
test85B<-gather(test85, weath,value,PPT:TMP)
test85B$weath<-factor(test85B$weath,labels=c("Precipitation (mm)","Temperature (C)"))


ggplot(test85B,aes(x=value,y=log(Bmass),col=RGroup,fill=RGroup))+geom_point()+
  geom_smooth(method="lm")+facet_grid(Period~weath,scales="free_x")+
  scale_fill_manual(values=cols)+
  scale_color_manual(values=cols)+
  ylab("log( Biomass (g) )")+
  xlab("")+
  theme_minimal()+
  theme(legend.position = "bottom",panel.spacing = unit(2, "lines"),legend.title = element_blank())+
  guides(color=guide_legend(nrows=2,byrow=F))

ggplot(test,aes(x=TMP,y=log(Bmass),col=RGroup,fill=RGroup))+geom_point()+
  geom_smooth(method="lm")+facet_grid(Period~RCP)+
  scale_fill_manual(values=cols)+
  scale_color_manual(values=cols)+
  theme_minimal()+
  theme(legend.position = "bottom",panel.spacing = unit(2, "lines"))+
  guides(color=guide_legend(nrows=2,byrow=T))


current<-read.csv(paste0("output/Stepwat.GCM.PPT.Temp.Current.",version,".csv"))
head(current)
test.C<-with(current,aggregate(cbind(PPT,TMP,Bmass),by=list(RGroup),mean))
head(test.C)
names(test.C)[1]<-c("RGroup")
test.C$RGroup<-as.character(test.C$RGroup)

#for (g in GCM){
#  for (r in RCP){
#    for (p in Period){
a.forb<-test.C$Bmass[which(test.C$RGroup=="a.cool.forb" )]+
  test.C$Bmass[which(test.C$RGroup=="a.warm.forb" )]
test.C$Bmass[which(test.C$RGroup=="a.cool.forb" )]<-a.forb
test.C$RGroup[which(test.C$RGroup=="a.cool.forb" )]<-"a.forb"
p.forb<-test.C$Bmass[which(test.C$RGroup=="p.cool.forb" )]+
  test.C$Bmass[which(test.C$RGroup=="p.warm.forb" )]
test.C$Bmass[which(test.C$RGroup=="p.cool.forb" )]<-p.forb
test.C$RGroup[which(test.C$RGroup=="p.cool.forb" )]<-"p.forb"
#    }
#  }
#}

Rgroup<-c("sagebrush","a.forb","p.forb","a.cool.grass","p.cool.grass","p.warm.grass","shrub")

x<-1
for (g in GCM){
  for (r in RCP){
    for (p in Period){
      NEW.DF<-data.frame(data.frame(ID=1:10))
      b<-1
      for(i in Rgroup) {
        bio.diff<-test$Bmass[which(test$RGroup==i &test$Period==p &test$RCP==r &test$GCM==g)]-
          test.C$Bmass[which(test.C$RGroup==i)]
        PPT.diff<-test$PPT[which(test$RGroup==i &test$Period==p &test$RCP==r &test$GCM==g)]-
          test.C$PPT[which(test.C$RGroup==i)]
        TEMP.diff<-test$TMP[which(test$RGroup==i &test$Period==p &test$RCP==r &test$GCM==g)]-
          test.C$TMP[which(test.C$RGroup==i)]
        NEW.DF$GCM[b]<-g
        NEW.DF$RCP[b]<-r
        NEW.DF$Period[b]<-p
        NEW.DF$RGroup[b]<-i
        NEW.DF$BMass.diff[b]<-bio.diff
        NEW.DF$PPT.diff[b]<-PPT.diff
        NEW.DF$TEMP.diff[b]<-TEMP.diff
        b<-b+1
      }
      
      if(x==1){
        DIFF.DF<-NEW.DF
      } else {DIFF.DF<-rbind(DIFF.DF,NEW.DF) }
      x<-x+1
      
    }
  }
}


DIFF.DF85<-DIFF.DF[which(DIFF.DF$RCP=="RCP85"),]
DIFF.DF85B<-gather(DIFF.DF85, weath,value,PPT.diff:TEMP.diff)
DIFF.DF85B$weath<-factor(DIFF.DF85B$weath,labels=c("Change in Precipitation (mm)"," Change in Temperature (C)"))

DIFF.DF85B$RGroup<-factor(DIFF.DF85B$RGroup,
                          levels=c("sagebrush","shrub","p.cool.grass","p.warm.grass", "a.cool.grass","p.forb","a.forb"),
                          labels=c("Big Sagebrush","Other Shrubs","C3 Perennial Grasses",
                                   "C4 Perennial Grasses","C3 Annual Grasses","Perennial Forbs","Annual Forbs"))

DIFF.DF85B$Period<-factor(DIFF.DF85B$Period,labels=c("Mid-century","Late-century"))


library(Cairo)

cairo_pdf("figures/gcm_biomas_variation.21.10.2022.pdf",height=9,width=12)
ggplot(DIFF.DF85B,aes(x=value,y=sign(BMass.diff)*log(abs(BMass.diff)),col=RGroup,fill=RGroup))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(Period~weath,scales="free_x")+
  scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4","mediumpurple4","darkred","chocolate3")) +
  scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1","indianred1","orange")) +  
  ylab("log( Change in Biomass (g) )")+
  xlab("")+
  theme_minimal(base_size = 22)+
  theme(legend.position = "bottom",panel.spacing = unit(2, "lines"),legend.title = element_blank())+
  guides(color=guide_legend(nrows=2,byrow=F))

dev.off()


ggplot(DIFF.DF85B,aes(x=value,y=BMass.diff,col=RGroup,fill=RGroup))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(Period~weath,scales="free_x")+
  scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4","mediumpurple4","darkred","chocolate3"),guide=F) +
  scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1","indianred1","orange")) +  labs(fill="Functional type") +
  ylab("log( Change in Biomass (g) )")+
  xlab("")+
  theme_minimal(base_size = 22)+
  theme(legend.position = "bottom",panel.spacing = unit(2, "lines"),legend.title = element_blank())+
  guides(color=guide_legend(nrows=2,byrow=F))
