version<-"21.10.2022"
#############################################################3
####### Change in Biomass absolute and percentages for results section
####### Trace Martyn 14.07.2017- Version 3
################################################################

### Change in Biomass absolute and percentages
GCM.bmass<-read.csv(paste0("output/barplot.calcs.",version,".csv"))

GCM.bmass$per.diff<-GCM.bmass$med.diff/GCM.bmass$mean*100
GCM.bmass$prop.diff<-GCM.bmass$med.diff/GCM.bmass$mean

str(GCM.bmass)

GCM.bmass.45.mid<-GCM.bmass[which(GCM.bmass$RCP=="RCP45"&GCM.bmass$Period=="d50yrs"),]
GCM.bmass.45.mid$rel<-GCM.bmass.45.mid$mean/sum(GCM.bmass.45.mid$mean)*100
GCM.bmass.45.late<-GCM.bmass[which(GCM.bmass$RCP=="RCP45"&GCM.bmass$Period=="d90yrs"),]
GCM.bmass.45.late$rel<-GCM.bmass.45.late$mean/sum(GCM.bmass.45.late$mean)*100
GCM.bmass.85.mid<-GCM.bmass[which(GCM.bmass$RCP=="RCP85"&GCM.bmass$Period=="d50yrs"),]
GCM.bmass.85.mid$rel<-GCM.bmass.85.mid$mean/sum(GCM.bmass.85.mid$mean)*100
GCM.bmass.85.late<-GCM.bmass[which(GCM.bmass$RCP=="RCP85"&GCM.bmass$Period=="d90yrs"),]
GCM.bmass.85.late$rel<-GCM.bmass.85.late$mean/sum(GCM.bmass.85.late$mean)*100

GCM.bmass2<-do.call(rbind,list(GCM.bmass.45.mid,GCM.bmass.45.late,GCM.bmass.85.mid,GCM.bmass.85.late))
write.csv(GCM.bmass2,paste0("output/mean.biomass.values.",version,".csv"))

