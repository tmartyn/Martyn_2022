library(ggplot2)
library(tidyr)
library(dplyr)
set.seed(5540)
Stepwat.DF<-read.csv("data/StepWat.300yrs.AllSites.2.csv")
Stepwat.Sorted<-read.csv(paste0("output/Stepwat.Sorted.GCM.Means.",version,".csv"))
Stepwat.Sorted.Current<-read.csv(paste0("output/Stepwat.Sorted.GCM.Current.Means.",version,".csv"))

cols<-c("Year","RCP","GCM","site","YEARS",
        #"PPT","StdDev","Temp","StdDev.1",
        "sagebrush","a.cool.forb","a.warm.forb",
        "p.cool.forb","p.warm.forb","a.cool.grass",
       "p.cool.grass","p.warm.grass","shrub")

DF<-Stepwat.DF[,cols]
DF<-DF[which(DF$Year>200),]

#JJ<- data.frame(
  
  JJ<-data.frame(DF %>% gather(key=Rgroup,value=Bmass,sagebrush:shrub,factor_key=T)) #%>%
  
  JJ<-with(JJ, aggregate(Bmass,by=list(Rgroup,RCP,GCM,site,YEARS),mean))
  #JJ%>%group_by(Rgroup,RCP,GCM,site,YEARS) %>% summarise(mean(Bmass))
  #)
names(JJ)<-c("Rgroup","RCP","GCM","site","YEARS","Bmass")



current<-JJ[which(JJ$RCP=="NONE"),]
current

JJ<-JJ[-which(JJ$RCP=="NONE"),]
JJ$Bmass.dff<- -9999999

for ( r in unique(JJ$Rgroup)) {
  for (p in unique(JJ$YEARS)) {
    for (g in unique(JJ$GCM)) {
      for (rcp in unique(JJ$RCP)) {
        for (s in unique(JJ$site)) {
          JJ$Bmass.dff[which(JJ$Rgroup==r & JJ$YEARS==p & JJ$RCP==rcp & JJ$GCM==g & JJ$site==s)]<-
          JJ$Bmass[which(JJ$Rgroup==r & JJ$YEARS==p & JJ$RCP==rcp & JJ$GCM==g & JJ$site==s)]-
          current$Bmass[which(current$Rgroup==r & current$site==s)]
        }
      }
    }
  }
}

JJ2<-JJ
#JJ<-JJ[-which(JJ$Rgroup=="a.warm.grass"),]
a.forbs<-c("a.cool.forb","a.warm.forb")
p.forbs<-c("p.cool.forb","p.warm.forb")
JJ$Rgroup<-as.character(JJ$Rgroup)
#groups<-factor(unique(Stepwat.Sorted.GCM$RGroup))
#replace.groups<-c("a.forb","a.cool.grass","a.forb","P.forb","p.cool.grass","p.forb","p.warm.grass","sagebrush","shrub")
JJ$Rgroup[which(JJ$Rgroup%in%a.forbs)]<-"a.forb"
JJ$Rgroup[which(JJ$Rgroup%in%p.forbs)]<-"p.forb"
JJ$Rgroup<-factor(JJ$Rgroup)

GG<-with(JJ,aggregate(cbind(Bmass,Bmass.dff),by=list(Rgroup,RCP,GCM,YEARS),mean))
write.csv(GG,"output/which.GCM.csv")


DFF<-data.frame(Percent=0,RGroup=rep("D",2),Period=rep("D",2),Type=rep("D",2))
DFF$RGroup<-as.character(DFF$RGroup)
DFF$Period<-as.character(DFF$Period)
DFF$Type<-as.character(DFF$Type)

sink("output/anova.results.10.21.2022.txt")
for (r in unique(JJ$Rgroup)) {
  
  cat("###########################")
  cat("\n")  
  print(r)
  cat("###########################")
  cat("\n")  
  for (p in unique(JJ$YEARS)) {
    #DFF$Rgroup[b]<-r
    #DFF$Period[b]<-p
    cat("-------------------------")
    cat("\n")  
    print(p)
    cat("-------------------------")
    cat("\n")  
    print(anova(lm(Bmass.dff~RCP+GCM+site,data=JJ[which(JJ$Rgroup==r&JJ$YEARS==p),])))
    cat("\n")  
    
    XX<-(anova(lm(Bmass.dff~RCP+GCM+site,data=JJ[which(JJ$Rgroup==r&JJ$YEARS==p),]))[2]/sum(anova(lm(Bmass.dff~RCP+GCM+site,data=JJ[which(JJ$Rgroup==r&JJ$YEARS==p),]))[2]))*100
    names(XX)<-"Percent"
    XX$RGroup<-paste0(r)
    XX$Period<-paste0(p)
    XX$Type<-rownames(XX)
    DFF<-rbind(DFF,XX)
}
}
sink()

DF.var<-DFF[3:dim(DFF)[1],]

#DF.var$Type<-factor(DF.var$Type,levels=c("site","GCM","RCP","Residuals"),labels=c("Site","GCM","RCP","Residuals"))
DF.var$Type<-factor(DF.var$Type,
                    levels=c("site","GCM","GCM:site","RCP"
                             ,"RCP:site","RCP:GCM",
                             "RCP:GCM:site","Residuals"),
                    labels=c("Site","GCM","GCM:Site","RCP",
                             "RCP:Site","RCP:GCM",
                             "RCP:GCM:site","Residuals"))

DF.var$RGroup<-factor(DF.var$RGroup, 
                    levels=c("sagebrush","shrub","p.cool.grass", "p.warm.grass","a.cool.grass","p.forb","a.forb"),
                    labels=c("Sagebrush","Other\n shrubs","C3PG","C4PG","C3AG","PF","AF"))

DF.var$Period<-factor(DF.var$Period,
                    levels=c("d50yrs","d90yrs"),
                    labels=c("Mid-century","Late-century"))

variance.plot<-ggplot(data=DF.var,aes(x=RGroup,y=Percent, fill=Type)) +
  facet_grid(~Period) +
  geom_bar(stat="identity") + 
  labs(fill="Variance\n source")+
  theme_minimal(base_size=16) +
  xlab("")+
  scale_fill_manual(values=c("gold","green4","deeppink","grey40"))
  # scale_fill_manual(values=c("gold","green4","lawngreen","hotpink",
  #                            "violetred4","skyblue",
  #                            "navyblue","grey50"))
ggsave("figures/variance.21.10.2022.pdf",variance.plot, width = 40, height = 20, units = "cm")


DF.var$Percent[which(DF.var$Type=="Residuals"&DF.var$Period=="Mid-century")]
DF.var$Percent[which(DF.var$Type=="Residuals"&DF.var$Period=="Late-century")]

mean(DF.var$Percent[which(DF.var$Type=="GCM"&DF.var$Period=="Mid-century")])
sd(DF.var$Percent[which(DF.var$Type=="GCM"&DF.var$Period=="Mid-century")])
mean(DF.var$Percent[which(DF.var$Type=="GCM"&DF.var$Period=="Late-century")])
sd(DF.var$Percent[which(DF.var$Type=="GCM"&DF.var$Period=="Late-century")])

DF.var[which(DF.var$Type=="RCP"&DF.var$Period=="Mid-century"),]
DF.var[which(DF.var$Type=="RCP"&DF.var$Period=="Late-century"),]

DF.var[which(DF.var$Type=="Site"&DF.var$Period=="Mid-century"),]
DF.var[which(DF.var$Type=="Site"&DF.var$Period=="Late-century"),]
mean(DF.var$Percent[which(DF.var$Type=="Site"&DF.var$Period=="Mid-century")])
mean(DF.var$Percent[which(DF.var$Type=="Site"&DF.var$Period=="Late-century")])
