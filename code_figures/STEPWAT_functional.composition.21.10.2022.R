#rm(list=ls())
version<-"21.10.2022"
########### Site Variability ####################################
########## make a new dataframe of median max and min values ###

#############################################################3
####### Code for Stepwat Figures
####### Trace Martyn 14.07.2017- Version 3
################################################################

library(ggplot2)

##################################################
############ read in CSVs#########################
GCM.means<-read.csv(paste0("output/Means.GCM.",version,".csv"))

#GCM<-unique(GCM.means$GCM) 
#GCM<-GCM[-which(GCM=="0")]
Site<- 1:10

Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass",
          "p.cool.grass","p.warm.grass","shrub")
Means<-data.frame(ID=1:400)
#SITE<-1:10
RCP<-unique(GCM.means$RCP)
Period<-unique(GCM.means$Period)
GCM<-unique(GCM.means$GCM)
Years<-200:300

total.C<-sum(GCM.means$M.Bmass[which(GCM.means$Period=="0")])
GCM.means$props<-0

for (g in GCM){
  for (p in Period) {
    for (r in RCP) {
      total<-sum(GCM.means$M.Bmass[which(GCM.means$Period==p & GCM.means$GCM==g & GCM.means$RCP==r)])
      GCM.means$props[which(GCM.means$Period==p & GCM.means$GCM==g & GCM.means$RCP==r)]<-(GCM.means$M.Bmass[which(GCM.means$Period==p & GCM.means$GCM==g & GCM.means$RCP==r)]/total)*100
    }
  }
}

total.C<-sum(GCM.means$M.Bmass[which(GCM.means$Period=="0")])
GCM.means$props[which(GCM.means$Period=="0")]<-(GCM.means$M.Bmass[which(GCM.means$Period=="0")]/total.C)*100


GCM.means[which(GCM.means$Period=="0"),]

GCM.means[which(GCM.means$Period=="50years" & GCM.means$RCP=="RCP85"),]

GG<-aggregate(GCM.means$props,by=list(GCM.means$RCP,GCM.means$Period,GCM.means$Rgroup),mean)

names(GG)<-c("RCP","Period","RGroup","Prop")
### need to combine frobs into P and A but keep C3/C4 grasses

#GG<-GG[-which(GG$RGroup=="a.warm.grass"),]

GG$RGroup<-factor(GG$RGroup, 
                  levels=c("sagebrush","shrub","p.cool.grass", "p.warm.grass","a.cool.grass",
                           "p.cool.forb","p.warm.forb",
                           "a.cool.forb","a.warm.forb"),
                  labels=c("Sagebrush","Other shrubs","C3PG","C4PG","C3AG","PF","PF","AF","AF"))


PP<-aggregate(GG$Prop,by=list(GG$RCP,GG$Period,GG$RGroup),sum)
names(PP)<-c("RCP","Period","RGroup","Prop")




#PP<-PP[-which(PP$RGroup=="a.warm.grass"),]

# PP$RGroup<-factor(PP$RGroup, 
#                   levels=c("sagebrush","shrub","p.cool.grass", "p.warm.grass","a.cool.grass","p.forb","a.forb"),
#                   labels=c("Sagebrush","Other shrubs","C3PG","C4PG","C3AG","PF","AF"))
# #new2$Period<-factor(new3$Period, labels=c("mid","late"))

PP$Period<-factor(PP$Period,levels=c("0","50years","90years"),labels=c("Current","Mid-century","Late-century"))

to.use<-c("RCP85","0")

## need to clean up a bit
Composition_plot<-ggplot(PP[which(PP$RCP%in%to.use),],aes(Period)) + geom_bar(aes(fill=RGroup,weight=Prop)) +
  facet_grid(.~RCP, scales="free_x", space="free") + 
  theme_minimal(base_size = 16) + 
  ylab("Percent Composition") +
  ggtitle("(B)")+
  scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4",
                               "mediumpurple4","darkred","chocolate3"),guide="none") +
  scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1",
                             "indianred1","orange")) +  
  labs(fill="Functional type") +
  guides(fill=guide_legend(nrow=1))+
  theme(panel.background = element_blank(),panel.grid.major=element_line(colour="gray80"),
        panel.grid.minor=element_line(colour="gray90"),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=11, color="black"), 
        axis.text.y = element_text(size=11,color="black"),
        legend.position="bottom")

#ggsave("figures/85_GCM_Current_Composition.pdf", width = 15, height = 15, units = "cm")

# to.use<-c("RCP45","0")
# 
# ## need to clean up a bit
# ggplot(PP[which(PP$RCP%in%to.use),],aes(Period)) + geom_bar(aes(fill=RGroup,weight=Prop)) +
#   facet_grid(.~RCP, scales="free_x", space="free") + theme_minimal() + ylab("Percent Composition") +
#   scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4","mediumpurple4","darkred","chocolate3"),guide=F) +
#   scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1","indianred1","orange")) +  labs(fill="Functional type") +
#   theme(panel.background = element_blank(),panel.grid.major=element_line(colour="gray80"),panel.grid.minor=element_line(colour="gray90"),strip.text.x = element_blank(),
#         axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11,color="black"))
# 
# 
# ggsave("figures/45_GCM_Current_Composition.pdf", width = 15, height = 15, units = "cm")

######### all to make sure they are different

# ## need to clean up a bit
# ggplot(PP,aes(Period)) + geom_bar(aes(fill=RGroup,weight=Prop)) +
#   facet_grid(.~RCP, scales="free_x", space="free") + theme_minimal() + ylab("Percent Composition") +
#   scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4","mediumpurple4","darkred","chocolate3"),guide=F) +
#   scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1","indianred1","orange")) +  labs(fill="Functional type") +
#   theme(panel.background = element_blank(),panel.grid.major=element_line(colour="gray80"),panel.grid.minor=element_line(colour="gray90"),strip.text.x = element_blank(),
#         axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11,color="black"))
# 
# ggsave("figures/ALL_RCP_GCM_Current_Composition.pdf", width = 15, height = 15, units = "cm")
# 
