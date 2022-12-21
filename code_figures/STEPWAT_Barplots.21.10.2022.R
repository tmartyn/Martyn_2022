rm(list=ls())
version<-"21.10.2022"
#############################################################3
####### Code for Stepwat Figures
####### Trace Martyn 14.07.2017- Version 3
################################################################
############ read in CSVs#########################
Stepwat.Sorted.Site.Current<-read.csv(paste0("output/Stepwat.Sorted.Site.Current.Means.",version,".csv"))
Stepwat.Sorted.Site.B<-read.csv(paste0("output/Stepwat.Sorted.Site.Means.",version,".csv"))

library(ggplot2)

########### average last 100 years of data - Site & Current ########
source("code/STEPWAT_make.bar.plot.calcs.21.10.2022.R")

new3S<-new2
new3S$Rgroup<-factor(new3S$Rgroup, 
                     levels=c("sagebrush","shrub","p.cool.grass", "p.warm.grass","a.cool.grass","p.forb","a.forb"),
                     labels=c("Sagebrush","Other shrubs","C3PG","C4PG","C3AG","PF","AF"))
new3S$Period<-factor(new3S$Period,
                     levels=c("d50yrs","d90yrs"),
                     labels=c("Mid","Late"))
new3S<-new3S[order(new3S$Rgroup,new3S$Period),]

limits2<-aes(ymax=c(new3S[which(new3S$RCP=="RCP85"),]$med.diff+new3S[which(new3S$RCP=="RCP85"),]$sd),ymin=c(new3S[which(new3S$RCP=="RCP85"),]$med.diff-new3S[which(new3S$RCP=="RCP85"),]$sd))
labels<-c("Sagebrush","Other shrubs","C3PG","C4PG","C3AG","PF","AF")
datagg2<-data.frame(x=rep(1.5,7),y=rep(50,7),label=labels,Rgroup=unique(new3S$Rgroup))
BMASS.br<-ggplot(new3S[which(new3S$RCP=="RCP85"),], 
                aes(x=Period,y=med.diff, fill=Rgroup, colour=Rgroup)) +
  ggtitle("(A)")+
  theme_minimal(base_size = 16)+
  facet_grid(.~Rgroup) + 
  geom_bar(stat="identity",position="dodge") + 
  scale_colour_manual(values=c("black","navyblue","darkgreen","darkgoldenrod4",
                               "mediumpurple4","darkred","chocolate3"),guide="none") +
  scale_fill_manual(values=c("gray60","skyblue","palegreen","gold","mediumpurple1",
                             "indianred1","orange"),guide="none") + 
  geom_errorbar(limits2, position=position_dodge(.9),width=0.35) + 
  theme(panel.background = element_blank(),panel.grid.major=element_line(colour="gray80"),
        panel.grid.minor=element_line(colour="gray90"),strip.text.x = element_blank(),
        axis.text.x = element_text(size=13, color="black"), 
        axis.text.y = element_text(size=13,color="black")) +
  ylab(expression(Mean~biomass~difference~from~current~(g/m^2))) +ylim(-50,90)   + 
  geom_text(aes(x,y,label=unique(new3S$Rgroup),group=NULL),data=datagg2, angle=90, hjust=0, size=8)

source("code_figures/STEPWAT_functional.composition.21.10.2022.R")
library(patchwork)

combined<-BMASS.br+Composition_plot+ plot_layout(guides = "collect") & theme(legend.position = "bottom")

pdf(height=8,width=15,paste0("figures/RCP85_comp_bar.pdf"))
combined
dev.off()
