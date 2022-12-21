version<-"19.10.2020"
#############################################################3
####### Code for Stepwat Figures
####### From Kyle A Palmquist, edited by Trace Martyn 14.07.2017- Version 5
################################################################

library(scales)

Stepwat.Sorted<-read.csv(paste0("output/Stepwat.Sorted.GCM.Means.",version,".csv"))
Stepwat.Sorted.Current<-read.csv(paste0("output/Stepwat.Sorted.GCM.Current.Means.",version,".csv"))

SITE<-1:10
RCP<-c("RCP45","RCP85")
YEARS<-c("50years","90years")
GCM<-c("CanESM2","CESM1-CAM5","CSIRO-Mk3-6-0","FGOALS-g2","FGOALS-s2",
       "GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-MR",
       "MIROC5","MIROC-ESM","MRI-CGCM3")
sage.coords<-matrix(c(0, -1,
                      0, 960,
                      200, 960,
                      200, -1,
                      0, -1), 
                    ncol = 2, byrow = TRUE)

grass.coords<-matrix(c(0, -1,
                       0, 88,
                       200, 88,
                       200, -1,
                       0, -1), 
                     ncol = 2, byrow = TRUE)

forb.coords<-matrix(c(0, -1,
                      0, 53,
                      200, 53,
                      200, -1,
                      0, -1), 
                    ncol = 2, byrow = TRUE)




################ Make figure ##########################################
pdf(height=9, width=13, "figures/GCM_Bmass_RCP45.pdf",useDingbats=F)
#par(mar=c(4,4.5,4,5)+0.1)
par(mfrow=c(2,3), oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)

source("code_figures/RCP45/STEPWAT_GCM.RCP45.Variability.Figure.mid.century.24.09.2018.R")

source("code_figures/RCP45/STEPWAT_GCM.RCP45.Variability.Figure.late.century.24.09.2018.R")

legend(-800,-13,c("Future sagebrush mean","Current sagebrush mean","Future sagebrush range",
                  "Future other shrubs mean","Current other shrubs mean","Future other shrubs range",
                  "Future C3PG mean","Current C3PG mean","Future C3PG range",
                  "Future C4PG mean","Current C4PG mean","Future C4PG range",
                  "Future C3AG mean","Current C3AG mean","Future C3AG range",
                  "Future PF mean","Current PF mean", "Future PF range",
                "Future AF mean","Current AF mean","Future PF range"),
       lwd=c(3,1.5,10,3,1.5,10,
             3,1.5,10,3,1.5,10,3,1.5,10,
             3,1.5,10,3,1.5,10),
       col=c("black","black","gray60",
             "blue","blue","skyblue",
             "seagreen4","seagreen4","seagreen1",
             "darkgoldenrod","darkgoldenrod","lightgoldenrod1",
             "mediumpurple4","mediumpurple4","plum1",
             "darkred","darkred","indianred1",
             "chocolate3","chocolate3","orange"), bty="n",ncol=7,cex=1)


dev.off()
