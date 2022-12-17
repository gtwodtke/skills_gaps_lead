################################################
################################################
##                                            ##
## PROGRAM NAME: 03_create_figure_a1          ##
## AUTHOR: GW                                 ##
## DATE: 8/4/2021                             ##
## UPDATED: 3/23/2022 (GW)                    ##
## DESCRIPTION:                               ##
##                                            ##  
##  creates bar plots for observed and        ##
##  counterfactual gaps in PPVT scores        ##
##                                            ##
################################################
################################################

rm(list=ls())
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

### LOAD ESTIMATES ###
load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\appendix.a.ppvt.gap.est.RData")

### CREATE DATA FILE ###
output<-rbind(ob.gap,sl.gap.w1,sl.gap.w2,sl.gap.w3,sl.gap.cm)
colnames(output)<-c("est","se")
output$gap<-rep(c("Below vs. Above Median Income","No HS Diploma vs. Diploma"),5)
output$wave<-c(rep("Observed",2),rep("Eq. Wave 1",2),rep("Eq. Wave 2",2),rep("Eq. Wave 3",2),rep("Eq. Cumulative",2))
rownames(output)<-c()

### PRINT OUTPUT
sink("D:\\projects\\skill_gaps_lead\\programs\\appendix_a\\_LOGS\\03_create_figure_a1_log.txt")

cat("PPVT scores\n")
print(output)

sink()

### BAR PLOT ###
ggplot(output, aes(x=gap, y=est, fill=wave)) + 
	geom_bar(position=position_dodge(),stat="identity",colour="black",size=.3) +
	geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se),size=.3,width=.2,position=position_dodge(.9)) +
	xlab("Gap") +	
	ylab("Standardized PPVT Scores") +
	scale_y_continuous(limits=c(-1.0,0.1),breaks=seq(from=-1.0,to=0.1,by=0.1)) +
	scale_fill_grey(start=0.2,end=0.9,name="Estimand",
		breaks=c("Eq. Cumulative","Eq. Wave 1","Eq. Wave 2","Eq. Wave 3","Observed"),
		labels=c("Eq. Cum. Exposure","Eq. Wave 1 Exposure","Eq. Wave 2 Exposure","Eq. Wave 3 Exposure","Observed")) +
	theme_bw()

ggsave("D:\\projects\\skill_gaps_lead\\figures\\figure_a1.tiff",width=6.5,height=5)

