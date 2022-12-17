################################################
################################################
##                                            ##
## PROGRAM NAME: 02_create_table_d1           ##
## AUTHOR: GW                                 ##
## DATE: 8/5/2021                             ##
## DESCRIPTION:                               ##
##                                            ##
##  creates figure of bias-adjusted PPVT      ##
##  gaps                                      ##
##                                            ##
################################################
################################################

rm(list=ls())
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

### LOAD ESTIMATES ###
load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\appendix.d.bias.est.RData")

##### COMPUTE BIAS-ADJUSTED COUNTERFACTUAL GAPS #####
psi<-seq(from=0,to=2,by=0.1)
bias.adj.bvw.ppvt.gap<-bias.adj.hvw.ppvt.gap<-bias.adj.nvd.ppvt.gap<-matrix(data=NA,nrow=length(psi),ncol=4)
bias.adj.bvw.attn.gap<-bias.adj.hvw.attn.gap<-bias.adj.nvd.attn.gap<-matrix(data=NA,nrow=length(psi),ncol=4)

# PPVT #
load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\ppvt.gap.est.RData")

for (i in 1:length(psi)) {
	bias.adj.bvw.ppvt.gap[i,1]<-psi[i]
	bias.adj.bvw.ppvt.gap[i,2]<-sl.dif.w1[1,1]-psi[i]*bias.est[1,1]
	bias.adj.bvw.ppvt.gap[i,3]<-bias.adj.bvw.ppvt.gap[i,2]-1.96*sl.dif.w1[1,2]
	bias.adj.bvw.ppvt.gap[i,4]<-bias.adj.bvw.ppvt.gap[i,2]+1.96*sl.dif.w1[1,2]

	bias.adj.hvw.ppvt.gap[i,1]<-psi[i]
	bias.adj.hvw.ppvt.gap[i,2]<-sl.dif.w1[2,1]-psi[i]*bias.est[2,1]
	bias.adj.hvw.ppvt.gap[i,3]<-bias.adj.hvw.ppvt.gap[i,2]-1.96*sl.dif.w1[2,2]
	bias.adj.hvw.ppvt.gap[i,4]<-bias.adj.hvw.ppvt.gap[i,2]+1.96*sl.dif.w1[2,2]

	bias.adj.nvd.ppvt.gap[i,1]<-psi[i]
	bias.adj.nvd.ppvt.gap[i,2]<-sl.dif.w1[3,1]-psi[i]*bias.est[3,1]
	bias.adj.nvd.ppvt.gap[i,3]<-bias.adj.nvd.ppvt.gap[i,2]-1.96*sl.dif.w1[3,2]
	bias.adj.nvd.ppvt.gap[i,4]<-bias.adj.nvd.ppvt.gap[i,2]+1.96*sl.dif.w1[3,2]
	}

# CBCL-AP #
load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\attn.gap.est.RData")

for (i in 1:length(psi)) {
	bias.adj.bvw.attn.gap[i,1]<-psi[i]
	bias.adj.bvw.attn.gap[i,2]<-sl.dif.w1[1,1]-psi[i]*bias.est[1,2]
	bias.adj.bvw.attn.gap[i,3]<-bias.adj.bvw.attn.gap[i,2]-1.96*sl.dif.w1[1,2]
	bias.adj.bvw.attn.gap[i,4]<-bias.adj.bvw.attn.gap[i,2]+1.96*sl.dif.w1[1,2]

	bias.adj.hvw.attn.gap[i,1]<-psi[i]
	bias.adj.hvw.attn.gap[i,2]<-sl.dif.w1[2,1]-psi[i]*bias.est[2,2]
	bias.adj.hvw.attn.gap[i,3]<-bias.adj.hvw.attn.gap[i,2]-1.96*sl.dif.w1[2,2]
	bias.adj.hvw.attn.gap[i,4]<-bias.adj.hvw.attn.gap[i,2]+1.96*sl.dif.w1[2,2]

	bias.adj.nvd.attn.gap[i,1]<-psi[i]
	bias.adj.nvd.attn.gap[i,2]<-sl.dif.w1[3,1]-psi[i]*bias.est[3,2]
	bias.adj.nvd.attn.gap[i,3]<-bias.adj.nvd.attn.gap[i,2]-1.96*sl.dif.w1[3,2]
	bias.adj.nvd.attn.gap[i,4]<-bias.adj.nvd.attn.gap[i,2]+1.96*sl.dif.w1[3,2]
	}

##### PRINT RESULTS #####
clabel<-c("psi","est","ll95ci","ul95ci")
colnames(bias.adj.bvw.ppvt.gap)<-colnames(bias.adj.hvw.ppvt.gap)<-colnames(bias.adj.nvd.ppvt.gap)<-clabel
colnames(bias.adj.bvw.attn.gap)<-colnames(bias.adj.hvw.attn.gap)<-colnames(bias.adj.nvd.attn.gap)<-clabel

sink("D:\\projects\\skill_gaps_lead\\programs\\appendix_d\\_LOGS\\02_create_figure_d1_log.txt")

cat("PPVT: Black vs White Gap\n")
print(bias.adj.bvw.ppvt.gap)
cat(" \n")
cat("PPVT: Hispanic vs White Gap\n")
print(bias.adj.hvw.ppvt.gap)
cat(" \n")
cat("PPVT: No College vs College\n")
print(bias.adj.nvd.ppvt.gap)
cat(" \n")
cat("CBCL-AP: Black vs White Gap\n")
print(bias.adj.bvw.attn.gap)
cat(" \n")
cat("CBCL-AP: Hispanic vs White Gap\n")
print(bias.adj.hvw.attn.gap)
cat(" \n")
cat("CBCL-AP: No College vs College\n")
print(bias.adj.nvd.attn.gap)

sink()

##### PLOT RESULTS #####
#dev.new(width=9,height=9)
tiff("D:\\projects\\skill_gaps_lead\\figures\\figure_d1.tiff",
	width=9,
	height=7,
	units='in',
	res=600)

par(mfrow=c(2,3))

par(mar=c(2.5,4,4,2))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.bvw.ppvt.gap[,1],bias.adj.bvw.ppvt.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.bvw.ppvt.gap[,1],bias.adj.bvw.ppvt.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.bvw.ppvt.gap[,1],bias.adj.bvw.ppvt.gap[,2],type="l",lty="solid")),
	main="Difference between Counterfactual and Observed\nBlack vs. White Gap",
	cex.main=0.95,
	xlab=" ",
	ylab="Standardized PPVT Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.hvw.ppvt.gap[,1],bias.adj.hvw.ppvt.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.hvw.ppvt.gap[,1],bias.adj.hvw.ppvt.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.hvw.ppvt.gap[,1],bias.adj.hvw.ppvt.gap[,2],type="l",lty="solid")),
	main="Difference between Counterfactual and Observed\nHispanic vs. White Gap",
	cex.main=0.95,
	xlab=" ",
	ylab="Standardized PPVT Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.nvd.ppvt.gap[,1],bias.adj.nvd.ppvt.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.nvd.ppvt.gap[,1],bias.adj.nvd.ppvt.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.nvd.ppvt.gap[,1],bias.adj.nvd.ppvt.gap[,2],type="l",lty="solid")),
	main="Difference between Counterfactual and Observed\nNo College vs. College Gap",
	cex.main=0.95,
	xlab=" ",
	ylab="Standardized PPVT Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

par(mar=c(5,4,2.5,2))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.bvw.attn.gap[,1],bias.adj.bvw.attn.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.bvw.attn.gap[,1],bias.adj.bvw.attn.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.bvw.attn.gap[,1],bias.adj.bvw.attn.gap[,2],type="l",lty="solid")),
	xlab="Multiples of Observed Bias",
	ylab="Standardized CBCL-AP Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.hvw.attn.gap[,1],bias.adj.hvw.attn.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.hvw.attn.gap[,1],bias.adj.hvw.attn.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.hvw.attn.gap[,1],bias.adj.hvw.attn.gap[,2],type="l",lty="solid")),
	xlab="Multiples of Observed Bias",
	ylab="Standardized CBCL-AP Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(bias.adj.nvd.attn.gap[,1],bias.adj.nvd.attn.gap[,3],type="l",lty="dashed"),
		  lines(bias.adj.nvd.attn.gap[,1],bias.adj.nvd.attn.gap[,4],type="l",lty="dashed"),
		  lines(bias.adj.nvd.attn.gap[,1],bias.adj.nvd.attn.gap[,2],type="l",lty="solid")),
	xlab="Multiples of Observed Bias",
	ylab="Standardized CBCL-AP Scores",
	xlim=c(0.0,2.0),
	ylim=c(-0.5,0.5))

dev.off()



