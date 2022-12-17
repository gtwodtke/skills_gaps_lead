sink("D:\\projects\\skill_gaps_lead\\programs\\_LOGS\\15_create_figure_2_log.txt")

################################################
################################################
##                                            ##
## PROGRAM NAME: 16_create_figure_2           ##
## DESCRIPTION:                               ##
##                                            ##  
##  creates density plots for lead exposure   ##
##  by race                                   ##
##                                            ##
################################################
################################################

rm(list=ls())
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(miceadds)
library(sandwich)
library(lmtest)

### LOAD PHDCN ###
phdcnmi<-read_dta("D:\\projects\\skill_gaps_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which((phdcnmi$"_mj"!=0 & phdcnmi$nrace_b!=2)),]

### RECODE VARIABLES ###
phdcnmi$navgbll_1<-exp(phdcnmi$nlnbll_1)
phdcnmi$navgbll_2<-exp(phdcnmi$nlnbll_2)
phdcnmi$navgbll_3<-exp(phdcnmi$nlnbll_3)
phdcnmi$ncumbll<-(phdcnmi$navgbll_1+phdcnmi$navgbll_2+phdcnmi$navgbll_3)/3

### COMPUTE DENSITIES ###
dwht1<-density(phdcnmi$navgbll_1[phdcnmi$nrace_b==4],bw=0.6,from=0,to=13)
dblk1<-density(phdcnmi$navgbll_1[phdcnmi$nrace_b==3],bw=0.6,from=0,to=13)
dhsp1<-density(phdcnmi$navgbll_1[phdcnmi$nrace_b==1],bw=0.6,from=0,to=13)

dwht2<-density(phdcnmi$navgbll_2[phdcnmi$nrace_b==4],bw=0.6,from=0,to=13)
dblk2<-density(phdcnmi$navgbll_2[phdcnmi$nrace_b==3],bw=0.6,from=0,to=13)
dhsp2<-density(phdcnmi$navgbll_2[phdcnmi$nrace_b==1],bw=0.6,from=0,to=13)

dwht3<-density(phdcnmi$navgbll_3[phdcnmi$nrace_b==4],bw=0.6,from=0,to=13)
dblk3<-density(phdcnmi$navgbll_3[phdcnmi$nrace_b==3],bw=0.6,from=0,to=13)
dhsp3<-density(phdcnmi$navgbll_3[phdcnmi$nrace_b==1],bw=0.6,from=0,to=13)

dwhtc<-density(phdcnmi$ncumbll[phdcnmi$nrace_b==4],bw=0.6,from=0,to=13)
dblkc<-density(phdcnmi$ncumbll[phdcnmi$nrace_b==3],bw=0.6,from=0,to=13)
dhspc<-density(phdcnmi$ncumbll[phdcnmi$nrace_b==1],bw=0.6,from=0,to=13)

### WALD TESTS ###
nmi<-50
miest.w1<-miest.w2<-miest.w3<-miest.cm<-matrix(data=NA,nrow=nmi,ncol=3)
mivar.w1<-mivar.w2<-mivar.w3<-mivar.cm<-matrix(data=0,nrow=3,ncol=3)

for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	m.w1<-lm.cluster(navgbll_1~nblack_b+nhispan_b,cluster='nlinknc_1',data=phdcn)
	miest.w1[i,]<-m.w1$lm_res$coefficients
	mivar.w1<-mivar.w1+(m.w1$vcov*(1/nmi))

	m.w2<-lm.cluster(navgbll_2~nblack_b+nhispan_b,cluster='nlinknc_1',data=phdcn)
	miest.w2[i,]<-m.w2$lm_res$coefficients
	mivar.w2<-mivar.w2+(m.w2$vcov*(1/nmi))

	m.w3<-lm.cluster(navgbll_3~nblack_b+nhispan_b,cluster='nlinknc_1',data=phdcn)
	miest.w3[i,]<-m.w3$lm_res$coefficients
	mivar.w3<-mivar.w3+(m.w3$vcov*(1/nmi))

	m.cm<-lm.cluster(ncumbll~nblack_b+nhispan_b,cluster='nlinknc_1',data=phdcn)
	miest.cm[i,]<-m.cm$lm_res$coefficients
	mivar.cm<-mivar.cm+(m.cm$vcov*(1/nmi))
	}

R<-diag(2)

vcov.w1<-as.matrix(mivar.w1+cov(miest.w1)*(1+(1/nmi)))
coef.w1<-as.matrix(colMeans(miest.w1))
chi2.w1<-round(t(R%*%coef.w1[2:3,1])%*%(solve(R%*%vcov.w1[2:3,2:3]%*%t(R)))%*%(R%*%coef.w1[2:3,1]),digits=1)
pval.w1<-round(pchisq(chi2.w1,2,lower.tail=F),digits=3)

vcov.w2<-as.matrix(mivar.w2+cov(miest.w2)*(1+(1/nmi)))
coef.w2<-as.matrix(colMeans(miest.w2))
chi2.w2<-round(t(R%*%coef.w2[2:3,1])%*%(solve(R%*%vcov.w2[2:3,2:3]%*%t(R)))%*%(R%*%coef.w2[2:3,1]),digits=1)
pval.w2<-round(pchisq(chi2.w2,2,lower.tail=F),digits=3)

vcov.w3<-as.matrix(mivar.w3+cov(miest.w3)*(1+(1/nmi)))
coef.w3<-as.matrix(colMeans(miest.w3))
chi2.w3<-round(t(R%*%coef.w3[2:3,1])%*%(solve(R%*%vcov.w3[2:3,2:3]%*%t(R)))%*%(R%*%coef.w3[2:3,1]),digits=1)
pval.w3<-round(pchisq(chi2.w3,2,lower.tail=F),digits=3)

vcov.cm<-as.matrix(mivar.cm+cov(miest.cm)*(1+(1/nmi)))
coef.cm<-as.matrix(colMeans(miest.cm))
chi2.cm<-round(t(R%*%coef.cm[2:3,1])%*%(solve(R%*%vcov.cm[2:3,2:3]%*%t(R)))%*%(R%*%coef.cm[2:3,1]),digits=1)
pval.cm<-round(pchisq(chi2.cm,2,lower.tail=F),digits=3)

### DENSITY PLOT ###
#tiff
tiff("D:\\projects\\skill_gaps_lead\\figures\\figure_2.tiff",width=8,height=8,units='in',res=600)
par(mfrow=c(2,2))

#wave 1
plot(dwht1,main="Wave 1 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5)
abline(v=mean(phdcnmi$navgbll_1[phdcnmi$nrace_b==4]),lwd=1.5,lty="solid",col="gray")
abline(v=mean(phdcnmi$navgbll_1[phdcnmi$nrace_b==3]),lwd=1.5,lty="dashed",col="gray")
abline(v=mean(phdcnmi$navgbll_1[phdcnmi$nrace_b==1]),lwd=1.5,lty="dotted",col="gray")
par(new=T)
plot(dwht1,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="solid",lwd=1.5,axes=FALSE)
par(new=T)
plot(dblk1,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dashed",lwd=1.5,axes=FALSE)
par(new=T)
plot(dhsp1,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotted",lwd=1.5,axes=FALSE)
legend("topright",inset=0.03,c("White","Hispanic","Black"),lty=c(1,3,2),cex=0.8)
text(10.5,0.32,labels=expression("Wald" ~ chi^2 ~ "="),cex=0.8)
text(12.3,0.32,labels=chi2.w1,cex=0.8)
if (pval.w1<0.001) {
	text(10.5,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.5,0.275,labels=expression("p ="),cex=0.8)	
		text(11.7,0.28,labels=pval.w1,cex=0.8)	
		}

#wave 2
par(new=F)
plot(dwht2,main="Wave 2 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5)
abline(v=mean(phdcnmi$navgbll_2[phdcnmi$nrace_b==4]),lwd=1.5,lty="solid",col="gray")
abline(v=mean(phdcnmi$navgbll_2[phdcnmi$nrace_b==3]),lwd=1.5,lty="dashed",col="gray")
abline(v=mean(phdcnmi$navgbll_2[phdcnmi$nrace_b==1]),lwd=1.5,lty="dotted",col="gray")
par(new=T)
plot(dwht2,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="solid",lwd=1.5,axes=FALSE)
par(new=T)
plot(dblk2,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dashed",lwd=1.5,axes=FALSE)
par(new=T)
plot(dhsp2,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotted",lwd=1.5,axes=FALSE)
text(10.5,0.32,labels=expression("Wald" ~ chi^2 ~ "="),cex=0.8)
text(12.3,0.32,labels=chi2.w2,cex=0.8)
if (pval.w2<0.001) {
	text(10.5,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.5,0.275,labels=expression("p ="),cex=0.8)	
		text(11.7,0.28,labels=pval.w2,cex=0.8)	
		}

#wave 3
par(new=F)
plot(dwht3,main="Wave 3 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5)
abline(v=mean(phdcnmi$navgbll_3[phdcnmi$nrace_b==4]),lwd=1.5,lty="solid",col="gray")
abline(v=mean(phdcnmi$navgbll_3[phdcnmi$nrace_b==3]),lwd=1.5,lty="dashed",col="gray")
abline(v=mean(phdcnmi$navgbll_3[phdcnmi$nrace_b==1]),lwd=1.5,lty="dotted",col="gray")
par(new=T)
plot(dwht3,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="solid",lwd=1.5,axes=FALSE)
par(new=T)
plot(dblk3,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dashed",lwd=1.5,axes=FALSE)
par(new=T)
plot(dhsp3,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotted",lwd=1.5,axes=FALSE)
text(10.5,0.32,labels=expression("Wald" ~ chi^2 ~ "="),cex=0.8)
text(12.3,0.32,labels=chi2.w3,cex=0.8)
if (pval.w3<0.001) {
	text(10.5,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.5,0.275,labels=expression("p ="),cex=0.8)	
		text(11.7,0.28,labels=pval.w3,cex=0.8)	
		}
#cumulative
par(new=F)
plot(dwhtc,main="Cumulative Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5)
abline(v=mean(phdcnmi$ncumbll[phdcnmi$nrace_b==4]),lwd=1.5,lty="solid",col="gray")
abline(v=mean(phdcnmi$ncumbll[phdcnmi$nrace_b==3]),lwd=1.5,lty="dashed",col="gray")
abline(v=mean(phdcnmi$ncumbll[phdcnmi$nrace_b==1]),lwd=1.5,lty="dotted",col="gray")
par(new=T)
plot(dwhtc,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="solid",lwd=1.5,axes=FALSE)
par(new=T)
plot(dblkc,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dashed",lwd=1.5,axes=FALSE)
par(new=T)
plot(dhspc,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotted",lwd=1.5,axes=FALSE)
text(10.5,0.32,labels=expression("Wald" ~ chi^2 ~ "="),cex=0.8)
text(12.3,0.32,labels=chi2.cm,cex=0.8)
if (pval.cm<0.001) {
	text(10.5,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.5,0.275,labels=expression("p ="),cex=0.8)	
		text(11.7,0.28,labels=pval.cm,cex=0.8)	
		}

dev.off()

sink()