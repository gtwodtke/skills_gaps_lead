sink("D:\\projects\\skill_gaps_lead\\programs\\_LOGS\\16_create_figure_3_log.txt")

################################################
################################################
##                                            ##
## PROGRAM NAME: 16_create_figure_3           ##
## DESCRIPTION:                               ##
##                                            ##  
##  creates density plots for lead exposure   ##
##  by education                              ##
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
dcol1<-density(phdcnmi$navgbll_1[phdcnmi$npccolgrd_b==1],bw=0.6,from=0,to=13)
dnoc1<-density(phdcnmi$navgbll_1[phdcnmi$npccolgrd_b==0],bw=0.6,from=0,to=13)

dcol2<-density(phdcnmi$navgbll_2[phdcnmi$npccolgrd_b==1],bw=0.6,from=0,to=13)
dnoc2<-density(phdcnmi$navgbll_2[phdcnmi$npccolgrd_b==0],bw=0.6,from=0,to=13)

dcol3<-density(phdcnmi$navgbll_3[phdcnmi$npccolgrd_b==1],bw=0.6,from=0,to=13)
dnoc3<-density(phdcnmi$navgbll_3[phdcnmi$npccolgrd_b==0],bw=0.6,from=0,to=13)

dcolc<-density(phdcnmi$ncumbll[phdcnmi$npccolgrd_b==1],bw=0.6,from=0,to=13)
dnocc<-density(phdcnmi$ncumbll[phdcnmi$npccolgrd_b==0],bw=0.6,from=0,to=13)

### WALD TESTS ###
nmi<-50
miest.w1<-miest.w2<-miest.w3<-miest.cm<-matrix(data=NA,nrow=nmi,ncol=1)
mivar.w1<-mivar.w2<-mivar.w3<-mivar.cm<-matrix(data=NA,nrow=nmi,ncol=1)

for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	m.w1<-lm.cluster(navgbll_1~npccolgrd_b,cluster='nlinknc_1',data=phdcn)
	miest.w1[i,1]<-m.w1$lm_res$coefficients[2]
	mivar.w1[i,1]<-m.w1$vcov[2,2]

	m.w2<-lm.cluster(navgbll_2~npccolgrd_b,cluster='nlinknc_1',data=phdcn)
	miest.w2[i,1]<-m.w2$lm_res$coefficients[2]
	mivar.w2[i,1]<-m.w2$vcov[2,2]

	m.w3<-lm.cluster(navgbll_3~npccolgrd_b,cluster='nlinknc_1',data=phdcn)
	miest.w3[i,1]<-m.w3$lm_res$coefficients[2]
	mivar.w3[i,1]<-m.w3$vcov[2,2]

	m.cm<-lm.cluster(ncumbll~npccolgrd_b,cluster='nlinknc_1',data=phdcn)
	miest.cm[i,1]<-m.cm$lm_res$coefficients[2]
	mivar.cm[i,1]<-m.cm$vcov[2,2]
	}

z.w1<-round(mean(miest.w1)/sqrt(mean(mivar.w1)+(var(miest.w1)*(1+(1/nmi)))),digits=2)
pval.w1<-round((pnorm(abs(z.w1),0,1,lower.tail=FALSE)*2),digits=3)

z.w2<-round(mean(miest.w2)/sqrt(mean(mivar.w2)+(var(miest.w2)*(1+(1/nmi)))),digits=2)
pval.w2<-round((pnorm(abs(z.w2),0,1,lower.tail=FALSE)*2),digits=3)

z.w3<-round(mean(miest.w3)/sqrt(mean(mivar.w3)+(var(miest.w3)*(1+(1/nmi)))),digits=2)
pval.w3<-round((pnorm(abs(z.w3),0,1,lower.tail=FALSE)*2),digits=3)

z.cm<-round(mean(miest.cm)/sqrt(mean(mivar.cm)+(var(miest.cm)*(1+(1/nmi)))),digits=2)
pval.cm<-round((pnorm(abs(z.cm),0,1,lower.tail=FALSE)*2),digits=3)

### DENSITY PLOT ###
#tiff
tiff("D:\\projects\\skill_gaps_lead\\figures\\figure_3.tiff",width=8,height=8,units='in',res=600)
par(mfrow=c(2,2))

#wave 1
plot(dcol1,main="Wave 1 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5,lty="dotdash")
abline(v=mean(phdcnmi$navgbll_1[phdcnmi$npccolgrd_b==1]),lwd=1.5,lty="dotdash",col="gray")
abline(v=mean(phdcnmi$navgbll_1[phdcnmi$npccolgrd_b==0]),lwd=1.5,lty="longdash",col="gray")
par(new=T)
plot(dcol1,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotdash",lwd=1.5,axes=FALSE)
par(new=T)
plot(dnoc1,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="longdash",lwd=1.5,axes=FALSE)
legend("topright",inset=0.03,c("PCG with College Degree","PCG without College Degree"),lty=c(4,5),cex=0.85)
text(10.3,0.32,labels=expression("Wald" ~ Z ~ "="),cex=0.8)
text(12.1,0.32,labels=z.w1,cex=0.8)
if (pval.w1<0.001) {
	text(10.3,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.3,0.275,labels=expression("p ="),cex=0.8)	
		text(11.5,0.28,labels=pval.w1,cex=0.8)	
		}

#wave 2
par(new=F)
plot(dcol2,main="Wave 2 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5,lty="dotdash")
abline(v=mean(phdcnmi$navgbll_2[phdcnmi$npccolgrd_b==1]),lwd=1.5,lty="dotdash",col="gray")
abline(v=mean(phdcnmi$navgbll_2[phdcnmi$npccolgrd_b==0]),lwd=1.5,lty="longdash",col="gray")
par(new=T)
plot(dcol2,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotdash",lwd=1.5,axes=FALSE)
par(new=T)
plot(dnoc2,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="longdash",lwd=1.5,axes=FALSE)
text(10.3,0.32,labels=expression("Wald" ~ Z ~ "="),cex=0.8)
text(12.1,0.32,labels=z.w2,cex=0.8)
if (pval.w2<0.001) {
	text(10.3,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.3,0.275,labels=expression("p ="),cex=0.8)	
		text(11.5,0.28,labels=pval.w2,cex=0.8)	
		}

#wave 3
par(new=F)
plot(dcol3,main="Wave 3 Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5,lty="dotdash")
abline(v=mean(phdcnmi$navgbll_3[phdcnmi$npccolgrd_b==1]),lwd=1.5,lty="dotdash",col="gray")
abline(v=mean(phdcnmi$navgbll_3[phdcnmi$npccolgrd_b==0]),lwd=1.5,lty="longdash",col="gray")
par(new=T)
plot(dcol3,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotdash",lwd=1.5,axes=FALSE)
par(new=T)
plot(dnoc3,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="longdash",lwd=1.5,axes=FALSE)
text(10.3,0.32,labels=expression("Wald" ~ Z ~ "="),cex=0.8)
text(12.1,0.32,labels=z.w3,cex=0.8)
if (pval.w3<0.001) {
	text(10.3,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.3,0.275,labels=expression("p ="),cex=0.8)	
		text(11.5,0.28,labels=pval.w3,cex=0.8)	
		}

#cumulative
par(new=F)
plot(dcolc,main="Cumulative Exposure to Lead",xlab="Mean Blood Lead Level (ug/dL)",ylab="Density",xlim=c(0,13),ylim=c(0,0.5),lwd=1.5,lty="dotdash")
abline(v=mean(phdcnmi$ncumbll[phdcnmi$npccolgrd_b==1]),lwd=1.5,lty="dotdash",col="gray")
abline(v=mean(phdcnmi$ncumbll[phdcnmi$npccolgrd_b==0]),lwd=1.5,lty="longdash",col="gray")
par(new=T)
plot(dcolc,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="dotdash",lwd=1.5,axes=FALSE)
par(new=T)
plot(dnocc,main="",xlab="",ylab="",xlim=c(0,13),ylim=c(0,0.5),lty="longdash",lwd=1.5,axes=FALSE)
text(10.3,0.32,labels=expression("Wald" ~ Z ~ "="),cex=0.8)
text(12.1,0.32,labels=z.cm,cex=0.8)
if (pval.cm<0.001) {
	text(10.3,0.28,labels=expression(p<0.001),cex=0.8)
	} else {
		text(10.3,0.275,labels=expression("p ="),cex=0.8)	
		text(11.5,0.28,labels=pval.cm,cex=0.8)	
		}

dev.off()

sink()