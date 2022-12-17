################################################
################################################
##                                            ##
## PROGRAM NAME: 01_create_table_d1           ##
## AUTHOR: GW                                 ##
## DATE: 8/5/2021                             ##
## DESCRIPTION:                               ##
##                                            ##
##  creates table of bias estimates for       ##
##  PPVT gaps                                 ##
##                                            ##
################################################
################################################

rm(list=ls())
library(haven)
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(ranger)
library(caret)
library(rpart)
library(SuperLearner)
library(doParallel)
library(doRNG)
library(foreach)
library(sys)

startTime<-Sys.time()

set.seed(8675309)

nmi<-50
ntrees<-200
vfolds<-20
m<-log(4)

##### LOAD PHDCN #####
phdcnmi<-read.dta("D:\\projects\\skill_gaps_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which(phdcnmi$"_mj"!=0),]

##### DEFINE VARIABLE SETS - EXCLUDE ALL CONTROLS #####
vars.w1<-c("nblack_b","nhispan_b","nothrace_b","npchsgrad_b","npcsomcol_b","npccolgrd_b","nlnbll_1")

##### TUNE ML HYPERPARAMETERS #####
registerDoSEQ()

phdcn.tune<-phdcnmi[which(phdcnmi$"_mj"==1),]

cntrl<-trainControl(method="CV",number=10)

rf.grid<-expand.grid(min.node.size=c(5,10,20),mtry=c(round(length(vars.w1)/2),round(length(vars.w1)/3),round(sqrt(length(vars.w1)))),splitrule="variance")
rf.ppvt<-train(nppvt_3~.,data=phdcn.tune[,c("nppvt_3",vars.w1)],method="ranger",tuneGrid=rf.grid,trControl=cntrl,metric="RMSE",respect.unordered.factors=TRUE,num.trees=ntrees,seed=8675309)
rf.attn<-train(natten_3~.,data=phdcn.tune[,c("natten_3",vars.w1)],method="ranger",tuneGrid=rf.grid,trControl=cntrl,metric="RMSE",respect.unordered.factors=TRUE,num.trees=ntrees,seed=8675309)
rf.sl.ppvt<-create.Learner("SL.ranger",name_prefix="rf.sl.ppvt",params=list(num.trees=ntrees,min.node.size=rf.ppvt$bestTune[,"min.node.size"],mtry=rf.ppvt$bestTune[,"mtry"],splitrule="variance",respect.unordered.factors=TRUE,seed=8675309))
rf.sl.attn<-create.Learner("SL.ranger",name_prefix="rf.sl.attn",params=list(num.trees=ntrees,min.node.size=rf.attn$bestTune[,"min.node.size"],mtry=rf.attn$bestTune[,"mtry"],splitrule="variance",respect.unordered.factors=TRUE,seed=8675309))

rt.grid<-expand.grid(cp=c(0.04,0.02,0.01,0.005,0.001,0.0005))
rt.ppvt<-train(nppvt_3~.,data=phdcn.tune[,c("nppvt_3",vars.w1)],method="rpart",tuneGrid=rt.grid,trControl=cntrl,metric="RMSE",minsplit=10,minbucket=4)
rt.attn<-train(natten_3~.,data=phdcn.tune[,c("natten_3",vars.w1)],method="rpart",tuneGrid=rt.grid,trControl=cntrl,metric="RMSE",minsplit=10,minbucket=4)
rt.sl.ppvt<-create.Learner("SL.rpart",name_prefix="rt.sl.ppvt",params=list(cp=rt.ppvt$bestTune[,"cp"],minsplit=10,minbucket=4,maxcomplete=0,maxsurrogate=0))
rt.sl.attn<-create.Learner("SL.rpart",name_prefix="rt.sl.attn",params=list(cp=rt.attn$bestTune[,"cp"],minsplit=10,minbucket=4,maxcomplete=0,maxsurrogate=0))

cv.control.sl<-SuperLearner.CV.control(V=vfolds)

##### ESTIMATE COUNTERFACTUAL GAPS #####
miest.sl.ppvt<-miest.sl.attn<-matrix(data=NA,nrow=nmi,ncol=3)

for (i in 1:nmi) {

	### LOAD MI DATA ###
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]
	print(i)

	### OBSERVED GAPS ###
	# PPVT #
	m0.race<-lm(nppvt_3~nblack_b+nhispan_b+nothrace_b,data=phdcn)
	m0.educ<-lm(nppvt_3~npccolgrd_b,data=phdcn)
	bvwgap.obs.ppvt<-m0.race$coefficients[2]
	hvwgap.obs.ppvt<-m0.race$coefficients[3]
	nvdgap.obs.ppvt<-m0.educ$coefficients[2]*(-1)

	# CBCL-AP #
	m0.race<-lm(natten_3~nblack_b+nhispan_b+nothrace_b,data=phdcn)
	m0.educ<-lm(natten_3~npccolgrd_b,data=phdcn)
	bvwgap.obs.attn<-m0.race$coefficients[2]
	hvwgap.obs.attn<-m0.race$coefficients[3]
	nvdgap.obs.attn<-m0.educ$coefficients[2]*(-1)

	### COMPUTE ESTIMATES ###

	# PPVT #
	y.train<-phdcn[,"nppvt_3"]
	x.train<-phdcn[,vars.w1]
	m1.sl.w1<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.ppvt$names,rf.sl.ppvt$names),cvControl=cv.control.sl)
	idata<-phdcn[,c("nppvt_3",vars.w1)]
	idata$nlnbll_1<-m	
	uhat<-predict(m1.sl.w1,idata)
	uhat.sl<-uhat$pred
	m2.sl.w1.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.w1.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	bvwgap.sl.ppvt<-m2.sl.w1.race$coefficients[2]
	hvwgap.sl.ppvt<-m2.sl.w1.race$coefficients[3]
	nvdgap.sl.ppvt<-m2.sl.w1.educ$coefficients[2]*(-1)

	# CBCL-AP #
	y.train<-phdcn[,"natten_3"]
	x.train<-phdcn[,vars.w1]
	m1.sl.w1<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.attn$names,rf.sl.attn$names),cvControl=cv.control.sl)
	idata<-phdcn[,c("natten_3",vars.w1)]
	idata$nlnbll_1<-m	
	uhat<-predict(m1.sl.w1,idata)
	uhat.sl<-uhat$pred
	m2.sl.w1.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.w1.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	bvwgap.sl.attn<-m2.sl.w1.race$coefficients[2]
	hvwgap.sl.attn<-m2.sl.w1.race$coefficients[3]
	nvdgap.sl.attn<-m2.sl.w1.educ$coefficients[2]*(-1)

	miest.sl.ppvt[i,1]<-bvwgap.sl.ppvt-bvwgap.obs.ppvt
	miest.sl.ppvt[i,2]<-hvwgap.sl.ppvt-hvwgap.obs.ppvt
	miest.sl.ppvt[i,3]<-nvdgap.sl.ppvt-nvdgap.obs.ppvt

	miest.sl.attn[i,1]<-bvwgap.sl.attn-bvwgap.obs.attn
	miest.sl.attn[i,2]<-hvwgap.sl.attn-hvwgap.obs.attn
	miest.sl.attn[i,3]<-nvdgap.sl.attn-nvdgap.obs.attn
	}

### COMBINE MI ESTIMATES ###
bias.dif<-matrix(data=NA,nrow=3,ncol=2)

for (i in 1:3) { 
	
	bias.dif[i,1]<-round(mean(miest.sl.ppvt[,i]),digits=3)
	bias.dif[i,2]<-round(mean(miest.sl.attn[,i]),digits=3)
	}

### PRINT RESULTS ###
rlabel<-c('BlackVsWhite.bias','HispanicVsWhite.bias','NoDegreeVsDegree.bias')
bias.est<-data.frame(bias.dif,row.names=rlabel)
colnames(bias.est)<-c('PPVT','CBCL-AP')

load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\ppvt.gap.est.RData")
bias.est[,1]<-bias.dif[,1]-sl.dif.w1[,1]

load("D:\\projects\\skill_gaps_lead\\data\\_TEMP\\attn.gap.est.RData")
bias.est[,2]<-bias.dif[,2]-sl.dif.w1[,1]

sink("D:\\projects\\skill_gaps_lead\\programs\\appendix_d\\_LOGS\\01_create_table_d1_log.txt")

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Omitted Variable Bias\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
print(bias.est)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")

print(startTime)
print(Sys.time())

sink()

##### SAVE ESTIMATES #####
save(bias.est,file="D:\\projects\\skill_gaps_lead\\data\\_TEMP\\appendix.d.bias.est.RData")

