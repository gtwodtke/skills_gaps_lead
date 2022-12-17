################################################
################################################
##                                            ##
## PROGRAM NAME: 05_create_table_b5           ##
## AUTHOR: GW                                 ##
## DATE: 3/21/2021                            ##
## DESCRIPTION:                               ##
##                                            ##
##  creates table of gap-closing estimates    ##
##  for ATTN scores under point-in-time and   ##
##  cumulative interventions at avg bll m=2   ##
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
library(ebal)
library(CBPS)
library(IDPmisc)
library(doParallel)
library(doRNG)
library(foreach)
library(sys)

startTime<-Sys.time()

set.seed(8675309)

nmi<-50
nboot<-200
ntrees<-200
vfolds<-20
save.cores<-0
m<-log(2)


#########################
# POINT-IN-TIME EFFECTS #
#########################

##### LOAD PHDCN #####
phdcnmi<-read.dta("D:\\projects\\skill_gaps_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which(phdcnmi$"_mj"!=0),]
phdcnmi<-phdcnmi[order(phdcnmi$"_mj",phdcnmi$nsubid),]

##### DEFINE VARIABLE SETS #####
vars.w1<-c("natten_3",
	     "nblack_b","nhispan_b","nothrace_b",
           "npcage_b","npchsgrad_b","npcsomcol_b","npccolgrd_b","nownhome_b","nfemale_b","nfamsize_b",
           "nlnbll_1","nlninc_1","npcemply_1","npcwelf_1","npcmarr_1","npcengl_1","ncondadvg_1","nsubage_3")
vars.w2<-c(vars.w1,c("nlnbll_2","nlninc_2","npcemply_2","npcwelf_2","npcmarr_2","npcengl_2","ncondadvg_2"))
vars.w3<-c(vars.w2,c("nlnbll_3","nlninc_3","npcemply_3","npcwelf_3","npcmarr_3","npcengl_3","ncondadvg_3"))

##### TUNE ML HYPERPARAMETERS #####
registerDoSEQ()

phdcn.tune<-phdcnmi[which(phdcnmi$"_mj"==1),]

cntrl<-trainControl(method="CV",number=10)

rf.grid.w1<-expand.grid(min.node.size=c(5,10,20),mtry=c(round(length(vars.w1[-1])/2),round(length(vars.w1[-1])/3),round(sqrt(length(vars.w1[-1])))),splitrule="variance")
rf.grid.w2<-expand.grid(min.node.size=c(5,10,20),mtry=c(round(length(vars.w2[-1])/2),round(length(vars.w2[-1])/3),round(sqrt(length(vars.w2[-1])))),splitrule="variance")
rf.grid.w3<-expand.grid(min.node.size=c(5,10,20),mtry=c(round(length(vars.w3[-1])/2),round(length(vars.w3[-1])/3),round(sqrt(length(vars.w3[-1])))),splitrule="variance")

rf.w1<-train(natten_3~.,data=phdcn.tune[,vars.w1],method="ranger",tuneGrid=rf.grid.w1,trControl=cntrl,metric="RMSE",respect.unordered.factors=TRUE,num.trees=ntrees,seed=8675309)
rf.w2<-train(natten_3~.,data=phdcn.tune[,vars.w2],method="ranger",tuneGrid=rf.grid.w2,trControl=cntrl,metric="RMSE",respect.unordered.factors=TRUE,num.trees=ntrees,seed=8675309)
rf.w3<-train(natten_3~.,data=phdcn.tune[,vars.w3],method="ranger",tuneGrid=rf.grid.w3,trControl=cntrl,metric="RMSE",respect.unordered.factors=TRUE,num.trees=ntrees,seed=8675309)

rf.sl.w1<-create.Learner("SL.ranger",name_prefix="rf.sl.w1",params=list(num.trees=ntrees,min.node.size=rf.w1$bestTune[,"min.node.size"],mtry=rf.w1$bestTune[,"mtry"],splitrule="variance",respect.unordered.factors=TRUE,seed=8675309))
rf.sl.w2<-create.Learner("SL.ranger",name_prefix="rf.sl.w2",params=list(num.trees=ntrees,min.node.size=rf.w2$bestTune[,"min.node.size"],mtry=rf.w2$bestTune[,"mtry"],splitrule="variance",respect.unordered.factors=TRUE,seed=8675309))
rf.sl.w3<-create.Learner("SL.ranger",name_prefix="rf.sl.w3",params=list(num.trees=ntrees,min.node.size=rf.w3$bestTune[,"min.node.size"],mtry=rf.w3$bestTune[,"mtry"],splitrule="variance",respect.unordered.factors=TRUE,seed=8675309))

rt.grid<-expand.grid(cp=c(0.01,0.005,0.001,0.0005))

rt.w1<-train(natten_3~.,data=phdcn.tune[,vars.w1],method="rpart",tuneGrid=rt.grid,trControl=cntrl,metric="RMSE",minsplit=2,minbucket=1)
rt.w2<-train(natten_3~.,data=phdcn.tune[,vars.w2],method="rpart",tuneGrid=rt.grid,trControl=cntrl,metric="RMSE",minsplit=2,minbucket=1)
rt.w3<-train(natten_3~.,data=phdcn.tune[,vars.w3],method="rpart",tuneGrid=rt.grid,trControl=cntrl,metric="RMSE",minsplit=2,minbucket=1)

rt.sl.w1<-create.Learner("SL.rpart",name_prefix="rt.sl.w1",params=list(cp=rt.w1$bestTune[,"cp"],minsplit=2,minbucket=1,maxcomplete=0,maxsurrogate=0))
rt.sl.w2<-create.Learner("SL.rpart",name_prefix="rt.sl.w2",params=list(cp=rt.w2$bestTune[,"cp"],minsplit=2,minbucket=1,maxcomplete=0,maxsurrogate=0))
rt.sl.w3<-create.Learner("SL.rpart",name_prefix="rt.sl.w3",params=list(cp=rt.w3$bestTune[,"cp"],minsplit=2,minbucket=1,maxcomplete=0,maxsurrogate=0))

cv.control.sl<-SuperLearner.CV.control(V=vfolds)

##### PARALLELIZATION #####
n.cores<-parallel::detectCores()-save.cores
my.cluster<-parallel::makeCluster(n.cores,type="PSOCK")
doParallel::registerDoParallel(cl=my.cluster)
foreach::getDoParRegistered()
clusterEvalQ(cl=my.cluster, library(SuperLearner))
registerDoRNG(8675309)

##### ESTIMATE COUNTERFACTUAL GAPS #####
miest.ob<-mivar.ob<-matrix(data=NA,nrow=nmi,ncol=3)

miest.lm.w1<-mivar.lm.w1<-miest.lm.w2<-mivar.lm.w2<-miest.lm.w3<-mivar.lm.w3<-matrix(data=NA,nrow=nmi,ncol=3)
miest.rt.w1<-mivar.rt.w1<-miest.rt.w2<-mivar.rt.w2<-miest.rt.w3<-mivar.rt.w3<-matrix(data=NA,nrow=nmi,ncol=3)
miest.rf.w1<-mivar.rf.w1<-miest.rf.w2<-mivar.rf.w2<-miest.rf.w3<-mivar.rf.w3<-matrix(data=NA,nrow=nmi,ncol=3)
miest.sl.w1<-mivar.sl.w1<-miest.sl.w2<-mivar.sl.w2<-miest.sl.w3<-mivar.sl.w3<-matrix(data=NA,nrow=nmi,ncol=3)

midest.lm.w1<-midvar.lm.w1<-midest.lm.w2<-midvar.lm.w2<-midest.lm.w3<-midvar.lm.w3<-matrix(data=NA,nrow=nmi,ncol=3)
midest.rt.w1<-midvar.rt.w1<-midest.rt.w2<-midvar.rt.w2<-midest.rt.w3<-midvar.rt.w3<-matrix(data=NA,nrow=nmi,ncol=3)
midest.rf.w1<-midvar.rf.w1<-midest.rf.w2<-midvar.rf.w2<-midest.rf.w3<-midvar.rf.w3<-matrix(data=NA,nrow=nmi,ncol=3)
midest.sl.w1<-midvar.sl.w1<-midest.sl.w2<-midvar.sl.w2<-midest.sl.w3<-midvar.sl.w3<-matrix(data=NA,nrow=nmi,ncol=3)

for (i in 1:nmi) {

	### LOAD MI DATA ###
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]
	print(i)

	### OBSERVED GAPS ###
	m0.race<-lm(natten_3~nblack_b+nhispan_b+nothrace_b,data=phdcn)
	m0.educ<-lm(natten_3~npccolgrd_b,data=phdcn)
	bvwgap.obs<-m0.race$coefficients[2]
	hvwgap.obs<-m0.race$coefficients[3]
	nvdgap.obs<-m0.educ$coefficients[2]*(-1)

	miest.ob[i,1]<-bvwgap.obs
	miest.ob[i,2]<-hvwgap.obs
	miest.ob[i,3]<-nvdgap.obs

	### COUNTERFACTUAL GAPS ###
	# Wave 1 #
	y.train<-phdcn[,"natten_3"]
	x.train<-phdcn[,vars.w1[-1]]
	m1.sl.w1<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.w1$names,rf.sl.w1$names),cvControl=cv.control.sl)
	idata<-phdcn[,vars.w1]
	idata$nlnbll_1<-m	
	uhat<-predict(m1.sl.w1,idata)
	uhat.lm<-uhat$library.predict[,1]
	uhat.rt<-uhat$library.predict[,2]
	uhat.rf<-uhat$library.predict[,3]
	uhat.sl<-uhat$pred
	m2.lm.w1.race<-lm(uhat.lm~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.lm.w1.educ<-lm(uhat.lm~npccolgrd_b,data=idata)
	miest.lm.w1[i,1]<-m2.lm.w1.race$coefficients[2]
	miest.lm.w1[i,2]<-m2.lm.w1.race$coefficients[3]
	miest.lm.w1[i,3]<-m2.lm.w1.educ$coefficients[2]*(-1)
	m2.rt.w1.race<-lm(uhat.rt~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rt.w1.educ<-lm(uhat.rt~npccolgrd_b,data=idata)
	miest.rt.w1[i,1]<-m2.rt.w1.race$coefficients[2]
	miest.rt.w1[i,2]<-m2.rt.w1.race$coefficients[3]
	miest.rt.w1[i,3]<-m2.rt.w1.educ$coefficients[2]*(-1)
	m2.rf.w1.race<-lm(uhat.rf~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rf.w1.educ<-lm(uhat.rf~npccolgrd_b,data=idata)
	miest.rf.w1[i,1]<-m2.rf.w1.race$coefficients[2]
	miest.rf.w1[i,2]<-m2.rf.w1.race$coefficients[3]
	miest.rf.w1[i,3]<-m2.rf.w1.educ$coefficients[2]*(-1)
	m2.sl.w1.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.w1.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	miest.sl.w1[i,1]<-m2.sl.w1.race$coefficients[2]
	miest.sl.w1[i,2]<-m2.sl.w1.race$coefficients[3]
	miest.sl.w1[i,3]<-m2.sl.w1.educ$coefficients[2]*(-1)

	# Wave 2 #
	y.train<-phdcn[,"natten_3"]
	x.train<-phdcn[,vars.w2[-1]]
	m1.sl.w2<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.w2$names,rf.sl.w2$names),cvControl=cv.control.sl)
	idata<-phdcn[,vars.w2]
	idata$nlnbll_2<-m	
	uhat<-predict(m1.sl.w2,idata)
	uhat.lm<-uhat$library.predict[,1]
	uhat.rt<-uhat$library.predict[,2]
	uhat.rf<-uhat$library.predict[,3]
	uhat.sl<-uhat$pred
	m2.lm.w2.race<-lm(uhat.lm~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.lm.w2.educ<-lm(uhat.lm~npccolgrd_b,data=idata)
	miest.lm.w2[i,1]<-m2.lm.w2.race$coefficients[2]
	miest.lm.w2[i,2]<-m2.lm.w2.race$coefficients[3]
	miest.lm.w2[i,3]<-m2.lm.w2.educ$coefficients[2]*(-1)
	m2.rt.w2.race<-lm(uhat.rt~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rt.w2.educ<-lm(uhat.rt~npccolgrd_b,data=idata)
	miest.rt.w2[i,1]<-m2.rt.w2.race$coefficients[2]
	miest.rt.w2[i,2]<-m2.rt.w2.race$coefficients[3]
	miest.rt.w2[i,3]<-m2.rt.w2.educ$coefficients[2]*(-1)
	m2.rf.w2.race<-lm(uhat.rf~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rf.w2.educ<-lm(uhat.rf~npccolgrd_b,data=idata)
	miest.rf.w2[i,1]<-m2.rf.w2.race$coefficients[2]
	miest.rf.w2[i,2]<-m2.rf.w2.race$coefficients[3]
	miest.rf.w2[i,3]<-m2.rf.w2.educ$coefficients[2]*(-1)
	m2.sl.w2.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.w2.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	miest.sl.w2[i,1]<-m2.sl.w2.race$coefficients[2]
	miest.sl.w2[i,2]<-m2.sl.w2.race$coefficients[3]
	miest.sl.w2[i,3]<-m2.sl.w2.educ$coefficients[2]*(-1)

	# Wave 3 #
	y.train<-phdcn[,"natten_3"]
	x.train<-phdcn[,vars.w3[-1]]
	m1.sl.w3<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.w3$names,rf.sl.w3$names),cvControl=cv.control.sl)
	idata<-phdcn[,vars.w3]
	idata$nlnbll_3<-m	
	uhat<-predict(m1.sl.w3,idata)
	uhat.lm<-uhat$library.predict[,1]
	uhat.rt<-uhat$library.predict[,2]
	uhat.rf<-uhat$library.predict[,3]
	uhat.sl<-uhat$pred
	m2.lm.w3.race<-lm(uhat.lm~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.lm.w3.educ<-lm(uhat.lm~npccolgrd_b,data=idata)
	miest.lm.w3[i,1]<-m2.lm.w3.race$coefficients[2]
	miest.lm.w3[i,2]<-m2.lm.w3.race$coefficients[3]
	miest.lm.w3[i,3]<-m2.lm.w3.educ$coefficients[2]*(-1)
	m2.rt.w3.race<-lm(uhat.rt~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rt.w3.educ<-lm(uhat.rt~npccolgrd_b,data=idata)
	miest.rt.w3[i,1]<-m2.rt.w3.race$coefficients[2]
	miest.rt.w3[i,2]<-m2.rt.w3.race$coefficients[3]
	miest.rt.w3[i,3]<-m2.rt.w3.educ$coefficients[2]*(-1)
	m2.rf.w3.race<-lm(uhat.rf~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rf.w3.educ<-lm(uhat.rf~npccolgrd_b,data=idata)
	miest.rf.w3[i,1]<-m2.rf.w3.race$coefficients[2]
	miest.rf.w3[i,2]<-m2.rf.w3.race$coefficients[3]
	miest.rf.w3[i,3]<-m2.rf.w3.educ$coefficients[2]*(-1)
	m2.sl.w3.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.w3.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	miest.sl.w3[i,1]<-m2.sl.w3.race$coefficients[2]
	miest.sl.w3[i,2]<-m2.sl.w3.race$coefficients[3]
	miest.sl.w3[i,3]<-m2.sl.w3.educ$coefficients[2]*(-1)

	midest.lm.w1[i,1]<-miest.lm.w1[i,1]-bvwgap.obs
	midest.lm.w1[i,2]<-miest.lm.w1[i,2]-hvwgap.obs
	midest.lm.w1[i,3]<-miest.lm.w1[i,3]-nvdgap.obs
	midest.lm.w2[i,1]<-miest.lm.w2[i,1]-bvwgap.obs
	midest.lm.w2[i,2]<-miest.lm.w2[i,2]-hvwgap.obs
	midest.lm.w2[i,3]<-miest.lm.w2[i,3]-nvdgap.obs
	midest.lm.w3[i,1]<-miest.lm.w3[i,1]-bvwgap.obs
	midest.lm.w3[i,2]<-miest.lm.w3[i,2]-hvwgap.obs
	midest.lm.w3[i,3]<-miest.lm.w3[i,3]-nvdgap.obs

	midest.rt.w1[i,1]<-miest.rt.w1[i,1]-bvwgap.obs
	midest.rt.w1[i,2]<-miest.rt.w1[i,2]-hvwgap.obs
	midest.rt.w1[i,3]<-miest.rt.w1[i,3]-nvdgap.obs
	midest.rt.w2[i,1]<-miest.rt.w2[i,1]-bvwgap.obs
	midest.rt.w2[i,2]<-miest.rt.w2[i,2]-hvwgap.obs
	midest.rt.w2[i,3]<-miest.rt.w2[i,3]-nvdgap.obs
	midest.rt.w3[i,1]<-miest.rt.w3[i,1]-bvwgap.obs
	midest.rt.w3[i,2]<-miest.rt.w3[i,2]-hvwgap.obs
	midest.rt.w3[i,3]<-miest.rt.w3[i,3]-nvdgap.obs

	midest.rf.w1[i,1]<-miest.rf.w1[i,1]-bvwgap.obs
	midest.rf.w1[i,2]<-miest.rf.w1[i,2]-hvwgap.obs
	midest.rf.w1[i,3]<-miest.rf.w1[i,3]-nvdgap.obs
	midest.rf.w2[i,1]<-miest.rf.w2[i,1]-bvwgap.obs
	midest.rf.w2[i,2]<-miest.rf.w2[i,2]-hvwgap.obs
	midest.rf.w2[i,3]<-miest.rf.w2[i,3]-nvdgap.obs
	midest.rf.w3[i,1]<-miest.rf.w3[i,1]-bvwgap.obs
	midest.rf.w3[i,2]<-miest.rf.w3[i,2]-hvwgap.obs
	midest.rf.w3[i,3]<-miest.rf.w3[i,3]-nvdgap.obs

	midest.sl.w1[i,1]<-miest.sl.w1[i,1]-bvwgap.obs
	midest.sl.w1[i,2]<-miest.sl.w1[i,2]-hvwgap.obs
	midest.sl.w1[i,3]<-miest.sl.w1[i,3]-nvdgap.obs
	midest.sl.w2[i,1]<-miest.sl.w2[i,1]-bvwgap.obs
	midest.sl.w2[i,2]<-miest.sl.w2[i,2]-hvwgap.obs
	midest.sl.w2[i,3]<-miest.sl.w2[i,3]-nvdgap.obs
	midest.sl.w3[i,1]<-miest.sl.w3[i,1]-bvwgap.obs
	midest.sl.w3[i,2]<-miest.sl.w3[i,2]-hvwgap.obs
	midest.sl.w3[i,3]<-miest.sl.w3[i,3]-nvdgap.obs

	clusterExport(cl=my.cluster,
		list(
			"vars.w1",
			"vars.w2",
			"vars.w3",
			"nmi",
			"nboot",
			"m",
			"ntrees",
			"phdcn",
			"rf.sl.w1",
			"rf.sl.w2",
			"rf.sl.w3",
			"rt.sl.w1",
			"rt.sl.w2",
			"rt.sl.w3",
			"rf.sl.w1_1",
			"rf.sl.w2_1",
			"rf.sl.w3_1",
			"rt.sl.w1_1",
			"rt.sl.w2_1",
			"rt.sl.w3_1"),
		envir=environment())
		
	### BOOTSTRAP SEs ###
	boot.est<-foreach(h=1:nboot, .combine=cbind) %dopar% {

		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (g in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% g]),]
				for (l in 1:g) { 
					boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) 
					}
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.m0.race<-lm(natten_3~nblack_b+nhispan_b+nothrace_b,data=boot.phdcn)
		boot.m0.educ<-lm(natten_3~npccolgrd_b,data=boot.phdcn)
		boot.bvwgap.obs<-boot.m0.race$coefficients[2]
		boot.hvwgap.obs<-boot.m0.race$coefficients[3]
		boot.nvdgap.obs<-boot.m0.educ$coefficients[2]*(-1)

		boot.y.train<-boot.phdcn[,"natten_3"]
		boot.x.train<-boot.phdcn[,vars.w1[-1]]
		boot.m1.sl.w1<-SuperLearner(Y=boot.y.train,X=boot.x.train,SL.library=c("SL.lm",rt.sl.w1$names,rf.sl.w1$names),cvControl=cv.control.sl)
		boot.idata<-boot.phdcn[,vars.w1]
		boot.idata$nlnbll_1<-m	
		boot.uhat<-predict(boot.m1.sl.w1,boot.idata)
		boot.uhat.lm<-boot.uhat$library.predict[,1]
		boot.uhat.rt<-boot.uhat$library.predict[,2]
		boot.uhat.rf<-boot.uhat$library.predict[,3]
		boot.uhat.sl<-boot.uhat$pred
		boot.m2.lm.w1.race<-lm(boot.uhat.lm~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.lm.w1.educ<-lm(boot.uhat.lm~npccolgrd_b,data=boot.idata)
		boot.bvwgap.lm.w1<-boot.m2.lm.w1.race$coefficients[2]
		boot.hvwgap.lm.w1<-boot.m2.lm.w1.race$coefficients[3]
		boot.nvdgap.lm.w1<-boot.m2.lm.w1.educ$coefficients[2]*(-1)
		boot.m2.rt.w1.race<-lm(boot.uhat.rt~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rt.w1.educ<-lm(boot.uhat.rt~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rt.w1<-boot.m2.rt.w1.race$coefficients[2]
		boot.hvwgap.rt.w1<-boot.m2.rt.w1.race$coefficients[3]
		boot.nvdgap.rt.w1<-boot.m2.rt.w1.educ$coefficients[2]*(-1)
		boot.m2.rf.w1.race<-lm(boot.uhat.rf~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rf.w1.educ<-lm(boot.uhat.rf~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rf.w1<-boot.m2.rf.w1.race$coefficients[2]
		boot.hvwgap.rf.w1<-boot.m2.rf.w1.race$coefficients[3]
		boot.nvdgap.rf.w1<-boot.m2.rf.w1.educ$coefficients[2]*(-1)
		boot.m2.sl.w1.race<-lm(boot.uhat.sl~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.sl.w1.educ<-lm(boot.uhat.sl~npccolgrd_b,data=boot.idata)
		boot.bvwgap.sl.w1<-boot.m2.sl.w1.race$coefficients[2]
		boot.hvwgap.sl.w1<-boot.m2.sl.w1.race$coefficients[3]
		boot.nvdgap.sl.w1<-boot.m2.sl.w1.educ$coefficients[2]*(-1)

		boot.y.train<-boot.phdcn[,"natten_3"]
		boot.x.train<-boot.phdcn[,vars.w2[-1]]
		boot.m1.sl.w2<-SuperLearner(Y=boot.y.train,X=boot.x.train,SL.library=c("SL.lm",rt.sl.w2$names,rf.sl.w2$names),cvControl=cv.control.sl)
		boot.idata<-boot.phdcn[,vars.w2]
		boot.idata$nlnbll_2<-m	
		boot.uhat<-predict(boot.m1.sl.w2,boot.idata)
		boot.uhat.lm<-boot.uhat$library.predict[,1]
		boot.uhat.rt<-boot.uhat$library.predict[,2]
		boot.uhat.rf<-boot.uhat$library.predict[,3]
		boot.uhat.sl<-boot.uhat$pred
		boot.m2.lm.w2.race<-lm(boot.uhat.lm~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.lm.w2.educ<-lm(boot.uhat.lm~npccolgrd_b,data=boot.idata)
		boot.bvwgap.lm.w2<-boot.m2.lm.w2.race$coefficients[2]
		boot.hvwgap.lm.w2<-boot.m2.lm.w2.race$coefficients[3]
		boot.nvdgap.lm.w2<-boot.m2.lm.w2.educ$coefficients[2]*(-1)
		boot.m2.rt.w2.race<-lm(boot.uhat.rt~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rt.w2.educ<-lm(boot.uhat.rt~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rt.w2<-boot.m2.rt.w2.race$coefficients[2]
		boot.hvwgap.rt.w2<-boot.m2.rt.w2.race$coefficients[3]
		boot.nvdgap.rt.w2<-boot.m2.rt.w2.educ$coefficients[2]*(-1)
		boot.m2.rf.w2.race<-lm(boot.uhat.rf~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rf.w2.educ<-lm(boot.uhat.rf~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rf.w2<-boot.m2.rf.w2.race$coefficients[2]
		boot.hvwgap.rf.w2<-boot.m2.rf.w2.race$coefficients[3]
		boot.nvdgap.rf.w2<-boot.m2.rf.w2.educ$coefficients[2]*(-1)
		boot.m2.sl.w2.race<-lm(boot.uhat.sl~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.sl.w2.educ<-lm(boot.uhat.sl~npccolgrd_b,data=boot.idata)
		boot.bvwgap.sl.w2<-boot.m2.sl.w2.race$coefficients[2]
		boot.hvwgap.sl.w2<-boot.m2.sl.w2.race$coefficients[3]
		boot.nvdgap.sl.w2<-boot.m2.sl.w2.educ$coefficients[2]*(-1)

		boot.y.train<-boot.phdcn[,"natten_3"]
		boot.x.train<-boot.phdcn[,vars.w3[-1]]
		boot.m1.sl.w3<-SuperLearner(Y=boot.y.train,X=boot.x.train,SL.library=c("SL.lm",rt.sl.w3$names,rf.sl.w3$names),cvControl=cv.control.sl)
		boot.idata<-boot.phdcn[,vars.w3]
		boot.idata$nlnbll_3<-m	
		boot.uhat<-predict(boot.m1.sl.w3,boot.idata)
		boot.uhat.lm<-boot.uhat$library.predict[,1]
		boot.uhat.rt<-boot.uhat$library.predict[,2]
		boot.uhat.rf<-boot.uhat$library.predict[,3]
		boot.uhat.sl<-boot.uhat$pred
		boot.m2.lm.w3.race<-lm(boot.uhat.lm~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.lm.w3.educ<-lm(boot.uhat.lm~npccolgrd_b,data=boot.idata)
		boot.bvwgap.lm.w3<-boot.m2.lm.w3.race$coefficients[2]
		boot.hvwgap.lm.w3<-boot.m2.lm.w3.race$coefficients[3]
		boot.nvdgap.lm.w3<-boot.m2.lm.w3.educ$coefficients[2]*(-1)
		boot.m2.rt.w3.race<-lm(boot.uhat.rt~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rt.w3.educ<-lm(boot.uhat.rt~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rt.w3<-boot.m2.rt.w3.race$coefficients[2]
		boot.hvwgap.rt.w3<-boot.m2.rt.w3.race$coefficients[3]
		boot.nvdgap.rt.w3<-boot.m2.rt.w3.educ$coefficients[2]*(-1)
		boot.m2.rf.w3.race<-lm(boot.uhat.rf~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rf.w3.educ<-lm(boot.uhat.rf~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rf.w3<-boot.m2.rf.w3.race$coefficients[2]
		boot.hvwgap.rf.w3<-boot.m2.rf.w3.race$coefficients[3]
		boot.nvdgap.rf.w3<-boot.m2.rf.w3.educ$coefficients[2]*(-1)
		boot.m2.sl.w3.race<-lm(boot.uhat.sl~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.sl.w3.educ<-lm(boot.uhat.sl~npccolgrd_b,data=boot.idata)
		boot.bvwgap.sl.w3<-boot.m2.sl.w3.race$coefficients[2]
		boot.hvwgap.sl.w3<-boot.m2.sl.w3.race$coefficients[3]
		boot.nvdgap.sl.w3<-boot.m2.sl.w3.educ$coefficients[2]*(-1)

		boot.bvwgap.lm.w1.d<-boot.bvwgap.lm.w1-boot.bvwgap.obs
		boot.hvwgap.lm.w1.d<-boot.hvwgap.lm.w1-boot.hvwgap.obs
		boot.nvdgap.lm.w1.d<-boot.nvdgap.lm.w1-boot.nvdgap.obs
		boot.bvwgap.lm.w2.d<-boot.bvwgap.lm.w2-boot.bvwgap.obs
		boot.hvwgap.lm.w2.d<-boot.hvwgap.lm.w2-boot.hvwgap.obs
		boot.nvdgap.lm.w2.d<-boot.nvdgap.lm.w2-boot.nvdgap.obs
		boot.bvwgap.lm.w3.d<-boot.bvwgap.lm.w3-boot.bvwgap.obs
		boot.hvwgap.lm.w3.d<-boot.hvwgap.lm.w3-boot.hvwgap.obs
		boot.nvdgap.lm.w3.d<-boot.nvdgap.lm.w3-boot.nvdgap.obs

		boot.bvwgap.rt.w1.d<-boot.bvwgap.rt.w1-boot.bvwgap.obs
		boot.hvwgap.rt.w1.d<-boot.hvwgap.rt.w1-boot.hvwgap.obs
		boot.nvdgap.rt.w1.d<-boot.nvdgap.rt.w1-boot.nvdgap.obs
		boot.bvwgap.rt.w2.d<-boot.bvwgap.rt.w2-boot.bvwgap.obs
		boot.hvwgap.rt.w2.d<-boot.hvwgap.rt.w2-boot.hvwgap.obs
		boot.nvdgap.rt.w2.d<-boot.nvdgap.rt.w2-boot.nvdgap.obs
		boot.bvwgap.rt.w3.d<-boot.bvwgap.rt.w3-boot.bvwgap.obs
		boot.hvwgap.rt.w3.d<-boot.hvwgap.rt.w3-boot.hvwgap.obs
		boot.nvdgap.rt.w3.d<-boot.nvdgap.rt.w3-boot.nvdgap.obs

		boot.bvwgap.rf.w1.d<-boot.bvwgap.rf.w1-boot.bvwgap.obs
		boot.hvwgap.rf.w1.d<-boot.hvwgap.rf.w1-boot.hvwgap.obs
		boot.nvdgap.rf.w1.d<-boot.nvdgap.rf.w1-boot.nvdgap.obs
		boot.bvwgap.rf.w2.d<-boot.bvwgap.rf.w2-boot.bvwgap.obs
		boot.hvwgap.rf.w2.d<-boot.hvwgap.rf.w2-boot.hvwgap.obs
		boot.nvdgap.rf.w2.d<-boot.nvdgap.rf.w2-boot.nvdgap.obs
		boot.bvwgap.rf.w3.d<-boot.bvwgap.rf.w3-boot.bvwgap.obs
		boot.hvwgap.rf.w3.d<-boot.hvwgap.rf.w3-boot.hvwgap.obs
		boot.nvdgap.rf.w3.d<-boot.nvdgap.rf.w3-boot.nvdgap.obs

		boot.bvwgap.sl.w1.d<-boot.bvwgap.sl.w1-boot.bvwgap.obs
		boot.hvwgap.sl.w1.d<-boot.hvwgap.sl.w1-boot.hvwgap.obs
		boot.nvdgap.sl.w1.d<-boot.nvdgap.sl.w1-boot.nvdgap.obs
		boot.bvwgap.sl.w2.d<-boot.bvwgap.sl.w2-boot.bvwgap.obs
		boot.hvwgap.sl.w2.d<-boot.hvwgap.sl.w2-boot.hvwgap.obs
		boot.nvdgap.sl.w2.d<-boot.nvdgap.sl.w2-boot.nvdgap.obs
		boot.bvwgap.sl.w3.d<-boot.bvwgap.sl.w3-boot.bvwgap.obs
		boot.hvwgap.sl.w3.d<-boot.hvwgap.sl.w3-boot.hvwgap.obs
		boot.nvdgap.sl.w3.d<-boot.nvdgap.sl.w3-boot.nvdgap.obs

		return(
			list(
				boot.bvwgap.obs,boot.hvwgap.obs,boot.nvdgap.obs,
				boot.bvwgap.lm.w1,boot.hvwgap.lm.w1,boot.nvdgap.lm.w1,
				boot.bvwgap.lm.w2,boot.hvwgap.lm.w2,boot.nvdgap.lm.w2,
				boot.bvwgap.lm.w3,boot.hvwgap.lm.w3,boot.nvdgap.lm.w3,
				boot.bvwgap.rt.w1,boot.hvwgap.rt.w1,boot.nvdgap.rt.w1,
				boot.bvwgap.rt.w2,boot.hvwgap.rt.w2,boot.nvdgap.rt.w2,
				boot.bvwgap.rt.w3,boot.hvwgap.rt.w3,boot.nvdgap.rt.w3,
				boot.bvwgap.rf.w1,boot.hvwgap.rf.w1,boot.nvdgap.rf.w1,
				boot.bvwgap.rf.w2,boot.hvwgap.rf.w2,boot.nvdgap.rf.w2,
				boot.bvwgap.rf.w3,boot.hvwgap.rf.w3,boot.nvdgap.rf.w3,
				boot.bvwgap.sl.w1,boot.hvwgap.sl.w1,boot.nvdgap.sl.w1,
				boot.bvwgap.sl.w2,boot.hvwgap.sl.w2,boot.nvdgap.sl.w2,
				boot.bvwgap.sl.w3,boot.hvwgap.sl.w3,boot.nvdgap.sl.w3,
				boot.bvwgap.lm.w1.d,boot.hvwgap.lm.w1.d,boot.nvdgap.lm.w1.d,
				boot.bvwgap.lm.w2.d,boot.hvwgap.lm.w2.d,boot.nvdgap.lm.w2.d,
				boot.bvwgap.lm.w3.d,boot.hvwgap.lm.w3.d,boot.nvdgap.lm.w3.d,
				boot.bvwgap.rt.w1.d,boot.hvwgap.rt.w1.d,boot.nvdgap.rt.w1.d,
				boot.bvwgap.rt.w2.d,boot.hvwgap.rt.w2.d,boot.nvdgap.rt.w2.d,
				boot.bvwgap.rt.w3.d,boot.hvwgap.rt.w3.d,boot.nvdgap.rt.w3.d,
				boot.bvwgap.rf.w1.d,boot.hvwgap.rf.w1.d,boot.nvdgap.rf.w1.d,
				boot.bvwgap.rf.w2.d,boot.hvwgap.rf.w2.d,boot.nvdgap.rf.w2.d,
				boot.bvwgap.rf.w3.d,boot.hvwgap.rf.w3.d,boot.nvdgap.rf.w3.d,
				boot.bvwgap.sl.w1.d,boot.hvwgap.sl.w1.d,boot.nvdgap.sl.w1.d,
				boot.bvwgap.sl.w2.d,boot.hvwgap.sl.w2.d,boot.nvdgap.sl.w2.d,
				boot.bvwgap.sl.w3.d,boot.hvwgap.sl.w3.d,boot.nvdgap.sl.w3.d))
		}

	boot.est<-matrix(unlist(boot.est),ncol=75,byrow=TRUE)

	mivar.ob[i,1]<-var(boot.est[,1])
	mivar.ob[i,2]<-var(boot.est[,2])
	mivar.ob[i,3]<-var(boot.est[,3])
	
	mivar.lm.w1[i,1]<-var(boot.est[,4])
	mivar.lm.w1[i,2]<-var(boot.est[,5])
	mivar.lm.w1[i,3]<-var(boot.est[,6])
	mivar.lm.w2[i,1]<-var(boot.est[,7])
	mivar.lm.w2[i,2]<-var(boot.est[,8])
	mivar.lm.w2[i,3]<-var(boot.est[,9])
	mivar.lm.w3[i,1]<-var(boot.est[,10])
	mivar.lm.w3[i,2]<-var(boot.est[,11])
	mivar.lm.w3[i,3]<-var(boot.est[,12])

	mivar.rt.w1[i,1]<-var(boot.est[,13])
	mivar.rt.w1[i,2]<-var(boot.est[,14])
	mivar.rt.w1[i,3]<-var(boot.est[,15])
	mivar.rt.w2[i,1]<-var(boot.est[,16])
	mivar.rt.w2[i,2]<-var(boot.est[,17])
	mivar.rt.w2[i,3]<-var(boot.est[,18])
	mivar.rt.w3[i,1]<-var(boot.est[,19])
	mivar.rt.w3[i,2]<-var(boot.est[,20])
	mivar.rt.w3[i,3]<-var(boot.est[,21])

	mivar.rf.w1[i,1]<-var(boot.est[,22])
	mivar.rf.w1[i,2]<-var(boot.est[,23])
	mivar.rf.w1[i,3]<-var(boot.est[,24])
	mivar.rf.w2[i,1]<-var(boot.est[,25])
	mivar.rf.w2[i,2]<-var(boot.est[,26])
	mivar.rf.w2[i,3]<-var(boot.est[,27])
	mivar.rf.w3[i,1]<-var(boot.est[,28])
	mivar.rf.w3[i,2]<-var(boot.est[,29])
	mivar.rf.w3[i,3]<-var(boot.est[,30])

	mivar.sl.w1[i,1]<-var(boot.est[,31])
	mivar.sl.w1[i,2]<-var(boot.est[,32])
	mivar.sl.w1[i,3]<-var(boot.est[,33])
	mivar.sl.w2[i,1]<-var(boot.est[,34])
	mivar.sl.w2[i,2]<-var(boot.est[,35])
	mivar.sl.w2[i,3]<-var(boot.est[,36])
	mivar.sl.w3[i,1]<-var(boot.est[,37])
	mivar.sl.w3[i,2]<-var(boot.est[,38])
	mivar.sl.w3[i,3]<-var(boot.est[,39])

	midvar.lm.w1[i,1]<-var(boot.est[,40])
	midvar.lm.w1[i,2]<-var(boot.est[,41])
	midvar.lm.w1[i,3]<-var(boot.est[,42])
	midvar.lm.w2[i,1]<-var(boot.est[,43])
	midvar.lm.w2[i,2]<-var(boot.est[,44])
	midvar.lm.w2[i,3]<-var(boot.est[,45])
	midvar.lm.w3[i,1]<-var(boot.est[,46])
	midvar.lm.w3[i,2]<-var(boot.est[,47])
	midvar.lm.w3[i,3]<-var(boot.est[,48])

	midvar.rt.w1[i,1]<-var(boot.est[,49])
	midvar.rt.w1[i,2]<-var(boot.est[,50])
	midvar.rt.w1[i,3]<-var(boot.est[,51])
	midvar.rt.w2[i,1]<-var(boot.est[,52])
	midvar.rt.w2[i,2]<-var(boot.est[,53])
	midvar.rt.w2[i,3]<-var(boot.est[,54])
	midvar.rt.w3[i,1]<-var(boot.est[,55])
	midvar.rt.w3[i,2]<-var(boot.est[,56])
	midvar.rt.w3[i,3]<-var(boot.est[,57])

	midvar.rf.w1[i,1]<-var(boot.est[,58])
	midvar.rf.w1[i,2]<-var(boot.est[,59])
	midvar.rf.w1[i,3]<-var(boot.est[,60])
	midvar.rf.w2[i,1]<-var(boot.est[,61])
	midvar.rf.w2[i,2]<-var(boot.est[,62])
	midvar.rf.w2[i,3]<-var(boot.est[,63])
	midvar.rf.w3[i,1]<-var(boot.est[,64])
	midvar.rf.w3[i,2]<-var(boot.est[,65])
	midvar.rf.w3[i,3]<-var(boot.est[,66])

	midvar.sl.w1[i,1]<-var(boot.est[,67])
	midvar.sl.w1[i,2]<-var(boot.est[,68])
	midvar.sl.w1[i,3]<-var(boot.est[,69])
	midvar.sl.w2[i,1]<-var(boot.est[,70])
	midvar.sl.w2[i,2]<-var(boot.est[,71])
	midvar.sl.w2[i,3]<-var(boot.est[,72])
	midvar.sl.w3[i,1]<-var(boot.est[,73])
	midvar.sl.w3[i,2]<-var(boot.est[,74])
	midvar.sl.w3[i,3]<-var(boot.est[,75])
	}

### COMBINE MI ESTIMATES ###
ob.est<-matrix(data=NA,nrow=3,ncol=2)

lm.est.w1<-lm.est.w2<-lm.est.w3<-matrix(data=NA,nrow=3,ncol=2)
rt.est.w1<-rt.est.w2<-rt.est.w3<-matrix(data=NA,nrow=3,ncol=2)
rf.est.w1<-rf.est.w2<-rf.est.w3<-matrix(data=NA,nrow=3,ncol=2)
sl.est.w1<-sl.est.w2<-sl.est.w3<-matrix(data=NA,nrow=3,ncol=2)

lm.dif.w1<-lm.dif.w2<-lm.dif.w3<-matrix(data=NA,nrow=3,ncol=4)
rt.dif.w1<-rt.dif.w2<-rt.dif.w3<-matrix(data=NA,nrow=3,ncol=4)
rf.dif.w1<-rf.dif.w2<-rf.dif.w3<-matrix(data=NA,nrow=3,ncol=4)
sl.dif.w1<-sl.dif.w2<-sl.dif.w3<-matrix(data=NA,nrow=3,ncol=4)

for (i in 1:3) { 

	# OBSERVED GAPS #
	ob.est[i,1]<-round(mean(miest.ob[,i]),digits=3)
	ob.est[i,2]<-round(sqrt(mean(mivar.ob[,i])+(var(miest.ob[,i])*(1+(1/nmi)))),digits=3)
	
	# LM ESTIMATES #
	lm.est.w1[i,1]<-round(mean(miest.lm.w1[,i]),digits=3)
	lm.est.w1[i,2]<-round(sqrt(mean(mivar.lm.w1[,i])+(var(miest.lm.w1[,i])*(1+(1/nmi)))),digits=3)

	lm.est.w2[i,1]<-round(mean(miest.lm.w2[,i]),digits=3)
	lm.est.w2[i,2]<-round(sqrt(mean(mivar.lm.w2[,i])+(var(miest.lm.w2[,i])*(1+(1/nmi)))),digits=3)

	lm.est.w3[i,1]<-round(mean(miest.lm.w3[,i]),digits=3)
	lm.est.w3[i,2]<-round(sqrt(mean(mivar.lm.w3[,i])+(var(miest.lm.w3[,i])*(1+(1/nmi)))),digits=3)

	# REG TREE ESTIMATES #
	rt.est.w1[i,1]<-round(mean(miest.rt.w1[,i]),digits=3)
	rt.est.w1[i,2]<-round(sqrt(mean(mivar.rt.w1[,i])+(var(miest.rt.w1[,i])*(1+(1/nmi)))),digits=3)

	rt.est.w2[i,1]<-round(mean(miest.rt.w2[,i]),digits=3)
	rt.est.w2[i,2]<-round(sqrt(mean(mivar.rt.w2[,i])+(var(miest.rt.w2[,i])*(1+(1/nmi)))),digits=3)

	rt.est.w3[i,1]<-round(mean(miest.rt.w3[,i]),digits=3)
	rt.est.w3[i,2]<-round(sqrt(mean(mivar.rt.w3[,i])+(var(miest.rt.w3[,i])*(1+(1/nmi)))),digits=3)

	# RANDOM FOREST ESTIMATES #
	rf.est.w1[i,1]<-round(mean(miest.rf.w1[,i]),digits=3)
	rf.est.w1[i,2]<-round(sqrt(mean(mivar.rf.w1[,i])+(var(miest.rf.w1[,i])*(1+(1/nmi)))),digits=3)

	rf.est.w2[i,1]<-round(mean(miest.rf.w2[,i]),digits=3)
	rf.est.w2[i,2]<-round(sqrt(mean(mivar.rf.w2[,i])+(var(miest.rf.w2[,i])*(1+(1/nmi)))),digits=3)

	rf.est.w3[i,1]<-round(mean(miest.rf.w3[,i]),digits=3)
	rf.est.w3[i,2]<-round(sqrt(mean(mivar.rf.w3[,i])+(var(miest.rf.w3[,i])*(1+(1/nmi)))),digits=3)

	# SUPER LEARNER ESTIMATES #
	sl.est.w1[i,1]<-round(mean(miest.sl.w1[,i]),digits=3)
	sl.est.w1[i,2]<-round(sqrt(mean(mivar.sl.w1[,i])+(var(miest.sl.w1[,i])*(1+(1/nmi)))),digits=3)

	sl.est.w2[i,1]<-round(mean(miest.sl.w2[,i]),digits=3)
	sl.est.w2[i,2]<-round(sqrt(mean(mivar.sl.w2[,i])+(var(miest.sl.w2[,i])*(1+(1/nmi)))),digits=3)

	sl.est.w3[i,1]<-round(mean(miest.sl.w3[,i]),digits=3)
	sl.est.w3[i,2]<-round(sqrt(mean(mivar.sl.w3[,i])+(var(miest.sl.w3[,i])*(1+(1/nmi)))),digits=3)
	}

for (i in 1:3) { 
	
	# LM ESTIMATES #
	lm.dif.w1[i,1]<-round(mean(midest.lm.w1[,i]),digits=3)
	lm.dif.w1[i,2]<-round(sqrt(mean(midvar.lm.w1[,i])+(var(midest.lm.w1[,i])*(1+(1/nmi)))),digits=3)
	lm.dif.w1[i,3]<-round(lm.dif.w1[i,1]/lm.dif.w1[i,2],digits=3)
	lm.dif.w1[i,4]<-round((pnorm(abs(lm.dif.w1[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	lm.dif.w2[i,1]<-round(mean(midest.lm.w2[,i]),digits=3)
	lm.dif.w2[i,2]<-round(sqrt(mean(midvar.lm.w2[,i])+(var(midest.lm.w2[,i])*(1+(1/nmi)))),digits=3)
	lm.dif.w2[i,3]<-round(lm.dif.w2[i,1]/lm.dif.w2[i,2],digits=3)
	lm.dif.w2[i,4]<-round((pnorm(abs(lm.dif.w2[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	lm.dif.w3[i,1]<-round(mean(midest.lm.w3[,i]),digits=3)
	lm.dif.w3[i,2]<-round(sqrt(mean(midvar.lm.w3[,i])+(var(midest.lm.w3[,i])*(1+(1/nmi)))),digits=3)
	lm.dif.w3[i,3]<-round(lm.dif.w3[i,1]/lm.dif.w3[i,2],digits=3)
	lm.dif.w3[i,4]<-round((pnorm(abs(lm.dif.w3[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# REG TREE ESTIMATES #
	rt.dif.w1[i,1]<-round(mean(midest.rt.w1[,i]),digits=3)
	rt.dif.w1[i,2]<-round(sqrt(mean(midvar.rt.w1[,i])+(var(midest.rt.w1[,i])*(1+(1/nmi)))),digits=3)
	rt.dif.w1[i,3]<-round(rt.dif.w1[i,1]/rt.dif.w1[i,2],digits=3)
	rt.dif.w1[i,4]<-round((pnorm(abs(rt.dif.w1[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	rt.dif.w2[i,1]<-round(mean(midest.rt.w2[,i]),digits=3)
	rt.dif.w2[i,2]<-round(sqrt(mean(midvar.rt.w2[,i])+(var(midest.rt.w2[,i])*(1+(1/nmi)))),digits=3)
	rt.dif.w2[i,3]<-round(rt.dif.w2[i,1]/rt.dif.w2[i,2],digits=3)
	rt.dif.w2[i,4]<-round((pnorm(abs(rt.dif.w2[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	rt.dif.w3[i,1]<-round(mean(midest.rt.w3[,i]),digits=3)
	rt.dif.w3[i,2]<-round(sqrt(mean(midvar.rt.w3[,i])+(var(midest.rt.w3[,i])*(1+(1/nmi)))),digits=3)
	rt.dif.w3[i,3]<-round(rt.dif.w3[i,1]/rt.dif.w3[i,2],digits=3)
	rt.dif.w3[i,4]<-round((pnorm(abs(rt.dif.w3[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# RANDOM FOREST ESTIMATES #
	rf.dif.w1[i,1]<-round(mean(midest.rf.w1[,i]),digits=3)
	rf.dif.w1[i,2]<-round(sqrt(mean(midvar.rf.w1[,i])+(var(midest.rf.w1[,i])*(1+(1/nmi)))),digits=3)
	rf.dif.w1[i,3]<-round(rf.dif.w1[i,1]/rf.dif.w1[i,2],digits=3)
	rf.dif.w1[i,4]<-round((pnorm(abs(rf.dif.w1[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	rf.dif.w2[i,1]<-round(mean(midest.rf.w2[,i]),digits=3)
	rf.dif.w2[i,2]<-round(sqrt(mean(midvar.rf.w2[,i])+(var(midest.rf.w2[,i])*(1+(1/nmi)))),digits=3)
	rf.dif.w2[i,3]<-round(rf.dif.w2[i,1]/rf.dif.w2[i,2],digits=3)
	rf.dif.w2[i,4]<-round((pnorm(abs(rf.dif.w2[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	rf.dif.w3[i,1]<-round(mean(midest.rf.w3[,i]),digits=3)
	rf.dif.w3[i,2]<-round(sqrt(mean(midvar.rf.w3[,i])+(var(midest.rf.w3[,i])*(1+(1/nmi)))),digits=3)
	rf.dif.w3[i,3]<-round(rf.dif.w3[i,1]/rf.dif.w3[i,2],digits=3)
	rf.dif.w3[i,4]<-round((pnorm(abs(rf.dif.w3[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# SUPER LEARNER ESTIMATES #
	sl.dif.w1[i,1]<-round(mean(midest.sl.w1[,i]),digits=3)
	sl.dif.w1[i,2]<-round(sqrt(mean(midvar.sl.w1[,i])+(var(midest.sl.w1[,i])*(1+(1/nmi)))),digits=3)
	sl.dif.w1[i,3]<-round(sl.dif.w1[i,1]/sl.dif.w1[i,2],digits=3)
	sl.dif.w1[i,4]<-round((pnorm(abs(sl.dif.w1[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	sl.dif.w2[i,1]<-round(mean(midest.sl.w2[,i]),digits=3)
	sl.dif.w2[i,2]<-round(sqrt(mean(midvar.sl.w2[,i])+(var(midest.sl.w2[,i])*(1+(1/nmi)))),digits=3)
	sl.dif.w2[i,3]<-round(sl.dif.w2[i,1]/sl.dif.w2[i,2],digits=3)
	sl.dif.w2[i,4]<-round((pnorm(abs(sl.dif.w2[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	sl.dif.w3[i,1]<-round(mean(midest.sl.w3[,i]),digits=3)
	sl.dif.w3[i,2]<-round(sqrt(mean(midvar.sl.w3[,i])+(var(midest.sl.w3[,i])*(1+(1/nmi)))),digits=3)
	sl.dif.w3[i,3]<-round(sl.dif.w3[i,1]/sl.dif.w3[i,2],digits=3)
	sl.dif.w3[i,4]<-round((pnorm(abs(sl.dif.w3[i,3]),0,1,lower.tail=FALSE)*2),digits=3)
	}

stopCluster(my.cluster)
rm(my.cluster)


######################
# CUMULATIVE EFFECTS #
######################

##### LOAD PHDCN #####
phdcnmi<-read.dta("D:\\projects\\skill_gaps_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which((phdcnmi$"_mj"!=0)),]
phdcnmi<-phdcnmi[order(phdcnmi$"_mj",phdcnmi$nsubid),]

##### SET HYPERPARAMETERS #####
cv.control.sl<-SuperLearner.CV.control(V=20)
rt.sl.cm<-create.Learner("SL.rpart",name_prefix="rt.sl.cm",params=list(minsplit=2,minbucket=1,cp=0.01,xval=10,maxcomplete=0,maxsurrogate=0))
rf.sl.cm<-create.Learner("SL.ranger",name_prefix="rf.sl.cm",params=list(num.trees=ntrees,min.node.size=10,mtry=8,seed=8675309))

##### DEFINE VARIABLE SETS #####
vars.b.cm<-c("nblack_b","nhispan_b","nothrace_b","npcage_b","npchsgrad_b","npcsomcol_b","npccolgrd_b","nownhome_b","nfemale_b","nfamsize_b")
vars.w1.cm<-c("nlninc_1","npcemply_1","npcwelf_1","npcmarr_1","npcengl_1","ncondadvg_1")
vars.w2.cm<-c("nlninc_2","npcemply_2","npcwelf_2","npcmarr_2","npcengl_2","ncondadvg_2")
vars.w3.cm<-c("nlninc_3","npcemply_3","npcwelf_3","npcmarr_3","npcengl_3","ncondadvg_3")

##### DEFINE RESIDUAL BALANCING FUNCTION #####
resbal<-function(M,C,Q,Z=rep(0,ncol(C)),max_iter=100,tol=1,print.level=0) {
  converged <- FALSE
  for (iter in 1:max_iter) {
    if (print.level >= 1) cat("iteration",iter,"\n")
    W<-c(Q * exp(C %*% Z))
    sumC<-t(C) %*% W
    gradient<-sumC-M
    if (max(abs(gradient)) < tol) {
      converged<-TRUE
      break
      }
    if (print.level >= 2) { cat("Iteration",iter,"maximum deviation is =",format(max(abs(gradient)),digits=4),"\n") }
    hessian=t(C) %*% (W*C)
    Coefs<-Z
    newton<-solve(hessian, gradient)
    Z<-Z-newton
    loss.new<-line.searcher(Base.weight=Q,Co.x=C,Tr.total=M,coefs=Z,Newton=newton,ss=1)
    loss.old<-line.searcher(Base.weight=Q,Co.x=C,Tr.total=M,coefs=Coefs,Newton=newton,ss=0)
    if (print.level>=3) { cat("new loss=",loss.new,"old loss=",loss.old,"\n") }
    if (loss.old <= loss.new) {
      ss.out<-optimize(line.searcher,lower=.001,upper=1,maximum=FALSE,Base.weight=Q,Co.x=C,Tr.total=M,coefs=Coefs,Newton=newton)
      if (print.level >= 3) { cat("LS Step Length is ",ss.out$minimum,"\n") }
      if (print.level >= 3) { cat("Loss is",ss.out$objective,"\n") }
      Z <- Coefs-ss.out$minimum*solve(hessian, gradient)
      }
    }
  list(W=W,sumC=sumC,converged=converged,maxdiff=max(abs(gradient)))
  }

##### PARALLELIZATION #####
n.cores<-parallel::detectCores()-save.cores
my.cluster<-parallel::makeCluster(n.cores,type="PSOCK")
doParallel::registerDoParallel(cl=my.cluster)
foreach::getDoParRegistered()
clusterEvalQ(cl=my.cluster, library(SuperLearner))
clusterEvalQ(cl=my.cluster, library(ebal))
clusterEvalQ(cl=my.cluster, library(tidyr))
clusterEvalQ(cl=my.cluster, library(dplyr))
registerDoRNG(8675309)

##### ESTIMATE COUNTERFACTUAL GAPS #####
miest.lm.cm<-miest.rt.cm<-miest.rf.cm<-miest.sl.cm<-matrix(data=NA,nrow=nmi,ncol=3)
mivar.lm.cm<-mivar.rt.cm<-mivar.rf.cm<-mivar.sl.cm<-matrix(data=NA,nrow=nmi,ncol=3)

midest.lm.cm<-midest.rt.cm<-midest.rf.cm<-midest.sl.cm<-matrix(data=NA,nrow=nmi,ncol=3)
midvar.lm.cm<-midvar.rt.cm<-midvar.rf.cm<-midvar.sl.cm<-matrix(data=NA,nrow=nmi,ncol=3)

for (i in 1:nmi) {

	### LOAD MI DATA ###
	print(i)
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	### OBSERVED GAPS ###
	m0.race<-lm(natten_3~nblack_b+nhispan_b+nothrace_b,data=phdcn)
	m0.educ<-lm(natten_3~npccolgrd_b,data=phdcn)
	bvwgap.obs<-m0.race$coefficients[2]
	hvwgap.obs<-m0.race$coefficients[3]
	nvdgap.obs<-m0.educ$coefficients[2]*(-1)

	### COUNTERFACTUAL GAPS ###

	# RESIDUALIZE CONFOUNDERS #
	resid_w2<-function(y) { residuals(lm(y~.,data=phdcn[,c(vars.b.cm,vars.w1.cm,"nlnbll_1")])) }
	resid_w3<-function(y) { residuals(lm(y~.,data=phdcn[,c(vars.b.cm,vars.w2.cm,"nlnbll_2")])) }
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2,ncondadvg_2), funs(r=resid_w2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3,ncondadvg_3), funs(r=resid_w3))

	# DEFINE CONSTRAINT MATRICES #
	C<-cbind(1,
		phdcn$nlninc_2_r,
		phdcn$nlninc_2_r*phdcn$nlnbll_1,
		phdcn$nlninc_2_r*phdcn$nlnbll_2,
		phdcn$nlninc_2_r*phdcn$npcage_b,
		phdcn$nlninc_2_r*phdcn$npchsgrad_b,
		phdcn$nlninc_2_r*phdcn$npcsomcol_b,
		phdcn$nlninc_2_r*phdcn$npccolgrd_b,
		phdcn$nlninc_2_r*phdcn$nownhome_b,
		phdcn$nlninc_2_r*phdcn$nfemale_b,
		phdcn$nlninc_2_r*phdcn$nblack_b,
		phdcn$nlninc_2_r*phdcn$nhispan_b,
		phdcn$nlninc_2_r*phdcn$nfamsize_b,
		phdcn$nlninc_2_r*phdcn$nlninc_1,
		phdcn$nlninc_2_r*phdcn$npcemply_1,
		phdcn$nlninc_2_r*phdcn$npcwelf_1,
		phdcn$nlninc_2_r*phdcn$npcmarr_1,
		phdcn$nlninc_2_r*phdcn$npcengl_1,
		phdcn$nlninc_2_r*phdcn$ncondadvg_1,
		phdcn$npcemply_2_r,
		phdcn$npcemply_2_r*phdcn$nlnbll_1,
		phdcn$npcemply_2_r*phdcn$nlnbll_2,
		phdcn$npcemply_2_r*phdcn$npcage_b,
		phdcn$npcemply_2_r*phdcn$npchsgrad_b,
		phdcn$npcemply_2_r*phdcn$npcsomcol_b,
		phdcn$npcemply_2_r*phdcn$npccolgrd_b,
		phdcn$npcemply_2_r*phdcn$nownhome_b,
		phdcn$npcemply_2_r*phdcn$nfemale_b,
		phdcn$npcemply_2_r*phdcn$nblack_b,
		phdcn$npcemply_2_r*phdcn$nhispan_b,
		phdcn$npcemply_2_r*phdcn$nfamsize_b,
		phdcn$npcemply_2_r*phdcn$nlninc_1,
		phdcn$npcemply_2_r*phdcn$npcemply_1,
		phdcn$npcemply_2_r*phdcn$npcwelf_1,
		phdcn$npcemply_2_r*phdcn$npcmarr_1,
		phdcn$npcemply_2_r*phdcn$npcengl_1,
		phdcn$npcemply_2_r*phdcn$ncondadvg_1,
		phdcn$npcwelf_2_r,
		phdcn$npcwelf_2_r*phdcn$nlnbll_1,
		phdcn$npcwelf_2_r*phdcn$nlnbll_2,
		phdcn$npcwelf_2_r*phdcn$npcage_b,
		phdcn$npcwelf_2_r*phdcn$npchsgrad_b,
		phdcn$npcwelf_2_r*phdcn$npcsomcol_b,
		phdcn$npcwelf_2_r*phdcn$npccolgrd_b,
		phdcn$npcwelf_2_r*phdcn$nownhome_b,
		phdcn$npcwelf_2_r*phdcn$nfemale_b,
		phdcn$npcwelf_2_r*phdcn$nblack_b,
		phdcn$npcwelf_2_r*phdcn$nhispan_b,
		phdcn$npcwelf_2_r*phdcn$nfamsize_b,
		phdcn$npcwelf_2_r*phdcn$nlninc_1,
		phdcn$npcwelf_2_r*phdcn$npcemply_1,
		phdcn$npcwelf_2_r*phdcn$npcwelf_1,
		phdcn$npcwelf_2_r*phdcn$npcmarr_1,
		phdcn$npcwelf_2_r*phdcn$npcengl_1,
		phdcn$npcwelf_2_r*phdcn$ncondadvg_1,
		phdcn$npcmarr_2_r,
		phdcn$npcmarr_2_r*phdcn$nlnbll_1,
		phdcn$npcmarr_2_r*phdcn$nlnbll_2,
		phdcn$npcmarr_2_r*phdcn$npcage_b,
		phdcn$npcmarr_2_r*phdcn$npchsgrad_b,
		phdcn$npcmarr_2_r*phdcn$npcsomcol_b,
		phdcn$npcmarr_2_r*phdcn$npccolgrd_b,
		phdcn$npcmarr_2_r*phdcn$nownhome_b,
		phdcn$npcmarr_2_r*phdcn$nfemale_b,
		phdcn$npcmarr_2_r*phdcn$nblack_b,
		phdcn$npcmarr_2_r*phdcn$nhispan_b,
		phdcn$npcmarr_2_r*phdcn$nfamsize_b,
		phdcn$npcmarr_2_r*phdcn$nlninc_1,
		phdcn$npcmarr_2_r*phdcn$npcemply_1,
		phdcn$npcmarr_2_r*phdcn$npcwelf_1,
		phdcn$npcmarr_2_r*phdcn$npcmarr_1,
		phdcn$npcmarr_2_r*phdcn$npcengl_1,
		phdcn$npcmarr_2_r*phdcn$ncondadvg_1,
		phdcn$npcengl_2_r,
		phdcn$npcengl_2_r*phdcn$nlnbll_1,
		phdcn$npcengl_2_r*phdcn$nlnbll_2,
		phdcn$npcengl_2_r*phdcn$npcage_b,
		phdcn$npcengl_2_r*phdcn$npchsgrad_b,
		phdcn$npcengl_2_r*phdcn$npcsomcol_b,
		phdcn$npcengl_2_r*phdcn$npccolgrd_b,
		phdcn$npcengl_2_r*phdcn$nownhome_b,
		phdcn$npcengl_2_r*phdcn$nfemale_b,
		phdcn$npcengl_2_r*phdcn$nblack_b,
		phdcn$npcengl_2_r*phdcn$nhispan_b,
		phdcn$npcengl_2_r*phdcn$nfamsize_b,
		phdcn$npcengl_2_r*phdcn$nlninc_1,
		phdcn$npcengl_2_r*phdcn$npcemply_1,
		phdcn$npcengl_2_r*phdcn$npcwelf_1,
		phdcn$npcengl_2_r*phdcn$npcmarr_1,
		phdcn$npcengl_2_r*phdcn$npcengl_1,
		phdcn$npcengl_2_r*phdcn$ncondadvg_1,
		phdcn$ncondadvg_2_r,
		phdcn$ncondadvg_2_r*phdcn$nlnbll_1,
		phdcn$ncondadvg_2_r*phdcn$nlnbll_2,
		phdcn$ncondadvg_2_r*phdcn$npcage_b,
		phdcn$ncondadvg_2_r*phdcn$npchsgrad_b,
		phdcn$ncondadvg_2_r*phdcn$npcsomcol_b,
		phdcn$ncondadvg_2_r*phdcn$npccolgrd_b,
		phdcn$ncondadvg_2_r*phdcn$nownhome_b,
		phdcn$ncondadvg_2_r*phdcn$nfemale_b,
		phdcn$ncondadvg_2_r*phdcn$nblack_b,
		phdcn$ncondadvg_2_r*phdcn$nhispan_b,
		phdcn$ncondadvg_2_r*phdcn$nfamsize_b,
		phdcn$ncondadvg_2_r*phdcn$nlninc_1,
		phdcn$ncondadvg_2_r*phdcn$npcemply_1,
		phdcn$ncondadvg_2_r*phdcn$npcwelf_1,
		phdcn$ncondadvg_2_r*phdcn$npcmarr_1,
		phdcn$ncondadvg_2_r*phdcn$npcengl_1,
		phdcn$ncondadvg_2_r*phdcn$ncondadvg_1,
		phdcn$nlninc_3_r,
		phdcn$nlninc_3_r*phdcn$nlnbll_2,
		phdcn$nlninc_3_r*phdcn$nlnbll_3,
		phdcn$nlninc_3_r*phdcn$npcage_b,
		phdcn$nlninc_3_r*phdcn$npchsgrad_b,
		phdcn$nlninc_3_r*phdcn$npcsomcol_b,
		phdcn$nlninc_3_r*phdcn$npccolgrd_b,
		phdcn$nlninc_3_r*phdcn$nownhome_b,
		phdcn$nlninc_3_r*phdcn$nfemale_b,
		phdcn$nlninc_3_r*phdcn$nblack_b,
		phdcn$nlninc_3_r*phdcn$nhispan_b,
		phdcn$nlninc_3_r*phdcn$nfamsize_b,
		phdcn$nlninc_3_r*phdcn$nlninc_2,
		phdcn$nlninc_3_r*phdcn$npcemply_2,
		phdcn$nlninc_3_r*phdcn$npcwelf_2,
		phdcn$nlninc_3_r*phdcn$npcmarr_2,
		phdcn$nlninc_3_r*phdcn$npcengl_2,
		phdcn$nlninc_3_r*phdcn$ncondadvg_2,
		phdcn$npcemply_3_r,
		phdcn$npcemply_3_r*phdcn$nlnbll_2,
		phdcn$npcemply_3_r*phdcn$nlnbll_3,
		phdcn$npcemply_3_r*phdcn$npcage_b,
		phdcn$npcemply_3_r*phdcn$npchsgrad_b,
		phdcn$npcemply_3_r*phdcn$npcsomcol_b,
		phdcn$npcemply_3_r*phdcn$npccolgrd_b,
		phdcn$npcemply_3_r*phdcn$nownhome_b,
		phdcn$npcemply_3_r*phdcn$nfemale_b,
		phdcn$npcemply_3_r*phdcn$nblack_b,
		phdcn$npcemply_3_r*phdcn$nhispan_b,
		phdcn$npcemply_3_r*phdcn$nfamsize_b,
		phdcn$npcemply_3_r*phdcn$nlninc_2,
		phdcn$npcemply_3_r*phdcn$npcemply_2,
		phdcn$npcemply_3_r*phdcn$npcwelf_2,
		phdcn$npcemply_3_r*phdcn$npcmarr_2,
		phdcn$npcemply_3_r*phdcn$npcengl_2,
		phdcn$npcemply_3_r*phdcn$ncondadvg_2,
		phdcn$npcwelf_3_r,
		phdcn$npcwelf_3_r*phdcn$nlnbll_2,
		phdcn$npcwelf_3_r*phdcn$nlnbll_3,
		phdcn$npcwelf_3_r*phdcn$npcage_b,
		phdcn$npcwelf_3_r*phdcn$npchsgrad_b,
		phdcn$npcwelf_3_r*phdcn$npcsomcol_b,
		phdcn$npcwelf_3_r*phdcn$npccolgrd_b,
		phdcn$npcwelf_3_r*phdcn$nownhome_b,
		phdcn$npcwelf_3_r*phdcn$nfemale_b,
		phdcn$npcwelf_3_r*phdcn$nblack_b,
		phdcn$npcwelf_3_r*phdcn$nhispan_b,
		phdcn$npcwelf_3_r*phdcn$nfamsize_b,
		phdcn$npcwelf_3_r*phdcn$nlninc_2,
		phdcn$npcwelf_3_r*phdcn$npcemply_2,
		phdcn$npcwelf_3_r*phdcn$npcwelf_2,
		phdcn$npcwelf_3_r*phdcn$npcmarr_2,
		phdcn$npcwelf_3_r*phdcn$npcengl_2,
		phdcn$npcwelf_3_r*phdcn$ncondadvg_2,
		phdcn$npcmarr_3_r,
		phdcn$npcmarr_3_r*phdcn$nlnbll_2,
		phdcn$npcmarr_3_r*phdcn$nlnbll_3,
		phdcn$npcmarr_3_r*phdcn$npcage_b,
		phdcn$npcmarr_3_r*phdcn$npchsgrad_b,
		phdcn$npcmarr_3_r*phdcn$npcsomcol_b,
		phdcn$npcmarr_3_r*phdcn$npccolgrd_b,
		phdcn$npcmarr_3_r*phdcn$nownhome_b,
		phdcn$npcmarr_3_r*phdcn$nfemale_b,
		phdcn$npcmarr_3_r*phdcn$nblack_b,
		phdcn$npcmarr_3_r*phdcn$nhispan_b,
		phdcn$npcmarr_3_r*phdcn$nfamsize_b,
		phdcn$npcmarr_3_r*phdcn$nlninc_2,
		phdcn$npcmarr_3_r*phdcn$npcemply_2,
		phdcn$npcmarr_3_r*phdcn$npcwelf_2,
		phdcn$npcmarr_3_r*phdcn$npcmarr_2,
		phdcn$npcmarr_3_r*phdcn$npcengl_2,
		phdcn$npcmarr_3_r*phdcn$ncondadvg_2,
		phdcn$npcengl_3_r,
		phdcn$npcengl_3_r*phdcn$nlnbll_2,
		phdcn$npcengl_3_r*phdcn$nlnbll_3,
		phdcn$npcengl_3_r*phdcn$npcage_b,
		phdcn$npcengl_3_r*phdcn$npchsgrad_b,
		phdcn$npcengl_3_r*phdcn$npcsomcol_b,
		phdcn$npcengl_3_r*phdcn$npccolgrd_b,
		phdcn$npcengl_3_r*phdcn$nownhome_b,
		phdcn$npcengl_3_r*phdcn$nfemale_b,
		phdcn$npcengl_3_r*phdcn$nblack_b,
		phdcn$npcengl_3_r*phdcn$nhispan_b,
		phdcn$npcengl_3_r*phdcn$nfamsize_b,
		phdcn$npcengl_3_r*phdcn$nlninc_2,
		phdcn$npcengl_3_r*phdcn$npcemply_2,
		phdcn$npcengl_3_r*phdcn$npcwelf_2,
		phdcn$npcengl_3_r*phdcn$npcmarr_2,
		phdcn$npcengl_3_r*phdcn$npcengl_2,
		phdcn$npcengl_3_r*phdcn$ncondadvg_2,
		phdcn$ncondadvg_3_r,
		phdcn$ncondadvg_3_r*phdcn$nlnbll_2,
		phdcn$ncondadvg_3_r*phdcn$nlnbll_3,
		phdcn$ncondadvg_3_r*phdcn$npcage_b,
		phdcn$ncondadvg_3_r*phdcn$npchsgrad_b,
		phdcn$ncondadvg_3_r*phdcn$npcsomcol_b,
		phdcn$ncondadvg_3_r*phdcn$npccolgrd_b,
		phdcn$ncondadvg_3_r*phdcn$nownhome_b,
		phdcn$ncondadvg_3_r*phdcn$nfemale_b,
		phdcn$ncondadvg_3_r*phdcn$nblack_b,
		phdcn$ncondadvg_3_r*phdcn$nhispan_b,
		phdcn$ncondadvg_3_r*phdcn$nfamsize_b,
		phdcn$ncondadvg_3_r*phdcn$nlninc_2,
		phdcn$ncondadvg_3_r*phdcn$npcemply_2,
		phdcn$ncondadvg_3_r*phdcn$npcwelf_2,
		phdcn$ncondadvg_3_r*phdcn$npcmarr_2,
		phdcn$ncondadvg_3_r*phdcn$npcengl_2,
		phdcn$ncondadvg_3_r*phdcn$ncondadvg_2)

	# DEFINE BASE WEIGHTS #
	Q<-rep(1,length(phdcn$nsubid))

	# DEFINE COLUMN MEANS OF CONSTRAINT MATRIX #
	M<-c(sum(Q),rep(0,ncol(C)-1))

	# COMPUTE BALANCING WEIGHTS #
	rbalwt<-resbal(M,C,Q)$W
	rbalwt[rbalwt>quantile(rbalwt,probs=0.99)]<-quantile(rbalwt,probs=0.99)
	rbalwt[rbalwt<quantile(rbalwt,probs=0.01)]<-quantile(rbalwt,probs=0.01)

	# COMPUTE POST-INTERVENTION GAPS #
	y.train<-phdcn[,"natten_3"]
	x.train<-phdcn[,c(vars.b.cm,vars.w1.cm,"nlnbll_3","nlnbll_2","nlnbll_1","nsubage_3")]
	m1.sl.cm<-SuperLearner(Y=y.train,X=x.train,SL.library=c("SL.lm",rt.sl.cm$names,rf.sl.cm$names),cvControl=cv.control.sl,obsWeights=rbalwt)
	idata<-phdcn[,c(vars.b.cm,vars.w1.cm,"natten_3","nlnbll_3","nlnbll_2","nlnbll_1","nsubage_3")]
	idata$nlnbll_3<-idata$nlnbll_2<-idata$nlnbll_1<-m	
	uhat<-predict(m1.sl.cm,idata)
	uhat.lm<-uhat$library.predict[,1]
	uhat.rt<-uhat$library.predict[,2]
	uhat.rf<-uhat$library.predict[,3]
	uhat.sl<-uhat$pred
	m2.lm.race<-lm(uhat.lm~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.lm.educ<-lm(uhat.lm~npccolgrd_b,data=idata)
	miest.lm.cm[i,1]<-m2.lm.race$coefficients[2]
	miest.lm.cm[i,2]<-m2.lm.race$coefficients[3]
	miest.lm.cm[i,3]<-m2.lm.educ$coefficients[2]*(-1)
	m2.rt.race<-lm(uhat.rt~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rt.educ<-lm(uhat.rt~npccolgrd_b,data=idata)
	miest.rt.cm[i,1]<-m2.rt.race$coefficients[2]
	miest.rt.cm[i,2]<-m2.rt.race$coefficients[3]
	miest.rt.cm[i,3]<-m2.rt.educ$coefficients[2]*(-1)
	m2.rf.race<-lm(uhat.rf~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.rf.educ<-lm(uhat.rf~npccolgrd_b,data=idata)
	miest.rf.cm[i,1]<-m2.rf.race$coefficients[2]
	miest.rf.cm[i,2]<-m2.rf.race$coefficients[3]
	miest.rf.cm[i,3]<-m2.rf.educ$coefficients[2]*(-1)
	m2.sl.race<-lm(uhat.sl~nblack_b+nhispan_b+nothrace_b,data=idata)
	m2.sl.educ<-lm(uhat.sl~npccolgrd_b,data=idata)
	miest.sl.cm[i,1]<-m2.sl.race$coefficients[2]
	miest.sl.cm[i,2]<-m2.sl.race$coefficients[3]
	miest.sl.cm[i,3]<-m2.sl.educ$coefficients[2]*(-1)

	midest.lm.cm[i,1]<-miest.lm.cm[i,1]-bvwgap.obs
	midest.lm.cm[i,2]<-miest.lm.cm[i,2]-hvwgap.obs
	midest.lm.cm[i,3]<-miest.lm.cm[i,3]-nvdgap.obs

	midest.rt.cm[i,1]<-miest.rt.cm[i,1]-bvwgap.obs
	midest.rt.cm[i,2]<-miest.rt.cm[i,2]-hvwgap.obs
	midest.rt.cm[i,3]<-miest.rt.cm[i,3]-nvdgap.obs

	midest.rf.cm[i,1]<-miest.rf.cm[i,1]-bvwgap.obs
	midest.rf.cm[i,2]<-miest.rf.cm[i,2]-hvwgap.obs
	midest.rf.cm[i,3]<-miest.rf.cm[i,3]-nvdgap.obs

	midest.sl.cm[i,1]<-miest.sl.cm[i,1]-bvwgap.obs
	midest.sl.cm[i,2]<-miest.sl.cm[i,2]-hvwgap.obs
	midest.sl.cm[i,3]<-miest.sl.cm[i,3]-nvdgap.obs

	### BOOTSTRAP SEs ###

	clusterExport(cl=my.cluster,
		list(
			"vars.b.cm",
			"vars.w1.cm",
			"vars.w2.cm",
			"vars.w3.cm",
			"nmi",
			"nboot",
			"ntrees",
			"m",
			"phdcn",
			"resbal",
			"rt.sl.cm",
			"rf.sl.cm",
			"rt.sl.cm_1",
			"rf.sl.cm_1"),
		envir=environment())

	boot.est.cm<-foreach(h=1:nboot, .combine=cbind) %dopar% {

		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (g in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% g]),]
				for (l in 1:g) { 
					boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) 
					}
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.m0.race<-lm(natten_3~nblack_b+nhispan_b+nothrace_b,data=boot.phdcn)
		boot.m0.educ<-lm(natten_3~npccolgrd_b,data=boot.phdcn)
		boot.bvwgap.obs<-boot.m0.race$coefficients[2]
		boot.hvwgap.obs<-boot.m0.race$coefficients[3]
		boot.nvdgap.obs<-boot.m0.educ$coefficients[2]*(-1)

		boot.resid_w2<-function(y) { residuals(lm(y~.,data=boot.phdcn[,c(vars.b.cm,vars.w1.cm,"nlnbll_1")])) }
		boot.resid_w3<-function(y) { residuals(lm(y~.,data=boot.phdcn[,c(vars.b.cm,vars.w2.cm,"nlnbll_2")])) }
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2,ncondadvg_2), funs(r=boot.resid_w2))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3,ncondadvg_3), funs(r=boot.resid_w3))

		boot.C<-cbind(1,
			boot.phdcn$nlninc_2_r,
			boot.phdcn$nlninc_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcage_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nblack_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$nlninc_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$npcemply_2_r,
			boot.phdcn$npcemply_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcage_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nblack_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcemply_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$npcwelf_2_r,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcage_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nblack_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcwelf_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$npcmarr_2_r,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcage_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nblack_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcmarr_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$npcengl_2_r,
			boot.phdcn$npcengl_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcage_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nblack_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcengl_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$ncondadvg_2_r,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nlnbll_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nlnbll_2,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcage_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nownhome_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nfemale_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nblack_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nhispan_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nfamsize_b,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$nlninc_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcemply_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcwelf_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcmarr_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$npcengl_1,
			boot.phdcn$ncondadvg_2_r*boot.phdcn$ncondadvg_1,
			boot.phdcn$nlninc_3_r,
			boot.phdcn$nlninc_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcage_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nblack_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$nlninc_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_2,
			boot.phdcn$npcemply_3_r,
			boot.phdcn$npcemply_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcage_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nblack_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcemply_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_2,
			boot.phdcn$npcwelf_3_r,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcage_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nblack_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcwelf_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_2,
			boot.phdcn$npcmarr_3_r,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcage_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nblack_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcmarr_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_2,
			boot.phdcn$npcengl_3_r,
			boot.phdcn$npcengl_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcage_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nblack_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$npcengl_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_2,
			boot.phdcn$ncondadvg_3_r,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nlnbll_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nlnbll_3,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcage_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npchsgrad_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcsomcol_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npccolgrd_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nownhome_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nfemale_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nblack_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nhispan_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nfamsize_b,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$nlninc_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcemply_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcwelf_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcmarr_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$npcengl_2,
			boot.phdcn$ncondadvg_3_r*boot.phdcn$ncondadvg_2)

		boot.Q<-rep(1,length(boot.phdcn$nsubid))

		boot.M<-c(sum(boot.Q),rep(0,ncol(boot.C)-1))

		boot.rbalwt<-resbal(boot.M,boot.C,boot.Q)$W
		boot.rbalwt[boot.rbalwt>quantile(boot.rbalwt,probs=0.99)]<-quantile(boot.rbalwt,probs=0.99)
		boot.rbalwt[boot.rbalwt<quantile(boot.rbalwt,probs=0.01)]<-quantile(boot.rbalwt,probs=0.01)

		boot.y.train<-boot.phdcn[,"natten_3"]
		boot.x.train<-boot.phdcn[,c(vars.b.cm,vars.w1.cm,"nlnbll_3","nlnbll_2","nlnbll_1","nsubage_3")]
		boot.m1.sl.cm<-SuperLearner(Y=boot.y.train,X=boot.x.train,SL.library=c("SL.lm",rt.sl.cm$names,rf.sl.cm$names),cvControl=cv.control.sl,obsWeights=boot.rbalwt)
		boot.idata<-boot.phdcn[,c(vars.b.cm,vars.w1.cm,"natten_3","nlnbll_3","nlnbll_2","nlnbll_1","nsubage_3")]
		boot.idata$nlnbll_3<-boot.idata$nlnbll_2<-boot.idata$nlnbll_1<-m 
		boot.uhat<-predict(boot.m1.sl.cm,boot.idata)
		boot.uhat.lm<-boot.uhat$library.predict[,1]
		boot.uhat.rt<-boot.uhat$library.predict[,2]
		boot.uhat.rf<-boot.uhat$library.predict[,3]
		boot.uhat.sl<-boot.uhat$pred
		boot.m2.lm.race<-lm(boot.uhat.lm~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.lm.educ<-lm(boot.uhat.lm~npccolgrd_b,data=boot.idata)
		boot.bvwgap.lm.cm<-boot.m2.lm.race$coefficients[2]
		boot.hvwgap.lm.cm<-boot.m2.lm.race$coefficients[3]
		boot.nvdgap.lm.cm<-boot.m2.lm.educ$coefficients[2]*(-1)
		boot.m2.rt.race<-lm(boot.uhat.rt~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rt.educ<-lm(boot.uhat.rt~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rt.cm<-boot.m2.rt.race$coefficients[2]
		boot.hvwgap.rt.cm<-boot.m2.rt.race$coefficients[3]
		boot.nvdgap.rt.cm<-boot.m2.rt.educ$coefficients[2]*(-1)
		boot.m2.rf.race<-lm(boot.uhat.rf~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.rf.educ<-lm(boot.uhat.rf~npccolgrd_b,data=boot.idata)
		boot.bvwgap.rf.cm<-boot.m2.rf.race$coefficients[2]
		boot.hvwgap.rf.cm<-boot.m2.rf.race$coefficients[3]
		boot.nvdgap.rf.cm<-boot.m2.rf.educ$coefficients[2]*(-1)
		boot.m2.sl.race<-lm(boot.uhat.sl~nblack_b+nhispan_b+nothrace_b,data=boot.idata)
		boot.m2.sl.educ<-lm(boot.uhat.sl~npccolgrd_b,data=boot.idata)
		boot.bvwgap.sl.cm<-boot.m2.sl.race$coefficients[2]
		boot.hvwgap.sl.cm<-boot.m2.sl.race$coefficients[3]
		boot.nvdgap.sl.cm<-boot.m2.sl.educ$coefficients[2]*(-1)

		boot.bvwgap.lm.cm.d<-boot.bvwgap.lm.cm-boot.bvwgap.obs
		boot.hvwgap.lm.cm.d<-boot.hvwgap.lm.cm-boot.hvwgap.obs
		boot.nvdgap.lm.cm.d<-boot.nvdgap.lm.cm-boot.nvdgap.obs
		boot.bvwgap.rt.cm.d<-boot.bvwgap.rt.cm-boot.bvwgap.obs
		boot.hvwgap.rt.cm.d<-boot.hvwgap.rt.cm-boot.hvwgap.obs
		boot.nvdgap.rt.cm.d<-boot.nvdgap.rt.cm-boot.nvdgap.obs
		boot.bvwgap.rf.cm.d<-boot.bvwgap.rf.cm-boot.bvwgap.obs
		boot.hvwgap.rf.cm.d<-boot.hvwgap.rf.cm-boot.hvwgap.obs
		boot.nvdgap.rf.cm.d<-boot.nvdgap.rf.cm-boot.nvdgap.obs
		boot.bvwgap.sl.cm.d<-boot.bvwgap.sl.cm-boot.bvwgap.obs
		boot.hvwgap.sl.cm.d<-boot.hvwgap.sl.cm-boot.hvwgap.obs
		boot.nvdgap.sl.cm.d<-boot.nvdgap.sl.cm-boot.nvdgap.obs

		return(
			list(
				boot.bvwgap.lm.cm,boot.hvwgap.lm.cm,boot.nvdgap.lm.cm,
				boot.bvwgap.rt.cm,boot.hvwgap.rt.cm,boot.nvdgap.rt.cm,
				boot.bvwgap.rf.cm,boot.hvwgap.rf.cm,boot.nvdgap.rf.cm,
				boot.bvwgap.sl.cm,boot.hvwgap.sl.cm,boot.nvdgap.sl.cm,
				boot.bvwgap.lm.cm.d,boot.hvwgap.lm.cm.d,boot.nvdgap.lm.cm.d,
				boot.bvwgap.rt.cm.d,boot.hvwgap.rt.cm.d,boot.nvdgap.rt.cm.d,
				boot.bvwgap.rf.cm.d,boot.hvwgap.rf.cm.d,boot.nvdgap.rf.cm.d,
				boot.bvwgap.sl.cm.d,boot.hvwgap.sl.cm.d,boot.nvdgap.sl.cm.d))
		}

	boot.est.cm<-matrix(unlist(boot.est.cm),ncol=24,byrow=TRUE)

	mivar.lm.cm[i,1]<-var(boot.est.cm[,1])
	mivar.lm.cm[i,2]<-var(boot.est.cm[,2])
	mivar.lm.cm[i,3]<-var(boot.est.cm[,3])
	mivar.rt.cm[i,1]<-var(boot.est.cm[,4])
	mivar.rt.cm[i,2]<-var(boot.est.cm[,5])
	mivar.rt.cm[i,3]<-var(boot.est.cm[,6])
	mivar.rf.cm[i,1]<-var(boot.est.cm[,7])
	mivar.rf.cm[i,2]<-var(boot.est.cm[,8])
	mivar.rf.cm[i,3]<-var(boot.est.cm[,9])
	mivar.sl.cm[i,1]<-var(boot.est.cm[,10])
	mivar.sl.cm[i,2]<-var(boot.est.cm[,11])
	mivar.sl.cm[i,3]<-var(boot.est.cm[,12])

	midvar.lm.cm[i,1]<-var(boot.est.cm[,13])
	midvar.lm.cm[i,2]<-var(boot.est.cm[,14])
	midvar.lm.cm[i,3]<-var(boot.est.cm[,15])
	midvar.rt.cm[i,1]<-var(boot.est.cm[,16])
	midvar.rt.cm[i,2]<-var(boot.est.cm[,17])
	midvar.rt.cm[i,3]<-var(boot.est.cm[,18])
	midvar.rf.cm[i,1]<-var(boot.est.cm[,19])
	midvar.rf.cm[i,2]<-var(boot.est.cm[,20])
	midvar.rf.cm[i,3]<-var(boot.est.cm[,21])
	midvar.sl.cm[i,1]<-var(boot.est.cm[,22])
	midvar.sl.cm[i,2]<-var(boot.est.cm[,23])
	midvar.sl.cm[i,3]<-var(boot.est.cm[,24])
	}

### COMBINE MI ESTIMATES ###
lm.est.cm<-rt.est.cm<-rf.est.cm<-sl.est.cm<-matrix(data=NA,nrow=3,ncol=2)
lm.dif.cm<-rt.dif.cm<-rf.dif.cm<-sl.dif.cm<-matrix(data=NA,nrow=3,ncol=4)

for (i in 1:3) { 

	# LM ESTIMATES #
	lm.est.cm[i,1]<-round(mean(miest.lm.cm[,i]),digits=3)
	lm.est.cm[i,2]<-round(sqrt(mean(mivar.lm.cm[,i])+(var(miest.lm.cm[,i])*(1+(1/nmi)))),digits=3)

	# REG TREE ESTIMATES #
	rt.est.cm[i,1]<-round(mean(miest.rt.cm[,i]),digits=3)
	rt.est.cm[i,2]<-round(sqrt(mean(mivar.rt.cm[,i])+(var(miest.rt.cm[,i])*(1+(1/nmi)))),digits=3)

	# RANDOM FOREST ESTIMATES #
	rf.est.cm[i,1]<-round(mean(miest.rf.cm[,i]),digits=3)
	rf.est.cm[i,2]<-round(sqrt(mean(mivar.rf.cm[,i])+(var(miest.rf.cm[,i])*(1+(1/nmi)))),digits=3)

	# SUPER LEARNER ESTIMATES #
	sl.est.cm[i,1]<-round(mean(miest.sl.cm[,i]),digits=3)
	sl.est.cm[i,2]<-round(sqrt(mean(mivar.sl.cm[,i])+(var(miest.sl.cm[,i])*(1+(1/nmi)))),digits=3)
	}

for (i in 1:3) { 
	
	# LM ESTIMATES #
	lm.dif.cm[i,1]<-round(mean(midest.lm.cm[,i]),digits=3)
	lm.dif.cm[i,2]<-round(sqrt(mean(midvar.lm.cm[,i])+(var(midest.lm.cm[,i])*(1+(1/nmi)))),digits=3)
	lm.dif.cm[i,3]<-round(lm.dif.cm[i,1]/lm.dif.cm[i,2],digits=3)
	lm.dif.cm[i,4]<-round((pnorm(abs(lm.dif.cm[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# REG TREE ESTIMATES #
	rt.dif.cm[i,1]<-round(mean(midest.rt.cm[,i]),digits=3)
	rt.dif.cm[i,2]<-round(sqrt(mean(midvar.rt.cm[,i])+(var(midest.rt.cm[,i])*(1+(1/nmi)))),digits=3)
	rt.dif.cm[i,3]<-round(rt.dif.cm[i,1]/rt.dif.cm[i,2],digits=3)
	rt.dif.cm[i,4]<-round((pnorm(abs(rt.dif.cm[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# RANDOM FOREST ESTIMATES #
	rf.dif.cm[i,1]<-round(mean(midest.rf.cm[,i]),digits=3)
	rf.dif.cm[i,2]<-round(sqrt(mean(midvar.rf.cm[,i])+(var(midest.rf.cm[,i])*(1+(1/nmi)))),digits=3)
	rf.dif.cm[i,3]<-round(rf.dif.cm[i,1]/rf.dif.cm[i,2],digits=3)
	rf.dif.cm[i,4]<-round((pnorm(abs(rf.dif.cm[i,3]),0,1,lower.tail=FALSE)*2),digits=3)

	# SUPER LEARNER ESTIMATES #
	sl.dif.cm[i,1]<-round(mean(midest.sl.cm[,i]),digits=3)
	sl.dif.cm[i,2]<-round(sqrt(mean(midvar.sl.cm[,i])+(var(midest.sl.cm[,i])*(1+(1/nmi)))),digits=3)
	sl.dif.cm[i,3]<-round(sl.dif.cm[i,1]/sl.dif.cm[i,2],digits=3)
	sl.dif.cm[i,4]<-round((pnorm(abs(sl.dif.cm[i,3]),0,1,lower.tail=FALSE)*2),digits=3)
	}

stopCluster(my.cluster)
rm(my.cluster)


#################
# PRINT RESULTS #
#################
rlabel<-c('BlackVsWhite','HispanicVsWhite','NoDegreeVsDegree')

ob.gap<-data.frame(ob.est,row.names=rlabel)

lm.gap.w1<-data.frame(lm.est.w1,row.names=rlabel)
lm.gap.w2<-data.frame(lm.est.w2,row.names=rlabel)
lm.gap.w3<-data.frame(lm.est.w3,row.names=rlabel)
lm.gap.cm<-data.frame(lm.est.cm,row.names=rlabel)

rt.gap.w1<-data.frame(rt.est.w1,row.names=rlabel)
rt.gap.w2<-data.frame(rt.est.w2,row.names=rlabel)
rt.gap.w3<-data.frame(rt.est.w3,row.names=rlabel)
rt.gap.cm<-data.frame(rt.est.cm,row.names=rlabel)

rf.gap.w1<-data.frame(rf.est.w1,row.names=rlabel)
rf.gap.w2<-data.frame(rf.est.w2,row.names=rlabel)
rf.gap.w3<-data.frame(rf.est.w3,row.names=rlabel)
rf.gap.cm<-data.frame(rf.est.cm,row.names=rlabel)

sl.gap.w1<-data.frame(sl.est.w1,row.names=rlabel)
sl.gap.w2<-data.frame(sl.est.w2,row.names=rlabel)
sl.gap.w3<-data.frame(sl.est.w3,row.names=rlabel)
sl.gap.cm<-data.frame(sl.est.cm,row.names=rlabel)

colnames(ob.gap)<-c('Est','SE')
colnames(lm.gap.w1)<-colnames(lm.gap.w2)<-colnames(lm.gap.w3)<-colnames(lm.gap.cm)<-c('Est','SE')
colnames(rt.gap.w1)<-colnames(rt.gap.w2)<-colnames(rt.gap.w3)<-colnames(rt.gap.cm)<-c('Est','SE')
colnames(rf.gap.w1)<-colnames(rf.gap.w2)<-colnames(rf.gap.w3)<-colnames(rf.gap.cm)<-c('Est','SE')
colnames(sl.gap.w1)<-colnames(sl.gap.w2)<-colnames(sl.gap.w3)<-colnames(sl.gap.cm)<-c('Est','SE')

lm.dif.w1<-data.frame(lm.dif.w1,row.names=rlabel)
lm.dif.w2<-data.frame(lm.dif.w2,row.names=rlabel)
lm.dif.w3<-data.frame(lm.dif.w3,row.names=rlabel)
lm.dif.cm<-data.frame(lm.dif.cm,row.names=rlabel)

rt.dif.w1<-data.frame(rt.dif.w1,row.names=rlabel)
rt.dif.w2<-data.frame(rt.dif.w2,row.names=rlabel)
rt.dif.w3<-data.frame(rt.dif.w3,row.names=rlabel)
rt.dif.cm<-data.frame(rt.dif.cm,row.names=rlabel)

rf.dif.w1<-data.frame(rf.dif.w1,row.names=rlabel)
rf.dif.w2<-data.frame(rf.dif.w2,row.names=rlabel)
rf.dif.w3<-data.frame(rf.dif.w3,row.names=rlabel)
rf.dif.cm<-data.frame(rf.dif.cm,row.names=rlabel)

sl.dif.w1<-data.frame(sl.dif.w1,row.names=rlabel)
sl.dif.w2<-data.frame(sl.dif.w2,row.names=rlabel)
sl.dif.w3<-data.frame(sl.dif.w3,row.names=rlabel)
sl.dif.cm<-data.frame(sl.dif.cm,row.names=rlabel)

colnames(lm.dif.w1)<-colnames(lm.dif.w2)<-colnames(lm.dif.w3)<-colnames(lm.dif.cm)<-c('Est','SE','Zstat','Pval')
colnames(rt.dif.w1)<-colnames(rt.dif.w2)<-colnames(rt.dif.w3)<-colnames(rt.dif.cm)<-c('Est','SE','Zstat','Pval')
colnames(rf.dif.w1)<-colnames(rf.dif.w2)<-colnames(rf.dif.w3)<-colnames(rf.dif.cm)<-c('Est','SE','Zstat','Pval')
colnames(sl.dif.w1)<-colnames(sl.dif.w2)<-colnames(sl.dif.w3)<-colnames(sl.dif.cm)<-c('Est','SE','Zstat','Pval')

sink("D:\\projects\\skill_gaps_lead\\programs\\appendix_b\\_LOGS\\05_create_table_b5_log.txt")

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Linear Models\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 1 Exposure\n")
print(lm.dif.w1)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 2 Exposure\n")
print(lm.dif.w2)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 3 Exposure\n")
print(lm.dif.w3)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Cumulative Exposure\n")
print(lm.dif.cm)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat(" \n")
cat("===========================================\n")
cat("Regression Trees\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 1 Exposure\n")
print(rt.dif.w1)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 2 Exposure\n")
print(rt.dif.w2)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 3 Exposure\n")
print(rt.dif.w3)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Cumulative Exposure\n")
print(rt.dif.cm)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("RANDOM FORESTS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 1 Exposure\n")
print(rf.dif.w1)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 2 Exposure\n")
print(rf.dif.w2)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 3 Exposure\n")
print(rf.dif.w3)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Cumulative Exposure\n")
print(rf.dif.cm)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat(" \n")
cat("===========================================\n")
cat("SUPER LEARNERS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 1 Exposure\n")
print(sl.dif.w1)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 2 Exposure\n")
print(sl.dif.w2)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Wave 3 Exposure\n")
print(sl.dif.w3)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Equalize Cumulative Exposure\n")
print(sl.dif.cm)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")

print(startTime)
print(Sys.time())

sink()


