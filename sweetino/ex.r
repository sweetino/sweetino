library(lubridate)
library(MatchIt)
library(survival)
library(data.table)

######################################################################################################################

######################################################################################################################
# CODE_J32<-as.data.table(unique(KCD6[grepl("J32",diseaseCODE)]$diseaseCODE))
# CODE_J33<-as.data.table(unique(KCD6[grepl("J33",diseaseCODE)]$diseaseCODE))
# 
# CODE_DEP1<-as.data.table(unique(KCD6[grepl("F33",diseaseCODE)]$diseaseCODE))
# CODE_DEP2<-as.data.table(unique(KCD6[grepl("F32",diseaseCODE)]$diseaseCODE))
# CODE_DEP3<-as.data.table(c('F313','F314','F315','F341','F381'))
# CODE_DEP<-rbind(CODE_DEP1,CODE_DEP2,CODE_DEP3)

# CODE_ANX1<-as.data.table(unique(KCD6[grepl("F40",diseaseCODE)]$diseaseCODE))
# CODE_ANX2<-as.data.table(unique(KCD6[grepl("F41",diseaseCODE)]$diseaseCODE))
# CODE_ANX<-rbind(CODE_ANX1,CODE_ANX2)
# CODE_ANX<-CODE_ANX[-1]

##################################################################################################
# P_J32<-getpatient(CODE_J32) #329757
# P_J33<-getpatient(CODE_J33) #22334

# nrow(as.data.table(unique(P_J32$PERSON_ID))) #329757

# nrow(as.data.table(unique(P_J33$PERSON_ID))) #22334
###################################################################################
P_J32<-P_J32[,.(PERSON_ID,Dx_DATE)]
P_J32<-unique(P_J32) 
P_J32_N<-P_J32[,.N, by = PERSON_ID]

setkey(P_J32,PERSON_ID)
setkey(P_J32_N,PERSON_ID)
P_J32_N<-P_J32_N[P_J32]

P_J32_2N<-P_J32_N[N>=2] #2李⑤??씠?긽 吏꾨떒

P_J32_2N_PID<-as.data.table(unique(P_J32_2N$PERSON_ID))
nrow(P_J32_2N_PID) #224511

P_J32_2N_0204<-P_J32_2N[Dx_DATE%/%10000<=2004]

P_J32_2N_0204

P_J32_P_J32_2N_0204_PID<-as.data.table(unique(P_J32_2N_0204$PERSON_ID))
nrow(P_J32_P_J32_2N_0204_PID) #79567

# ########################################################################################
# P_J32_0204<-P_J32_N[Dx_DATE%/%10000<=2004] #2002?뀈1?썡1?씪遺?꽣 2004?뀈12?썡31?씪 ?궗?씠?뿉 CRS濡? 吏꾨떒諛쏆? ?솚?옄
# nrow(as.data.table(unique(P_J32_0204$PERSON_ID))) #103572
# P_J32_0204_2N<-P_J32_0204[N>=2] #2李⑤??씠?긽 吏꾨떒
# nrow(as.data.table(unique(P_J32_0204_2N$PERSON_ID))) #79567
#########################################################################################
P_J32_N_min<-P_J32_2N_0204[,.SD[which.min(Dx_DATE)], by = list(PERSON_ID)] #79567
P_J32_N_max<-P_J32_2N_0204[,.SD[which.max(Dx_DATE)], by = list(PERSON_ID)]

setnames(P_J32_N_min,"Dx_DATE","Dx_min")
setnames(P_J32_N_max,"Dx_DATE","Dx_max") 

setkey(P_J32_2N_0204,PERSON_ID,Dx_DATE)

setkey(P_J32_2N_0204_3M,PERSON_ID,Dx_min)

P_J32_2N_0204_3M

# 3媛쒖썡 ?씠?긽 媛꾧꺽?쑝濡? 吏꾨떒?맂 ?솚?옄
P_J32_2N_0204_3M<-P_J32_N_min[P_J32_N_max,.(PERSON_ID,N,Dx_min,Dx_max)]
P_J32_2N_0204_3M[,Dx_time:=ymd(Dx_max)-ymd(Dx_min)]
P_J32_2N_0204_3M<-P_J32_2N_0204_3M[Dx_time>=90] #35483
P_J32_PID<-as.data.table(P_J32_2N_0204_3M$PERSON_ID)

P_J32_PID
# setkey(P_J32_PID,V1)
# setkey(P_J32,PERSON_ID)
# a<-P_J32[P_J32_PID]
# setkey(a,PERSON_ID,Dx_DATE)
# View(a)

P_J32_F<-P_J32_2N_0204_3M[,.(PERSON_ID,Dx_DATE=Dx_min)] #35483
####################################################################################################################
P_J33_0204<-P_J33[Dx_DATE%/%10000<=2004] 
P_J33_0204<-P_J33_0204[,.(PERSON_ID,Dx_DATE)]
P_J33_F<-P_J33_0204[, .SD[which.min(Dx_DATE)], by = list(PERSON_ID)] #6550

P_CRS<-rbind(P_J32_F,P_J33_F)
P_CRS<-P_CRS[, .SD[which.min(Dx_DATE)], by = list(PERSON_ID)] #39126

##########################################################################################
setkey(P_CRS,PERSON_ID)
setkey(DTJK,PERSON_ID)
P_CRS_AGE<-DTJK[P_CRS,.(PERSON_ID,Dx_DATE,AGE_GROUP,STND_Y)]
P_CRS_AGE<-P_CRS_AGE[STND_Y<=2004]
P_CRS_AGE<-P_CRS_AGE[, .SD[which.min(STND_Y)], by = list(PERSON_ID)] #30384
P_CRS_AGE_F<-P_CRS_AGE[AGE_GROUP>=5]
P_CRS_AGE_F[,STND_Y:=NULL] #17571

##################################################################
P_DEP<-P_DEP[,.(PERSON_ID,DEP_DATE=Dx_DATE)]
P_ANX<-P_ANX[,.(PERSON_ID,ANX_DATE=Dx_DATE)]

setkey(P_CRS_AGE_F,PERSON_ID)
setkey(P_DEP,PERSON_ID)
setkey(P_ANX,PERSON_ID)

P_CRS_MDO<-P_DEP[P_CRS_AGE_F]
P_CRS_MDO<-P_ANX[P_CRS_MDO]

P_CRS_MDO[ANX_DATE%/%10000<=2004,ANX0204:=ANX_DATE]
P_CRS_MDO[ANX_DATE%/%10000>=2005,ANX0513:=ANX_DATE]
P_CRS_MDO[DEP_DATE%/%10000<=2004,DEP0204:=DEP_DATE]
P_CRS_MDO[DEP_DATE%/%10000>=2005,DEP0513:=DEP_DATE]

P_CRS_MDO[is.na(P_CRS_MDO)]<-0
P_CRS_MDO_non0204<-P_CRS_MDO[ANX0204==0&DEP0204==0]
P_CRS_MDO_non0204_2<-P_CRS_MDO_non0204[, .SD[which.min(Dx_DATE)], by = list(PERSON_ID)]

P_CRS_FFF<-P_CRS_MDO_non0204_2[,.(PERSON_ID,Dx_DATE,ANX_DATE,DEP_DATE)]

P_CRS_FFF
######################################################################################################################
CODE_POL<-as.data.table(unique(KCD6[grepl("J33",diseaseCODE),]$diseaseCODE))
P_POL<-getpatient(CODE_POL) #329757

P_POL<-P_POL[, .SD[which.min(Dx_DATE)], by = list(PERSON_ID)]
setnames(P_POL,"Dx_CODE","POL_CODE")
setnames(P_POL,"Dx_DATE","POL_DATE")
P_POL

setkey(P_CRS_FFF,PERSON_ID)
setkey(P_POL,PERSON_ID)

P_CRS_Polyp<-P_POL[P_CRS_FFF,.(PERSON_ID,POL_DATE,Dx_DATE,ANX_DATE,DEP_DATE)]
P_CRS_Polyp

P_CRS_Polyp[is.na(P_CRS_Polyp)]<-0

####################################################################################################################
P_CRS_Polyp[,ANX_DATE:=NULL]
P_CRS_Polyp[,DEP_DATE:=NULL]

P_CRS_Polyp[POL_DATE!=0,Polyp:=1]
P_CRS_Polyp[POL_DATE==0,Polyp:=0]

P_CRS_Polyp[Polyp==0]
##########################################################################################
### 2. CRS_Polyp with JK
setkey(P_CRS_Polyp,PERSON_ID)
setkey(DTJK,PERSON_ID)
P_CRS_Polyp_JK <- DTJK[P_CRS_Polyp,.(STND_Y,PERSON_ID,SEX,AGE_GROUP,
                                     SIDO,CTRB_PT_TYPE_CD,
                                     DFAB_GRD_CD,Dx_DATE,Polyp),nomatch=0]
P_CRS_Polyp_JK<-P_CRS_Polyp_JK[, .SD[which.min(STND_Y)], by = list(PERSON_ID)]

JK_DTH <- DTJK[P_CRS_Polyp,.(PERSON_ID,DTH_YM),nomatch=0]
JK_DTH[is.na(JK_DTH)]<-0
JK_DTH<-JK_DTH[, .SD[which.max(DTH_YM)], by = list(PERSON_ID)]

P_CRS_Polyp_JK_F<-merge(P_CRS_Polyp_JK,JK_DTH)
P_CRS_Polyp_JK_F[,DTH_year:=DTH_YM%/%100]

P_CRS_Polyp_F<-P_CRS_Polyp_JK_F[DTH_year>=2005|DTH_year==0] #2002-2004?궗留앹옄?젣?쇅 #1375
P_CRS_Polyp_JK_F[,DTH_year:=NULL]

# #########################################################################################################
# ### 3. get non-CRS patient with JK
dt_CRSPID<-as.data.table(P_CRS$PERSON_ID)
setnames(dt_CRSPID,"PERSON_ID")

setkey(dt_CRSPID,PERSON_ID)
setkey(DTJK,PERSON_ID)
dt_NonCRSPID <- unique(DTJK[!dt_CRSPID,.(PERSON_ID)])
dt_NonCRSPID_10<-dt_NonCRSPID[sample(.N,100000)]

setkey(dt_NonCRSPID_10,PERSON_ID)
setkey(DTJK,PERSON_ID)
dt_NonCRS_JK<-dt_NonCRSPID_10[DTJK,.(PERSON_ID,STND_Y,SEX,AGE_GROUP,
                                     SIDO,CTRB_PT_TYPE_CD,DFAB_GRD_CD),nomatch=0]

nonCRS_JK_DTH <- DTJK[dt_NonCRSPID_10,.(PERSON_ID,DTH_YM),nomatch=0]
nonCRS_JK_DTH[is.na(nonCRS_JK_DTH)]<-0
nonCRS_JK_DTH<-nonCRS_JK_DTH[, .SD[which.max(DTH_YM)], by = list(PERSON_ID)]

dt_NonCRS_JK<-dt_NonCRS_JK[,.SD[which.min(STND_Y)],by=list(PERSON_ID)]
NonCRS_JK<-merge(dt_NonCRS_JK,nonCRS_JK_DTH)

NonCRS_JK[,DTH_year:=DTH_YM%/%100]
NonCRS_JK<-NonCRS_JK[DTH_year>=2005|DTH_year==0] #2002-2004?궗留앹옄?젣?쇅
NonCRS_JK[,DTH_year:=NULL]

###########################################################################################################
NonCRS_JK
P_CRS_Polyp_JK_F

colnames(P_CRS_Polyp_JK_F)
colnames(NonCRS_JK)

NonCRS_JK[,Dx_DATE:=0]
NonCRS_JK[,CRS:=0]
P_CRS_Polyp_JK_F[,CRS:=1]
NonCRS_JK[,Polyp:=0]


setcolorder(NonCRS_JK,c(1,2,3,4,5,6,7,9,11,8,10))

CRS_Polyp_nonCRS<-rbind(P_CRS_Polyp_JK_F,NonCRS_JK)

CRS_Polyp_nonCRS
#######################################################################################################################
CRS_Polyp_nonCRS<-as.data.frame(CRS_Polyp_nonCRS)
row.names(CRS_Polyp_nonCRS)<-c(CRS_Polyp_nonCRS$PERSON_ID)


m.out1 <-matchit(CRS ~factor(STND_Y)+factor(SEX) + factor(AGE_GROUP) +
                   factor(SIDO) + factor(CTRB_PT_TYPE_CD),
                 data = CRS_Polyp_nonCRS,method="nearest", ratio=2)
CRSwPolyp_nonCRS_matched1 <- as.data.table(match.data(m.out1))

######################################################################################################

matched_Dx_DATE<-matching_Dx_DATE_2(CRSwPolyp_nonCRS_matched1,m.out1)
########################################################################################
matched_Dx_DATE
CRSwPolyp_nonCRS_matched1[,PERSON_ID:=as.character(PERSON_ID)]

setkey(CRSwPolyp_nonCRS_matched1,PERSON_ID)
setkey(matched_Dx_DATE,PERSON_ID)

CRS_Dx_matched<-CRSwPolyp_nonCRS_matched1[matched_Dx_DATE]
CRS_Dx_matched[,Dx_DATE:=NULL]
setnames(CRS_Dx_matched,"i.Dx_DATE","Dx_DATE")

#########################################################################################################
CRS_1<-CRS_Dx_matched

P_ANX[ANX_DATE%/%10000<=2004,ANX0204:=ANX_DATE]
P_ANX[ANX_DATE%/%10000>=2005,ANX0513:=ANX_DATE]
P_ANX[is.na(P_ANX)]<-0
P_ANX_non0204<-P_ANX[ANX0204==0]
P_ANX_non0204<-P_ANX_non0204[,.(PERSON_ID,ANX_DATE)]
P_ANX_non0204<-P_ANX_non0204[,.SD[which.min(ANX_DATE)],by=list(PERSON_ID)]
P_ANX_non0204[,PERSON_ID:=as.character(PERSON_ID)]

P_DEP[DEP_DATE%/%10000<=2004,DEP0204:=DEP_DATE]
P_DEP[DEP_DATE%/%10000>=2005,DEP0513:=DEP_DATE]
P_DEP[is.na(P_DEP)]<-0
P_DEP_non0204<-P_DEP[DEP0204==0]
P_DEP_non0204<-P_DEP_non0204[,.(PERSON_ID,DEP_DATE)]
P_DEP_non0204<-P_DEP_non0204[,.SD[which.min(DEP_DATE)],by=list(PERSON_ID)]
P_DEP_non0204[,PERSON_ID:=as.character(PERSON_ID)]

setkey(CRS_1,PERSON_ID)
setkey(P_ANX_non0204,PERSON_ID)
setkey(P_DEP_non0204,PERSON_ID)

CRS_1<-P_DEP_non0204[CRS_1]
CRS_1[is.na(CRS_1)]<-0
CRS_1<-P_ANX_non0204[CRS_1]
CRS_1[is.na(CRS_1)]<-0
CRS_1
#######################################################################
CRS_1[,DTH_YM:=as.numeric(DTH_YM)]
CRS_1[DTH_YM%%100==1, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==3, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==4, DTH_YM:=DTH_YM*100+30]
CRS_1[DTH_YM%%100==5, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==6, DTH_YM:=DTH_YM*100+30]
CRS_1[DTH_YM%%100==7, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==8, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==9, DTH_YM:=DTH_YM*100+30]
CRS_1[DTH_YM%%100==10, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%%100==11, DTH_YM:=DTH_YM*100+30]
CRS_1[DTH_YM%%100==12, DTH_YM:=DTH_YM*100+31]
CRS_1[DTH_YM%/%100==2004 & DTH_YM%%100==2, DTH_YM:=DTH_YM*100+29]
CRS_1[DTH_YM%/%100==2008 & DTH_YM%%100==2, DTH_YM:=DTH_YM*100+29]
CRS_1[DTH_YM%/%100==2012 & DTH_YM%%100==2, DTH_YM:=DTH_YM*100+29]
CRS_1[DTH_YM%%100==2, DTH_YM:=DTH_YM*100+28]
######################################################################################################
CRS_polyp_MDO<-CRS_1

str(CRS_polyp_MDO)
CRS_polyp_MDO <- as.data.table(CRS_polyp_MDO)
######################################################################################################
CRS_polyp_MDO[DEP_DATE!=0,END_DEP:=DEP_DATE]
CRS_polyp_MDO[DEP_DATE==0&DTH_YM==0,END_DEP:=20131231]
CRS_polyp_MDO[DEP_DATE==0&DTH_YM!=0,END_DEP:=DTH_YM]
CRS_polyp_MDO[,time_DEP:=as.integer(ymd(END_DEP) - ymd(Dx_DATE))]
CRS_polyp_MDO[,time_DEP_Y:=time_DEP/365.25]

CRS_polyp_MDO[ANX_DATE!=0,END_ANX:=ANX_DATE]
CRS_polyp_MDO[ANX_DATE==0&DTH_YM==0,END_ANX:=20131231]
CRS_polyp_MDO[ANX_DATE==0&DTH_YM!=0,END_ANX:=DTH_YM]
CRS_polyp_MDO[,time_ANX:=as.integer(ymd(END_ANX) - ymd(Dx_DATE))]
CRS_polyp_MDO[,time_ANX_Y:=time_ANX/365.25]

CRS_polyp_MDO[CRS==1&Polyp==1,CRSpolyp:=1]
CRS_polyp_MDO[CRS==1&Polyp==0,CRSpolyp:=0]

CRS_polyp_MDO[is.na(CRS_polyp_MDO)]<-0

CRS_polyp_MDO[DEP_DATE!=0,DEP:=1]
CRS_polyp_MDO[ANX_DATE!=0,ANX:=1]
CRS_polyp_MDO[is.na(CRS_polyp_MDO)]<-0

###############################################################################################

library(survminer)
library(survival)

P_DEP_fit <- survfit(Surv(time_DEP_Y, DEP) ~ CRS, data = CRS_polyp_MDO, type = 'kaplan-meier')
cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRS, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
KM_CRS_DEP<-ggsurvplot(P_DEP_fit, fun = "cumhaz", color = NULL, 
                       palette = c("blue","red"),
                       surv.scale = c("default", "percent"),
                       conf.int = F, conf.int.fill = "gray", censor = F, pval = FALSE,
                       pval.size = 5, pval.coord = c(NULL, NULL), 
                       xlab = "Time (Years)",
                       ylab = "Cumulative hazard", 
                       font.main = c(16, "plain", "black"),
                       font.x = c(14, "plain", "black"), 
                       font.y = c(14, "plain", "black"),
                       font.tickslab = c(8, "plain", "black"), 
                       xlim = c(0,12),break.time.by = 2, ylim = c(0,0.6),
                       legend = c("top"),
                       legend.title = "", legend.labs = c("Comparison","CRS"), 
                       font.legend = c(10,"plain", "black"),
                       risk.table = T,
                       risk.table.title = "Number at risk by time",risk.table.col = "black",
                       risk.table.fontsize = 3, risk.table.y.text.col = F,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ggtheme = ggplot2::theme_classic())


KM_CRS_DEP$plot <- KM_CRS_DEP$plot + labs(
  title    = "Depression",                     
  subtitle = "",  
  caption  = paste("\n","Unadjusted HR =",round(hr, 2),",","95% CI",
                   "(", round(ci[1], 2), "-", round(ci[2], 2), ")"))
KM_CRS_DEP

###########################################################################################
P_ANX_fit <- survfit(Surv(time_ANX_Y, ANX) ~ CRS, data = CRS_polyp_MDO, type = 'kaplan-meier')
cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRS, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
KM_CRS_ANX<-ggsurvplot(P_ANX_fit, fun = "cumhaz", color = NULL, 
                       palette = c("blue","red"),
                       surv.scale = c("default", "percent"),
                       conf.int = F, conf.int.fill = "gray", censor = F, pval = FALSE,
                       pval.size = 5, pval.coord = c(NULL, NULL), 
                       xlab = "Time (Years)",
                       ylab = "Cumulative hazard", 
                       font.main = c(16, "plain", "black"),
                       font.x = c(14, "plain", "black"), 
                       font.y = c(14, "plain", "black"),
                       font.tickslab = c(8, "plain", "black"), 
                       xlim = c(0,12),break.time.by = 2, ylim = c(0,0.6),
                       legend = c("top"),
                       legend.title = "", legend.labs = c("Comparison","CRS"), 
                       font.legend = c(10,"plain", "black"),
                       risk.table = T,
                       risk.table.title = "Number at risk by time",risk.table.col = "black",
                       risk.table.fontsize = 3, risk.table.y.text.col = F,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ggtheme = ggplot2::theme_classic())


KM_CRS_ANX$plot <- KM_CRS_ANX$plot + labs(
  title    = "Anxiety",                     
  subtitle = "",  
  caption  = paste("\n","Unadjusted HR =",round(hr, 2),",","95% CI",
                   "(", round(ci[1], 2), "-", round(ci[2], 2), ")"))
KM_CRS_ANX

###########################################################################################

###########################################################################################
P_ANX_fit <- survfit(Surv(time_ANX_Y, ANX) ~ CRSpolyp, data = CRS_polyp_MDO, type = 'kaplan-meier')
cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRSpolyp, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
KM_CRS_ANX<-ggsurvplot(P_ANX_fit, fun = "cumhaz", color = NULL, 
                       palette = c("green","red","blue"),
                       surv.scale = c("default", "percent"),
                       conf.int = F, conf.int.fill = "gray", censor = F, pval = FALSE,
                       pval.size = 5, pval.coord = c(NULL, NULL), 
                       xlab = "Time (Years)",
                       ylab = "Cumulative hazard", 
                       font.main = c(16, "plain", "black"),
                       font.x = c(14, "plain", "black"), 
                       font.y = c(14, "plain", "black"),
                       font.tickslab = c(8, "plain", "black"), 
                       xlim = c(0,12),break.time.by = 2, ylim = c(0,0.6),
                       legend = c("top"),
                       legend.title = "", legend.labs = c("Comparison","CRSwNP","CRSsNP"), 
                       font.legend = c(10,"plain", "black"),
                       risk.table = T,
                       risk.table.title = "Number at risk by time",risk.table.col = "black",
                       risk.table.fontsize = 3, risk.table.y.text.col = F,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ggtheme = ggplot2::theme_classic())


KM_CRS_ANX$plot <- KM_CRS_ANX$plot + labs(
  title    = "Anxiety",                     
  subtitle = "",  
  caption  = paste("\n","Unadjusted HR (CRSwNP) =",round(hr[1], 2),",","95% CI",
                   "(", round(ci[1], 2), "-", round(ci[3], 2), ")",
                   "\n","Unadjusted HR (CRSsNP) =",round(hr[2], 2),",","95% CI",
                   "(", round(ci[2], 2), "-", round(ci[4], 2), ")"))
KM_CRS_ANX
###########################################################################################


###########################################################################################
P_DEP_fit <- survfit(Surv(time_DEP_Y, DEP) ~ CRSpolyp, data = CRS_polyp_MDO, type = 'kaplan-meier')
cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRSpolyp, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
KM_CRS_DEP<-ggsurvplot(P_DEP_fit, fun = "cumhaz", color = NULL, 
                       palette = c("green","red","blue"),
                       surv.scale = c("default", "percent"),
                       conf.int = F, conf.int.fill = "gray", censor = F, pval = FALSE,
                       pval.size = 5, pval.coord = c(NULL, NULL), 
                       xlab = "Time (Years)",
                       ylab = "Cumulative hazard", 
                       font.main = c(16, "plain", "black"),
                       font.x = c(14, "plain", "black"), 
                       font.y = c(14, "plain", "black"),
                       font.tickslab = c(8, "plain", "black"), 
                       xlim = c(0,12),break.time.by = 2, ylim = c(0,0.6),
                       legend = c("top"),
                       legend.title = "", legend.labs = c("Comparison","CRSwNP","CRSsNP"),
                       font.legend = c(10,"plain", "black"),
                       risk.table = T,
                       risk.table.title = "Number at risk by time",risk.table.col = "black",
                       risk.table.fontsize = 3, risk.table.y.text.col = F,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ggtheme = ggplot2::theme_classic())


KM_CRS_DEP$plot <- KM_CRS_DEP$plot + labs(
  title    = "Depression",                     
  subtitle = "",  
  caption  = paste("\n","Unadjusted HR (CRSwNP) =",round(hr[1], 2),",","95% CI",
                   "(", round(ci[1], 2), "-", round(ci[3], 2), ")",
                   "\n","Unadjusted HR (CRSsNP) =",round(hr[2], 2),",","95% CI",
                   "(", round(ci[2], 2), "-", round(ci[4], 2), ")"))
KM_CRS_DEP

###########################################################################################
###########################################################################################

CRS_polyp_MDO

setnames(CRS_polyp_MDO,"CTRB_PT_TYPE_CD","Income")
setnames(CRS_polyp_MDO,"SIDO","Residence")

###########################################################################################
# ## AGE_GROUP
CRS_polyp_MDO[AGE_GROUP<= 9, AGE_GROUP := 1]
CRS_polyp_MDO[AGE_GROUP>= 10 & AGE_GROUP <= 13, AGE_GROUP := 2]
CRS_polyp_MDO[AGE_GROUP>= 14 , AGE_GROUP := 3]
# 
# ## 吏?뿭 (1 : ?꽌?슱, 2 : 湲고? 愿묒뿭?떆, 3 : ?룄 吏?뿭)
CRS_polyp_MDO[Residence == 11, Residence := 1]
CRS_polyp_MDO[Residence>= 26 & Residence <= 31, Residence := 2]
CRS_polyp_MDO[Residence>= 36 , Residence := 3]
# 
# ## 蹂댄뿕猷? 援ш컙 (1 : 0-30%, 2 : 30-70%, 3 : 70-100%)
CRS_polyp_MDO[Income < 4, Income := 1]
CRS_polyp_MDO[Income>= 4 & Income <= 7, Income := 2]
CRS_polyp_MDO[Income> 7 , Income := 3]

## AGE_GROUP 1: "<45 years old",2: "45-64 years old", 3: ">64 years old"
CRS_polyp_MDO[, AGE_GROUP:= as.character(AGE_GROUP)] #change data type
CRS_polyp_MDO[AGE_GROUP == "1", AGE_GROUP := "1. <45 years old"]
CRS_polyp_MDO[AGE_GROUP == "2", AGE_GROUP := "2. 45-64 years old"]
CRS_polyp_MDO[AGE_GROUP == "3", AGE_GROUP := "3. >64 years old"]
#
## SEX 1:male, 2:female
CRS_polyp_MDO[, SEX:= as.character(SEX)]
CRS_polyp_MDO[SEX == "1", SEX := "1. Male"]
CRS_polyp_MDO[SEX == "2", SEX := "2. Female"]
#
## SIDO 1: ?꽌?슱, 2: 遺?궛??援ъ씤泥쒓킅二쇰??쟾?슱?궛(愿묒뿭?떆), 3: 寃쎄린媛뺤썝異⑹껌?쟾?씪寃쎌긽?젣二쇱꽭醫?
CRS_polyp_MDO[, Residence:= as.character(Residence)]
CRS_polyp_MDO[Residence == "1", Residence := "1. Seoul"]
CRS_polyp_MDO[Residence == "2", Residence := "2. Other metropolitan"]
CRS_polyp_MDO[Residence == "3", Residence := "3. Do"]
#
## Income 1: ?냼?뱷遺꾩쐞 1~3, 2: ?냼?뱷遺꾩쐞 4~7, 3:?냼?뱷遺꾩쐞 8~10
CRS_polyp_MDO[, Income:= as.character(Income)]
CRS_polyp_MDO[Income == "1", Income := "1. 0-30%"]#?냼?뱷遺꾩쐞1~3
CRS_polyp_MDO[Income == "2", Income := "2. 30-70%"]#?냼?뱷遺꾩쐞4~7
CRS_polyp_MDO[Income == "3", Income := "3. 70-100%"]#?냼?뱷遺꾩쐞8~10


## CRSsPolyp 0: 
CRS_polyp_MDO[, CRSpolyp:= as.character(CRSpolyp)]
CRS_polyp_MDO[CRSpolyp == "1", CRSpolyp := "1.Comparision"]#?냼?뱷遺꾩쐞1~3
CRS_polyp_MDO[CRSpolyp == "2", CRSpolyp := "2.CRSwPolyp"]#?냼?뱷遺꾩쐞4~7
CRS_polyp_MDO[CRSpolyp == "3", CRSpolyp := "3.CRSsPolyp"]#?냼?뱷遺꾩쐞8~10

levels(CRS_polyp_MDO$CRSpolyp)<-c("2.Comparision","0.CRSsPolyp","1.CRSwPolyp")

table(CRS_polyp_MDO$CRSpolyp)

CRS_polyp_MDO$CRSpolyp = factor(CRS_polyp_MDO$CRSpolyp, levels=c("1.Comparision","3.CRSsPolyp","2.CRSwPolyp"))


CRS_polyp_MDO[, CRS:= as.character(CRS)]
CRS_polyp_MDO[CRS == "1", CRS := "1.CRS"]
CRS_polyp_MDO[CRS == "0", CRS := "0.Comparision"]
############################################################################################
# TABLE
############################################################################################
library(moonBook)
library(ztable)
a<-mytable(CRSpolyp~SEX+AGE_GROUP+Residence+Income,data = CRS_polyp_MDO)

table(CRS_polyp_MDO$CRSpolyp)

options(ztable.type="viewer")

ztable(a,caption="Table 1. Characteristics of the study subjects")

############################################################################################
result <- matrix(nrow = 19, ncol = 7)
colnames(result) <- c("Variables","n", "Cases", "Person-years", "Incidence",
                      "Unadjusted HR (95% CI)", "Adjusted HR (95% CI)")
result[, 1] <- c("Group", paste0(names(table(CRS_polyp_MDO$CRSpolyp))),
                 "Gender", paste0(names(table(CRS_polyp_MDO$SEX))),
                 "Age (years)", paste0(names(table(CRS_polyp_MDO$AGE_GROUP))),
                 "Residence", paste0(names(table(CRS_polyp_MDO$Residence))),
                 "Income", paste0(names(table(CRS_polyp_MDO$Income))))

## N
result[2:4, 2]   <- table(CRS_polyp_MDO$CRSpolyp)
result[6:7, 2]   <- table(CRS_polyp_MDO$SEX)
result[9:11, 2]  <- table(CRS_polyp_MDO$AGE_GROUP)
result[13:15, 2] <- table(CRS_polyp_MDO$Residence)
result[17:19, 2] <- table(CRS_polyp_MDO$Income)

## cases
result[2:4, 3]   <- table(CRS_polyp_MDO$CRSpolyp, CRS_polyp_MDO$ANX)[, 2]
result[6:7, 3]   <- table(CRS_polyp_MDO$SEX, CRS_polyp_MDO$ANX)[, 2]
result[9:11, 3]  <- table(CRS_polyp_MDO$AGE_GROUP, CRS_polyp_MDO$ANX)[, 2]
result[13:15, 3] <- table(CRS_polyp_MDO$Residence, CRS_polyp_MDO$ANX)[, 2]
result[17:19, 3] <- table(CRS_polyp_MDO$Income, CRS_polyp_MDO$ANX)[, 2]

## person-years
result[2:4, 4]   <- round(aggregate(time_ANX_Y ~ CRSpolyp, CRS_polyp_MDO, sum)[,2],1)
result[6:7, 4]   <- round(aggregate(time_ANX_Y ~ SEX, CRS_polyp_MDO, sum)[,2],1)
result[9:11, 4]  <- round(aggregate(time_ANX_Y ~ AGE_GROUP, CRS_polyp_MDO, sum)[,2],1)
result[13:15, 4] <- round(aggregate(time_ANX_Y ~ Residence, CRS_polyp_MDO, sum)[,2],1)
result[17:19, 4] <- round(aggregate(time_ANX_Y ~ Income, CRS_polyp_MDO, sum)[,2],1)

## incidence rate per 1,000 person-years
result[, 5]   <- round(as.numeric(result[, 3]) / as.numeric(result[, 4]) * 1000, 1)

## univariate Cox
result[2, 6]  <- "1.00 (ref)"
result[6, 6]  <- "1.00 (ref)"
result[9, 6]  <- "1.00 (ref)"
result[13, 6] <- "1.00 (ref)"
result[17, 6] <- "1.00 (ref)"

cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRSpolyp, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[3:4, 6]     <- paste0(round(hr, 2), " ", round(coef(summary(cox))[5],3),"(", round(ci[,1], 2), "-", round(ci[,2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ SEX, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[7, 6]     <- paste0(round(hr, 2), " ", round(coef(summary(cox))[5],3), "(", round(ci[1], 2), "-", round(ci[2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(AGE_GROUP), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[10:11, 6]  <- paste0(round(hr, 2), " ", round(coef(summary(cox))[5],3), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(Residence), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[14:15, 6] <- paste0(round(hr, 2), " ", round(coef(summary(cox))[5],3), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(Income), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[18:19, 6] <- paste0(round(hr, 2), " ", round(coef(summary(cox))[5],3), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

## multiple Cox
result[2, 7]  <- 1
result[6, 7]  <- 1
result[9, 7]  <- 1
result[13, 7] <- 1
result[17, 7] <- 1

cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRSpolyp + factor(SEX) + factor(AGE_GROUP) + 
               factor(Residence)+ factor(Income), 
             data = CRS_polyp_MDO)

hr <- exp(coef(cox))
ci <- exp(confint(cox))

m.cox <- cbind(hr, ci) 
m.cox <- data.frame(m.cox)
m.cox$hrci <- paste0(round(m.cox[[1]], 2), "(", round(m.cox[[2]], 2), "-", round(m.cox[[3]], 2), ")")

result[3:4, 7]    <- m.cox[1:2, 4]
result[7,7] <- m.cox[3,4]
result[10:11, 7] <- m.cox[4:5, 4]
result[14:15, 7] <- m.cox[6:7, 4]
result[18:19, 7]   <- m.cox[8:9, 4]

result[is.na(result)] <- ""

## delete auto row numbering
a<-as.data.frame(result)
rownames(a)<-a$Variables
a$Variables <- NULL

library(ztable)
ztable(a,align="|c|c|c|c|c|c|c|c|",
       caption="Table. Incidence and hazard ratios(95% CI) for CRS - ANX")
#################################################################################################################

############################################################################################
result <- matrix(nrow = 19, ncol = 7)
colnames(result) <- c("Variables","N", "Cases", "Person-years", "Incidence",
                      "Unadjusted HR", "Adjusted HR")
result[, 1] <- c("Group", paste0(names(table(CRS_polyp_MDO$CRSpolyp))),
                 "Gender", paste0(names(table(CRS_polyp_MDO$SEX))),
                 "Ages(years)", paste0(names(table(CRS_polyp_MDO$AGE_GROUP))),
                 "Residence", paste0(names(table(CRS_polyp_MDO$Residence))),
                 "Income", paste0(names(table(CRS_polyp_MDO$Income))))

result
## N
result[2:4, 2]   <- table(CRS_polyp_MDO$CRSpolyp)
result[6:7, 2]   <- table(CRS_polyp_MDO$SEX)
result[9:11, 2]  <- table(CRS_polyp_MDO$AGE_GROUP)
result[13:15, 2] <- table(CRS_polyp_MDO$Residence)
result[17:19, 2] <- table(CRS_polyp_MDO$Income)

## cases
result[2:4, 3]   <- table(CRS_polyp_MDO$CRSpolyp, CRS_polyp_MDO$DEP)[, 2]
result[6:7, 3]   <- table(CRS_polyp_MDO$SEX, CRS_polyp_MDO$DEP)[, 2]
result[9:11, 3]  <- table(CRS_polyp_MDO$AGE_GROUP, CRS_polyp_MDO$DEP)[, 2]
result[13:15, 3] <- table(CRS_polyp_MDO$Residence, CRS_polyp_MDO$DEP)[, 2]
result[17:19, 3] <- table(CRS_polyp_MDO$Income, CRS_polyp_MDO$DEP)[, 2]
result
## person-years
result[2:4, 4]   <- round(aggregate(time_DEP_Y ~ CRSpolyp, CRS_polyp_MDO, sum)[,2],1)
result[6:7, 4]   <- round(aggregate(time_DEP_Y ~ SEX, CRS_polyp_MDO, sum)[,2],1)
result[9:11, 4]  <- round(aggregate(time_DEP_Y ~ AGE_GROUP, CRS_polyp_MDO, sum)[,2],1)
result[13:15, 4] <- round(aggregate(time_DEP_Y ~ Residence, CRS_polyp_MDO, sum)[,2],1)
result[17:19, 4] <- round(aggregate(time_DEP_Y ~ Income, CRS_polyp_MDO, sum)[,2],1)

## incidence rate per 1,000 person-years
result[, 5]   <- round(as.numeric(result[, 3]) / as.numeric(result[, 4]) * 1000, 1)
result
## univariate Cox
result[2, 6]  <- 1
result[6, 6]  <- 1
result[9, 6]  <- 1
result[13, 6] <- 1
result[17, 6] <- 1

cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRSpolyp, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[3:4, 6]     <- paste0(round(hr, 2), "(", round(ci[,1], 2), "-", round(ci[,2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ SEX, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[7, 6]     <- paste0(round(hr, 2), "(", round(ci[1], 2), "-", round(ci[2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(AGE_GROUP), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[10:11, 6]  <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(Residence), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[14:15, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(Income), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[18:19, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

## multiple Cox
result[2, 7]  <- 1
result[6, 7]  <- 1
result[9, 7]  <- 1
result[13, 7] <- 1
result[17, 7] <- 1

cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRSpolyp + factor(SEX) + factor(AGE_GROUP) + 
               factor(Residence)+ factor(Income), 
             data = CRS_polyp_MDO)

hr <- exp(coef(cox))
ci <- exp(confint(cox))

m.cox <- cbind(hr, ci) 
m.cox <- data.frame(m.cox)
m.cox$hrci <- paste0(round(m.cox[[1]], 2), "(", round(m.cox[[2]], 2), "-", round(m.cox[[3]], 2), ")")

result[3:4, 7]    <- m.cox[1:2, 4]
result[7,7] <- m.cox[3,4]
result[10:11, 7] <- m.cox[4:5, 4]
result[14:15, 7] <- m.cox[6:7, 4]
result[18:19, 7]   <- m.cox[8:9, 4]

result[is.na(result)] <- ""

## delete auto row numbering
a<-as.data.frame(result)
rownames(a)<-a$Variables
a$Variables <- NULL

library(ztable)
ztable(a,align="|c|c|c|c|c|c|c|c|",
       caption="Table. Incidence and hazard ratios(95% CI) for CRS - DEP")
#################################################################################################################

############################################################################################
result <- matrix(nrow = 18, ncol = 7)
colnames(result) <- c("Variables","N", "Cases", "Person-years", "Incidence",
                      "Unadjusted HR", "Adjusted HR")
result[, 1] <- c("Group", paste0(names(table(CRS_polyp_MDO$CRS))),
                 "Gender", paste0(names(table(CRS_polyp_MDO$SEX))),
                 "Ages(years)", paste0(names(table(CRS_polyp_MDO$AGE_GROUP))),
                 "Residence", paste0(names(table(CRS_polyp_MDO$Residence))),
                 "Income", paste0(names(table(CRS_polyp_MDO$Income))))
## N
result[2:3, 2]   <- table(CRS_polyp_MDO$CRS)
result[5:6, 2]   <- table(CRS_polyp_MDO$SEX)
result[8:10, 2]  <- table(CRS_polyp_MDO$AGE_GROUP)
result[12:14, 2] <- table(CRS_polyp_MDO$Residence)
result[16:18, 2] <- table(CRS_polyp_MDO$Income)

## cases
result[2:3, 3]   <- table(CRS_polyp_MDO$CRS, CRS_polyp_MDO$DEP)[, 2]
result[5:6, 3]   <- table(CRS_polyp_MDO$SEX, CRS_polyp_MDO$DEP)[, 2]
result[8:10, 3]  <- table(CRS_polyp_MDO$AGE_GROUP, CRS_polyp_MDO$DEP)[, 2]
result[12:14, 3] <- table(CRS_polyp_MDO$Residence, CRS_polyp_MDO$DEP)[, 2]
result[16:18, 3] <- table(CRS_polyp_MDO$Income, CRS_polyp_MDO$DEP)[, 2]
result
## person-years
result[2:3, 4]   <- round(aggregate(time_DEP_Y ~ CRS, CRS_polyp_MDO, sum)[,2],1)
result[5:6, 4]   <- round(aggregate(time_DEP_Y ~ SEX, CRS_polyp_MDO, sum)[,2],1)
result[8:10, 4]  <- round(aggregate(time_DEP_Y ~ AGE_GROUP, CRS_polyp_MDO, sum)[,2],1)
result[12:14, 4] <- round(aggregate(time_DEP_Y ~ Residence, CRS_polyp_MDO, sum)[,2],1)
result[16:18, 4] <- round(aggregate(time_DEP_Y ~ Income, CRS_polyp_MDO, sum)[,2],1)

## incidence rate per 1,000 person-years
result[, 5]   <- round(as.numeric(result[, 3]) / as.numeric(result[, 4]) * 1000, 1)
## univariate Cox
result[2, 6]  <- 1
result[5, 6]  <- 1
result[8, 6]  <- 1
result[12, 6] <- 1
result[16, 6] <- 1

result

cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRS, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[3, 6]     <- paste0(round(hr, 2), "(", round(ci[,1], 2), "-", round(ci[,2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ SEX, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[6, 6]     <- paste0(round(hr, 2), "(", round(ci[1], 2), "-", round(ci[2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(AGE_GROUP), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[9:10, 6]  <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(Residence), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[13:14, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_DEP_Y, DEP) ~ factor(Income), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[17:18, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

## multiple Cox
result[2, 7]  <- 1
result[5, 7]  <- 1
result[8, 7]  <- 1
result[12, 7] <- 1
result[16, 7] <- 1

cox <- coxph(Surv(time_DEP_Y, DEP) ~ CRS + factor(SEX) + factor(AGE_GROUP) + 
               factor(Residence)+ factor(Income), 
             data = CRS_polyp_MDO)

hr <- exp(coef(cox))
ci <- exp(confint(cox))

m.cox <- cbind(hr, ci) 
m.cox <- data.frame(m.cox)
m.cox$hrci <- paste0(round(m.cox[[1]], 2), "(", round(m.cox[[2]], 2), "-", round(m.cox[[3]], 2), ")")


result[3, 7]    <- m.cox[1, 4]
result[6,7] <- m.cox[2,4]
result[9:10, 7] <- m.cox[3:4, 4]
result[13:14, 7] <- m.cox[5:6, 4]
result[17:18, 7]   <- m.cox[7:8, 4]

result[is.na(result)] <- ""

## delete auto row numbering
a<-as.data.frame(result)
rownames(a)<-a$Variables
a$Variables <- NULL

library(ztable)
ztable(a,align="|c|c|c|c|c|c|c|c|",
       caption="Table. Incidence and hazard ratios(95% CI) for CRS - DEP")
#################################################################################################################


############################################################################################
result <- matrix(nrow = 18, ncol = 7)
colnames(result) <- c("Variables","N", "Cases", "Person-years", "Incidence",
                      "Unadjusted HR", "Adjusted HR")
result[, 1] <- c("Group", paste0(names(table(CRS_polyp_MDO$CRS))),
                 "Gender", paste0(names(table(CRS_polyp_MDO$SEX))),
                 "Ages(years)", paste0(names(table(CRS_polyp_MDO$AGE_GROUP))),
                 "Residence", paste0(names(table(CRS_polyp_MDO$Residence))),
                 "Income", paste0(names(table(CRS_polyp_MDO$Income))))
## N
result[2:3, 2]   <- table(CRS_polyp_MDO$CRS)
result[5:6, 2]   <- table(CRS_polyp_MDO$SEX)
result[8:10, 2]  <- table(CRS_polyp_MDO$AGE_GROUP)
result[12:14, 2] <- table(CRS_polyp_MDO$Residence)
result[16:18, 2] <- table(CRS_polyp_MDO$Income)

## cases
result[2:3, 3]   <- table(CRS_polyp_MDO$CRS, CRS_polyp_MDO$ANX)[, 2]
result[5:6, 3]   <- table(CRS_polyp_MDO$SEX, CRS_polyp_MDO$ANX)[, 2]
result[8:10, 3]  <- table(CRS_polyp_MDO$AGE_GROUP, CRS_polyp_MDO$ANX)[, 2]
result[12:14, 3] <- table(CRS_polyp_MDO$Residence, CRS_polyp_MDO$ANX)[, 2]
result[16:18, 3] <- table(CRS_polyp_MDO$Income, CRS_polyp_MDO$ANX)[, 2]
result
## person-years
result[2:3, 4]   <- round(aggregate(time_ANX_Y ~ CRS, CRS_polyp_MDO, sum)[,2],1)
result[5:6, 4]   <- round(aggregate(time_ANX_Y ~ SEX, CRS_polyp_MDO, sum)[,2],1)
result[8:10, 4]  <- round(aggregate(time_ANX_Y ~ AGE_GROUP, CRS_polyp_MDO, sum)[,2],1)
result[12:14, 4] <- round(aggregate(time_ANX_Y ~ Residence, CRS_polyp_MDO, sum)[,2],1)
result[16:18, 4] <- round(aggregate(time_ANX_Y ~ Income, CRS_polyp_MDO, sum)[,2],1)

## incidence rate per 1,000 person-years
result[, 5]   <- round(as.numeric(result[, 3]) / as.numeric(result[, 4]) * 1000, 1)
## univariate Cox
result[2, 6]  <- 1
result[5, 6]  <- 1
result[8, 6]  <- 1
result[12, 6] <- 1
result[16, 6] <- 1

result

cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRS, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[3, 6]     <- paste0(round(hr, 2), "(", round(ci[,1], 2), "-", round(ci[,2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ SEX, data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[6, 6]     <- paste0(round(hr, 2), "(", round(ci[1], 2), "-", round(ci[2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(AGE_GROUP), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[9:10, 6]  <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(Residence), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[13:14, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

cox <- coxph(Surv(time_ANX_Y, ANX) ~ factor(Income), data = CRS_polyp_MDO)
hr <- exp(coef(cox))
ci <- exp(confint(cox))
result[17:18, 6] <- paste0(round(hr, 2), "(", round(ci[, 1], 2), "-", round(ci[, 2], 2), ")")

## multiple Cox
result[2, 7]  <- 1
result[5, 7]  <- 1
result[8, 7]  <- 1
result[12, 7] <- 1
result[16, 7] <- 1

cox <- coxph(Surv(time_ANX_Y, ANX) ~ CRS + factor(SEX) + factor(AGE_GROUP) + 
               factor(Residence)+ factor(Income), 
             data = CRS_polyp_MDO)

hr <- exp(coef(cox))
ci <- exp(confint(cox))

m.cox <- cbind(hr, ci) 
m.cox <- data.frame(m.cox)
m.cox$hrci <- paste0(round(m.cox[[1]], 2), "(", round(m.cox[[2]], 2), "-", round(m.cox[[3]], 2), ")")


result[3, 7]    <- m.cox[1, 4]
result[6,7] <- m.cox[2,4]
result[9:10, 7] <- m.cox[3:4, 4]
result[13:14, 7] <- m.cox[5:6, 4]
result[17:18, 7]   <- m.cox[7:8, 4]

result[is.na(result)] <- ""

## delete auto row numbering
a<-as.data.frame(result)
rownames(a)<-a$Variables
a$Variables <- NULL

library(ztable)
ztable(a,align="|c|c|c|c|c|c|c|c|",
       caption="Table. Incidence and hazard ratios(95% CI) for CRS - ANX")
#################################################################################################################
