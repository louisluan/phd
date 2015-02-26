#Initialize data environment
DIRS_MAC_DIR <- "~/Documents/phd/thesis/"
DIRS_WIN_DIR <- "C:/programs/git/phd/phd/thesis/"
DIRS_DATA_UTIL <-"data_util.R"
DIRS_FILE_UTIL <- "file_util.R"
DIRS_TEST_MAC <- list.dirs(DIRS_MAC_DIR)
DIRS_RDATA_WIN <- "D:/CSMAR/rdata"
DIRS_RDATA_MAC <-"~/CSMAR/rdata/"
DIRS_RDATA <-ifelse(length(DIRS_TEST_MAC)>1,DIRS_RDATA_MAC, DIRS_RDATA_WIN)
DIRS_FP_DATA <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_DATA_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_DATA_UTIL,sep="") )

DIRS_FP_FILE <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_FILE_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_FILE_UTIL,sep="") )
setwd(DIRS_RDATA)
load("ALL_MANUAL_DATA.RData")
load("csmar_cleaned.RData")
source(DIRS_FP_DATA,encoding="UTF-8")

rm(list=ls(pattern = "DIRS_"))


require(plm)
require(sampleSelection)

DD_IDX <- arrange(dr,Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(EXPN=OP_COST+MGMT_EXP+SALES_EXP,
         COREREV=OP_INCOME+OP_TAX-FAIR_CH,
         EARN=(CORE_PROFIT+OP_TAX-FAIR_CH)/lag(MKT_CAP),
         # LEV=TT_LIAB/lag(MKT_CAP),
         ATO=OP_INCOME/lag(TT_ASSET),
         LEV=DEBT_RATIO,
         SIZE=log(TT_ASSET),
         E_P=COREREV/lag(MKT_CAP),
         D=as.integer(YRET>=0),
         F_E_P=lead(E_P),
         FROE=lead(ROE),FYRET=lead(YRET),LROE=lag(ROE),LYRET=lag(YRET),
         LROA=lag(ROA),FROA=lead(ROA),LEPS=lag(EPS),FEPS=lead(EPS),
         LEARN=lag(EARN),FEARN=lead(EARN),LE_P=lag(E_P),FE_P=lead(E_P),
         LCOREREV=lag(COREREV),FCOREREV=lead(COREREV),
         DLOSS=as.integer(COREREV-OP_COST<0)  ) %>%
  winsor_df(p=0.05) %>%
  ungroup() %>%
  p_balance(id="Stkcd",from=2007,to=2013)

DIDX <- left_join(dm,DD_IDX)
DIDX$AUDIT[is.na(DIDX$AUDIT)]<-T


DIDX$mgmt_op <- DIDX$sentiment 
DIDX$csr_con <- DIDX$csrscore>4
DIDX$ctrl_cap <- rowSums(select(DIDX,FINCORP,BUDGET,ERP,MIS)) >= 3
DIDX$mkt_inno <- rowSums(select(DIDX,WEIXIN,WEIBO)) >= 1
DIDX$audit_unq <- DIDX$AUDIT

DIDX$mgmt_dplm <- DIDX$MGMTMASTER/ifelse(DIDX$MGMTNUM==0,999,DIDX$MGMTNUM)
DIDX$mgmt_dplm[DIDX$mgmt_dplm>1]=1
DIDX$mgmt_dplm<-winsor(DIDX$mgmt_dplm,0.05)
DIDX$mgmt_exp <- DIDX$MGMTAVGAGE
DIDX$mgmt_exp<-winsor(DIDX$mgmt_exp,0.05)

DIDX <- mutate(DIDX,
               MCSINDEX= 30*mgmt_op + 10*csr_con  + 
                 10*ctrl_cap + 20*mkt_inno + 20*audit_unq + 
                 5*mgmt_dplm + 5*log(mgmt_exp),
               DC_MS = ntile(MCSINDEX,10),
               QC_MS =ntile(MCSINDEX,5),
               DMS=DC_MS<3) %>%
  winsor_df
load("Earnings_Mangement.RData")

MIDX<-left_join(DIDX,EM)



  FUNDH<-read.table("../HLD_Fundhold.csv",sep="\t",header = T,encoding = "UTF-8",
                    stringsAsFactors = F) %>%
  mutate(year=extractyear(Reptdt),Stkcd=X.U.FEFF.Stkcd) %>%
  filter(substr(Reptdt,6,10)=="12-31") %>%
  select(Stkcd,year,Holdperct) %>%
  group_by(Stkcd,year) %>%
  summarise(FUNDHD=sum(Holdperct)) %>%
  distinct(Stkcd,year) 


MIDX<-left_join(MIDX,FUNDH)




#   DADT<-read.table("../FIN_Audit.csv",sep="\t",header = T,encoding = "UTF-8",stringsAsFactors = F) %>%
#   mutate(FADT=nchar(Iadtunit),year=extractyear(Accper),
#          Stkcd=X.U.FEFF.Stkcd) %>%
#   select(Stkcd,year,FADT) %>%
#   group_by(Stkcd,year) %>%
#   summarise(FAUD=sum(FADT)) %>%
#   distinct(Stkcd,year) %>%
#   mutate(FADT=ifelse(FAUD>0,1,0) )
# 
# MIDX<-left_join(MIDX,DADT)

#fm_DD <- LU_DA ~  LEV + SIZE + SOE  + INDCD +FADT

#DD_DD <-lm(fm_DD,data=MIDX,na.action = na.omit)

DD_DD0<-lm(LU_DA ~  LEV+ SIZE + SOE +ROA + DLOSS*MCSINDEX +INDC,data=MIDX)
summary(DD_DD0)

DD_DD <-heckit(audit_unq~ SIZE+LEV+ROA+DLOSS+ATO,  
               LU_DA ~  LEV+ SIZE + SOE  +ROA + DLOSS*MCSINDEX+INDC ,data=MIDX)
summary(DD_DD)

DD_DD1 <-heckit(audit_unq~ SIZE+LEV+ROA+MCSINDEX+DLOSS+ATO,  
               FUNDHD ~  LEV+ SIZE + SOE  + LU_DA*MCSINDEX ,data=MIDX)
summary(DD_DD1)

MIDX$FUNDHD[is.na(MIDX$FUNDHD)]<-0
DD_DD3 <- lm(FUNDHD ~  LEV+ SIZE + SOE +ROA + MCSINDEX+INDCD+factor(year),data=MIDX)
summary(DD_DD3)

MIDX$FUNDHD[is.na(MIDX$FUNDHD)]<-0
DD_DD2 <- lm(FUNDHD ~  LEV+ SIZE + SOE  + LU_DA*MCSINDEX +INDCD,data=MIDX)
summary(DD_DD2)


o_descriptive(as.data.frame(select(MIDX,LEV,SIZE, SOE,ROA,MCSINDEX,audit_unq,FUNDHD,DLOSS,LU_DA) ),
              head="EM_Descriptives",file="MCS_EM_DESC.htm"
              )
o_corr(corr(select(MIDX,LEV,SIZE, SOE,ROA,MCSINDEX,audit_unq,FUNDHD,DLOSS,LU_DA) ),
       head="EM_CORR",file="MCS_EM_CORR.htm"
  )

o_reg<-function(...,head="Regression Result",ylab,ctrl,ctlab,file="./reg.htm"){
  stargazer(...,type="html",title=head,
            report="vc*p",digits=3,
            dep.var.labels=ylab,
            model.names=FALSE,model.numbers=TRUE,
            header=FALSE,
            omit=ctrl,omit.labels=ctlab,
            selection.equation = F,
            flip=TRUE,out.header=TRUE,out=file)
}

o_reg(DD_DD0,head="EM_DLOSS",ylab="Discretional Accrual",
      ctrl="INDC",ctlab="Controls",
      file="EM_DLOSS_REG.htm")

o_reg(DD_DD2,head="EM_FUNDHD",ylab="Fund Hold %",
      ctrl="INDC",ctlab="Controls",
      file="EM_FUNDHD_REG.htm")

o_reg(DD_DD,head="EM_HECKIT",ylab="Discretional Accrual",
      ctrl="INDC",ctlab="Controls",
      file="EM_DLOSS_HECKIT.htm")

o_reg(DD_DD1,head="FD_HECKIT",ylab="Fund Hold %",
      ctrl="INDC",ctlab="Controls",
      file="FUND_HECKIT.htm")

