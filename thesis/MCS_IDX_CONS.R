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

DD_IDX <- arrange(dr,Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(EXPN=OP_COST+MGMT_EXP+SALES_EXP+IMP_LOSS,
         COREREV=OP_INCOME+OP_TAX-FAIR_CH,
         EARN=(CORE_PROFIT+OP_TAX-FAIR_CH)/lag(MKT_CAP),
         # LEV=TT_LIAB/lag(MKT_CAP),
         LEV=DEBT_RATIO,
         SIZE=log(TT_ASSET),
         E_P=COREREV/lag(MKT_CAP),
         D=as.integer(YRET>=0),
         F_E_P=lead(E_P),
         FROE=lead(ROE),FYRET=lead(YRET),LROE=lag(ROE),LYRET=lag(YRET),
         LROA=lag(ROA),FROA=lead(ROA),LEPS=lag(EPS),FEPS=lead(EPS),
         LEARN=lag(EARN),FEARN=lead(EARN),LE_P=lag(E_P),FE_P=lead(E_P),
         LCOREREV=lag(COREREV),FCOREREV=lead(COREREV),
         DLOSS=as.integer(COREREV<0)  ) %>%
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
DIDX$FY<-factor(DIDX$year)

DIDX <- mutate(DIDX,
               MCSINDEX= 30*mgmt_op + 10*csr_con  + 
                 10*ctrl_cap + 20*mkt_inno + 20*audit_unq + 
                 5*mgmt_dplm + 5*log(mgmt_exp),
               DC_MS = ntile(MCSINDEX,10),
               QC_MS =ntile(MCSINDEX,5),
               DMS=DC_MS<3) %>%
  winsor_df


load("Conservatism_Data.RData")
load("Earnings_Mangement.RData")
load("Over_Ivest.RData")

MIDX <-left_join(DIDX,CS,by=c("Stkcd","year") ) %>%
  left_join(OV) %>%
  left_join(EM) %>%
  mutate(OVER_INV=OVER_INV/10000) %>%
  winsor_df

MIDX$DY <- MIDX$YRET<0

fm_cs <-  CSCORE ~  LEV + SIZE + SOE  +OVER_INV*MCSINDEX +INDCD 

CS_DD <-plm(fm_cs,data=subset(MIDX,OVER_INV<0),model="pooling",index=c("Stkcd","year"))
summary(CS_DD)


fm_cs1 <-  CSMATCH ~  LEV + SIZE + SOE  +OVER_INV*MCSINDEX +INDCD 

CS_DD1 <-plm(fm_cs1,data=subset(MIDX,OVER_INV<0),model="pooling",index=c("Stkcd","year"))
summary(CS_DD1)

fm_cs2 <-  CSCORE ~  LEV + SIZE + SOE  +OVER_INV*MCSINDEX +INDCD 

CS_DD2 <-plm(fm_cs2,data=subset(MIDX,OVER_INV>0),model="pooling",index=c("Stkcd","year"))
summary(CS_DD2)

fm_cs3 <-  CSMATCH ~  LEV + SIZE + SOE  +OVER_INV*MCSINDEX +INDCD 

CS_DD3 <-plm(fm_cs3,data=subset(MIDX,OVER_INV>0),model="pooling",index=c("Stkcd","year"))
summary(CS_DD3)


o_descriptive(as.data.frame(select(MIDX,LEV,SIZE,SOE,MCSINDEX,CSCORE,CSMATCH,OVER_INV) ),
              head="CONS_Descriptives",file="CONS_DESC.htm"
)

o_corr(corr(select(MIDX,LEV,SIZE,SOE,MCSINDEX,CSCORE,CSMATCH,OVER_INV) ),
              head="CONS_CORR",file="CONS_CORR.htm"
)

o_reg(CS_DD,CS_DD1,CS_DD2,CS_DD3,colnames=T,
      head="CONS_REG",file="CONS_REG.htm",
      ctrl="INDC",ctlab="Controls",
      ylab=c("CSCORE","MATCHING","CSCORE","MATCHING") )



