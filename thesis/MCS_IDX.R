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
         LEV=TT_LIAB/lag(MKT_CAP),
         SIZE=log(TT_ASSET),
         E_P=COREREV/lag(MKT_CAP),
         D=as.integer(YRET>=0),

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


fm_lm_ret<-YRET ~  mgmt_op + csr_con  + 
    ctrl_cap + mkt_inno + audit_unq + 
  mgmt_dplm + mgmt_exp   + LEV + SIZE

DD_PLM_RET <-plm(fm_lm_ret,data=DIDX,model="within",index=c("Stkcd","year"))
summary(DD_PLM_RET)

fm_lm_ern <- E_P ~     mgmt_op + csr_con  + 
    ctrl_cap  + mkt_inno + audit_unq + 
  mgmt_dplm + mgmt_exp   + LEV + SIZE


LM_ERN <-plm(fm_lm_ern,data=DIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_ERN)


DIDX <- mutate(DIDX,
               MSCORE= winsor(30*mgmt_op + 10*csr_con  + 
  10*ctrl_cap + 20*mkt_inno + 20*audit_unq + 
  5*mgmt_dplm + 5*log(mgmt_exp),p=0.05),
  DC_MS = ntile(MSCORE,10),
  DMS=DC_MS>8) %>% 
  group_by(year) 



fm_lm_idx <- E_P ~    MSCORE   + LEV + SIZE +FY

LM_IDX <-plm(fm_lm_idx,data=DIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_IDX)


fm_lm_idx_r <- YRET ~    MSCORE   + LEV + SIZE 

LM_IDX_R <-plm(fm_lm_idx_r,data=DIDX,model="within",index=c("Stkcd","year"))
summary(LM_IDX_R)



t.test(DIDX$YRET[DIDX$DC_MS==1 & DIDX$year==2012],DIDX$YRET[DIDX$DC_MS==10 & DIDX$year==2012],na.rm=T)


load("Earnings_Mangement.RData")

MIDX <- left_join(DIDX,EM) %>%
  winsor_df


fm_dd <-  DD_DA ~   SIZE + DEBT_RATIO + SOE  

LM_DD <-plm(fm_dd,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_DD)

fm_lu <- LU_DA  ~  LEV + SIZE + SOE  + FY +  MSCORE*DLOSS 

LU_DD <-plm(fm_lu,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(LU_DD)

fm_md <-  MODI_DA ~  LEV + SIZE + SOE  + MSCORE*E_P +FY

MD_DD <-plm(fm_md,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(MD_DD)

load("Conservatism_Data.RData")

MIDX <-left_join(MIDX,CS)


fm_cs <-  CSMATCH ~  LEV + SIZE + SOE  + MSCORE*DD_DA +FY

CS_DD <-plm(fm_cs,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(CS_DD)

fm_cc <-  CSCORE ~  LEV + SIZE + SOE  + MSCORE*DD_DA +FY

CC_DD <-plm(fm_cc,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(CC_DD)

fm_gc <-  GSCORE ~  LEV + SIZE + SOE  + MSCORE*DD_DA +FY

GC_DD <-plm(fm_cc,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(GC_DD)


o_descriptive(as.data.frame(select(d,mgmt_op:audit_unq)),
              head="MCS Index Components",
              file="./MCS_INDEX_COMPONENTS_DESC.htm"
)


load("Over_Ivest.RData")

MIDX<-left_join(DIDX,OV)

fm_ov <-  OVER_INV ~  LEV + SIZE + SOE  + MSCORE +FY

OV_DD <-plm(fm_ov,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(OV_DD)

fm_ov1 <-  FROE ~  LEV + SIZE + SOE  + MSCORE*OVER_INV +FY

OV1_DD <-plm(fm_ov1,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(OV1_DD)

fm_ov2 <-  E_P ~  LEV + SIZE + SOE  + MSCORE*OVER_INV +FY

OV2_DD <-plm(fm_ov2,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(OV2_DD)

