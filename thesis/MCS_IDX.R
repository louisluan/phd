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

o_descriptive(as.data.frame( select(DIDX,mgmt_op:mgmt_exp) ),
              head="Collected Data Descriptives",
              file="MCS_Colleced_DESC.htm")

o_corr(corr(select(DIDX,mgmt_op:mgmt_exp) ),
       head="Collected Data Correlation",
       file="MCS_Collected_CORR.htm")

DIDX$FY<-factor(DIDX$year)


fm_lm_ret<-YRET ~  mgmt_op + csr_con  + 
    ctrl_cap + mkt_inno + audit_unq + 
  mgmt_dplm + mgmt_exp   + LEV + SIZE 

DD_PLM_RET <-plm(fm_lm_ret,data=DIDX,model="within",index=c("Stkcd","year"))
summary(DD_PLM_RET)


o_reg(DD_PLM_RET,head="MCS Factors TO RETURN Regression Result",
      ctrl="FY",ctlab="Controls",ylab="Annual Stock Return",
      file="MCS_Factors_REG_RETURN.htm")

fm_lm_fern <- F_E_P ~     mgmt_op + csr_con  + 
    ctrl_cap  + mkt_inno + audit_unq + 
  mgmt_dplm + mgmt_exp   + LEV + SIZE +SOE  


LM_FERN <-plm(fm_lm_fern,data=DIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_FERN)

fm_lm_ern <- E_P ~     mgmt_op + csr_con  + 
  ctrl_cap  + mkt_inno + audit_unq + 
  mgmt_dplm + mgmt_exp   + LEV + SIZE +SOE  


LM_ERN <-plm(fm_lm_ern,data=DIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_ERN)

o_reg(LM_ERN,LM_FERN,head="MCS Factors TO EARINGS Regression Result",
      ctrl="FY",ctlab="Controls",ylab=c("Scaled t Earnings","Scaled t+1 Earnings"),
      file="MCS_Factors_REG_EARNINGS.htm")

DIDX <- mutate(DIDX,
               MCSINDEX= 30*mgmt_op + 10*csr_con  + 
  10*ctrl_cap + 20*mkt_inno + 20*audit_unq + 
  5*mgmt_dplm + 5*log(mgmt_exp),
  DC_MS = ntile(MCSINDEX,10),
  QC_MS =ntile(MCSINDEX,5),
  DMS=DC_MS<3) %>%
  winsor_df

DIDX2011 <- filter(DIDX,year==2011) %>%
  ungroup() %>%
  mutate(MCSINDEX2011=MCSINDEX) %>%
  select(MCSINDEX2011) %>%
  as.data.frame
DIDX2012 <- filter(DIDX,year==2012) %>%
  ungroup() %>%
  mutate(MCSINDEX2012=MCSINDEX) %>%
  select(MCSINDEX2012) %>%
  as.data.frame
DIDX2013 <- filter(DIDX,year==2013) %>%
  ungroup() %>%
  mutate(MCSINDEX2013=MCSINDEX) %>%
  select(MCSINDEX2013) %>%
  as.data.frame
MCSINDEX<-cbind(DIDX2011,DIDX2012,DIDX2013)


o_descriptive(MCSINDEX ,
              head="MCS INDEX Descriptive Statitics ",
              file="MCS_INDEX_DESC.htm")

o_png(file="MCS_INDEX_HIST.PNG",w=800,h=600)
hist(DIDX$MCSINDEX,main="MCS指数直方图",xlab="MCS INDEX")
dev.off()

fm_lm_idx <- E_P ~    MCSINDEX   + LEV + SIZE +FY +SOE + INDCD

LM_IDX <-plm(fm_lm_idx,data=DIDX,model="pooling",index=c("Stkcd","year"))
summary(LM_IDX)

o_descriptive(as.data.frame(select(DIDX,E_P,YRET,MCSINDEX,LEV,SIZE,SOE,DMS) ),
              head="MCS INDEX PERFORMANCE DESCRIPTIVES",
              file="MCS_INDEX_PERF_DESC.htm")

o_corr(corr(as.matrix(select(DIDX,E_P,YRET,MCSINDEX,LEV,SIZE,SOE,DMS) ) ),
              head="MCS INDEX PERFORMANCE CORRELATION MATRIX",
              file="MCS_INDEX_PERF_CORR.htm")




fm_lm_idx_r02 <- YRET ~    E_P   + LEV + SIZE+ M_B + INDCD

LM_IDX_R02 <-lm(fm_lm_idx_r02,data=DIDX)
summary(LM_IDX_R02)

fm_lm_idx_r03 <- YRET ~    E_P*DMS   + LEV + SIZE+ M_B + INDCD

LM_IDX_R03 <-lm(fm_lm_idx_r03,data=DIDX)
summary(LM_IDX_R03)

o_reg(LM_IDX_R02,LM_IDX_R03,
      head="MCS INDEX ERC Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab=c("Return"),
      file="MCS_INDEX_ERC_REG.htm"
)


fm_lm_idx_r <- YRET ~    MCSINDEX   + LEV + SIZE+M_B+ INDCD +SOE+E_P

LM_IDX_R <-plm(fm_lm_idx_r,data=DIDX,model="pooling",index=c("Stkcd","year"))

fm_lm_idx_r1 <- FYRET ~    MCSINDEX   + LEV + SIZE+ M_B+INDCD +SOE+E_P

LM_IDX_R1 <-plm(fm_lm_idx_r1,data=DIDX,model="pooling",index=c("Stkcd","year"))

fm_lm_idx_r2 <- E_P ~    MCSINDEX   + LEV + SIZE+ SOE+INDCD 

LM_IDX_R2 <-plm(fm_lm_idx_r2,data=DIDX,model="pooling",index=c("Stkcd","year"))

fm_lm_idx_r3 <- F_E_P ~    MCSINDEX   + LEV + SIZE+ SOE+INDCD 

LM_IDX_R3 <-plm(fm_lm_idx_r3,data=DIDX,model="pooling",index=c("Stkcd","year"))

o_reg(LM_IDX_R,
      head="MCS INDEX PERFORMANCE CORRELATION Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab=c("Return t","Return t+1","Scaled Earnings t","Scaled Earnings t+1"),
      file="MCS_INDEX_PERF_REG2.htm"
      )

o_reg(LM_IDX_R2,LM_IDX_R3,
      head="MCS INDEX PERFORMANCE CORRELATION Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab=c("Scaled Earnings t","Scaled Earnings t+1"),
      file="MCS_INDEX_PERF_REG1.htm"
)

meansum<-group_by(DIDX,year,QC_MS) %>%
  summarise(mROE=mean(ROE,na.rm=T),mE_P=mean(E_P,na.rm = T),
          mYRET=mean(EARN,na.rm = T),
          mMCSIDEX=mean(MCSINDEX,na.rm = T))

stargazer(as.matrix(meansum),
              title="MCS INDEX QUINTILE DESCRIPTIVES",
          summary=F,
          digits=2,
          column.sep.width ="10pt",
          out.header=T,
              out="MCS_INDEX_PERF_QUIN.htm")

t.test(DIDX$ROE[DIDX$QC_MS==1 & DIDX$year==2011],
       DIDX$ROE[DIDX$QC_MS==5 & DIDX$year==2011],na.rm=T)

t.test(DIDX$ROE[DIDX$QC_MS==1 & DIDX$year==2012],
       DIDX$ROE[DIDX$QC_MS==5 & DIDX$year==2012],na.rm=T)

t.test(DIDX$ROE[DIDX$QC_MS==1 & DIDX$year==2013],
       DIDX$ROE[DIDX$QC_MS==5 & DIDX$year==2013],na.rm=T)


load("Earnings_Mangement.RData")

MIDX <- left_join(DIDX,EM) %>%
  winsor_df

o_descriptive(as.data.frame(select(MIDX,SOE,DLOSS,MCSINDEX,LEV,SIZE) ),
              head="EARNINGS MANGEMENT DESCRIPTIVES",
              file="MCS_INDEX_EM_DESC.htm")

o_corr(corr(as.matrix(select(MIDX,SOE,DLOSS,MCSINDEX,LEV,SIZE) ) ),
       head="EARNINGS MANGEMENT CORRELATION MATRIX",
       file="MCS_INDEX_EM_CORR.htm")

# 
# fm_dd <-  DD_DA ~   SIZE   + LEV + DC_MS*MLOSS + SOE 
# 
# LM_DD <-ivreg(fm_dd,data=MIDX)
# summary(LM_DD)
# 
# fm_md <-  MODI_DA ~   SIZE  + MCSINDEX + LEV + INDCD |.-MCSINDEX + ROE
# 
# MD_DD <-ivreg(fm_md,data=MIDX)
# summary(MD_DD)
# 
# fm_ld <-  LU_DA ~   SIZE  + MCSINDEX + LEV + INDCD |.-MCSINDEX + ROE
# 
# MD_LD <-ivreg(fm_ld,data=MIDX)
# summary(MD_LD)
fm_DD <- DD_DA  ~  LEV + SIZE + SOE  + INDCD +  MCSINDEX*DLOSS 

DD_DD <-lm(fm_DD,data=MIDX,na.action = na.omit)


fm_lu <- LU_DA  ~  LEV + SIZE + SOE  + INDCD +  MCSINDEX*DLOSS 

LU_DD <-plm(fm_lu,data=MIDX,model="pooling",index=c("Stkcd","year"))

summary(DD_DD)

o_reg(LU_DD,DD_DD,
      head="MCS INDEX EM CORRELATION Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab=c("Lu DA","DD DA"),
      file="MCS_INDEX_EM_REG.htm"
)

#Conservatism Reg----------------------------------
load("Conservatism_Data.RData")

MIDX <-left_join(MIDX,CS)
MIDX$DY <- MIDX$YRET<0


fm_cs <-  CSMATCH ~  LEV + SIZE + SOE  + MCSINDEX*DD_DA +INDCD

CS_DD <-plm(fm_cs,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(CS_DD)



fm_cc <-  GSCORE ~  LEV + SIZE + SOE  + MCSINDEX*DD_DA +INDCD

GC_DD <-plm(fm_gc,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(GC_DD)

o_descriptive(as.data.frame(select(MIDX,SOE,CSCORE,CSMATCH,MCSINDEX,LEV,SIZE) ),
              head="CONSERVATISM DESCRIPTIVES",
              file="MCS_INDEX_CONS_DESC.htm")

o_corr(corr(as.matrix(select(MIDX,SOE,CSCORE,CSMATCH,MCSINDEX,LEV,SIZE) ) ),
       head="CONSERVATISM CORRELATION MATRIX",
       file="MCS_INDEX_CONS_CORR.htm")

o_reg(CS_DD,GC_DD,
      head="MCS INDEX CONSERVATISM Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab=c("Matching","CSCORE"),
      file="MCS_INDEX_CONS_REG.htm")

#Over-Invest Reg
load("Over_Ivest.RData")

MIDX<-left_join(DIDX,OV) %>%
  winsor_df


fm_ov <-  OVER_INV ~   LEV + SOE + MCSINDEX*SIZE + INDCD

OV_DD <-plm(fm_ov,data=MIDX,model="pooling",index=c("Stkcd","year"))
summary(OV_DD)




o_descriptive(as.data.frame(dplyr::select(MIDX,SOE,OVER_INV,MCSINDEX,LEV,SIZE) ),
              head="OVER-INVESTMENT DESCRIPTIVES",
              file="MCS_INDEX_OVI_DESC.htm")

o_corr(corr(as.matrix(dplyr::select(MIDX,SOE,OVER_INV,MCSINDEX,LEV,SIZE) ) ),
       head="OVER-INVESTMENT CORRELATION MATRIX",
       file="MCS_INDEX_OVI_CORR.htm")

o_reg(OV_DD,
      head="MCS INDEX OVER-INVESTMENT Regression Result",
      ctrl="INDCD",ctlab="CONTROLS",
      ylab="OVER-INVESTMENT",
      file="MCS_INDEX_OVI_REG.htm")


