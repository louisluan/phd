load("~/CSMAR/reduced_ws.RData")
source("data_util.R")

#Earnings Management Data preparation--------------

em<-select(d_fs,Stkcd,year,SOE,INDC,AUDIT)
#总流动资产
em$TCA<-d_fs$A001100000
#现金
em$CASH<-d_fs$A001101000
#总流动负债
em$TCLIAB<-d_fs$A002100000
#折旧
em$DEPRE<-d_fs$D000103000
#总资产
em$TA<-d_fs$A001000000
#总收入
em$INCOME<-d_fs$B001100000
#固定资产
em$PPE<-d_fs$A001212000
#无形资产
em$INTANG<-d_fs$A001218000
#应收票据
em$NOTERCV<-d_fs$A001110000
#应收款
em$RCV<-d_fs$A001111000
#其他应收款
em$ORCV<-d_fs$A001121000
#一年内到期的长期负债
em$LIAB1Y<-d_fs$A002125000
#长期待摊费用
em$LTEXP<-d_fs$A001221000
#存货
em$INVENTORY<-d_fs$A001123000
#应付账款
em$AP<-d_fs$A002108000
#应付税费
em$TAXP<-d_fs$A002113000
#其他流动资产
em$OCA<-d_fs$A001125000



#helper function for regression-------

reg_jones_dd <- function(x,accrual,ta,revenue,ppe){
  message("Sort data id descending t before use")
  a_ta<-accrual/lag(ta) %>%
    winsor(0.01)
  one_ta<-1/lag(ta) %>%
    winsor(0.01)
  rev_ta<-(revenue-lag(revenue))/lag(ta) %>%
    winsor(0.01)
  ppe_ta<-lag(ppe)/lag(ta) %>%
    winsor(0.01)
  mds<-lm(a_ta ~ one_ta + rev_ta + ppe_ta + 0, data = subsetx)
  return(mds)
}






