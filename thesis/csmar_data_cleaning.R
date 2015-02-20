setwd("~/Documents/phd/thesis/")
source("data_util.R")
source("file_util.R")

#read in data files----------------------------
setwd("~/CSMAR/")
cscode<-read.csv("~/Documents/phd/cscode.txt",stringsAsFactors=F,header = T)
q<-f_list_file()
f_readtable_list(q)
d_trd<-TRD_Year

#Combining data------------------------------
HLD_Contrshr <- arrange(HLD_Contrshr,desc(Reptdt)) %>%
  distinct(Stkcd)

j_fs <- filter(FS_Combas,substr(Accper,6,10)=="12-31") %>%
  left_join(FS_Comins,by=c("Stkcd","Accper")) %>%
  left_join(FS_Comscfd,by=c("Stkcd","Accper")) %>%
  left_join(FS_Comscfi,by=c("Stkcd","Accper")) %>%
  left_join(FI_T1,by=c("Stkcd","Accper")) %>%
  left_join(FI_T2,by=c("Stkcd","Accper")) %>%
  left_join(FI_T3,by=c("Stkcd","Accper")) %>%
  left_join(FI_T4,by=c("Stkcd","Accper")) %>%
  left_join(FI_T5,by=c("Stkcd","Accper")) %>%
  left_join(FI_T6,by=c("Stkcd","Accper")) %>%
  left_join(FI_T7,by=c("Stkcd","Accper")) %>%
  left_join(FI_T8,by=c("Stkcd","Accper")) %>%
  left_join(FI_T9,by=c("Stkcd","Accper")) %>%
  left_join(FI_T10,by=c("Stkcd","Accper")) %>%
  left_join(FIN_Audit,by=c("Stkcd","Accper")) %>%
  left_join(HLD_Contrshr,by=c("Stkcd")) %>%
  left_join(HLD_Copro,by=c("Stkcd")) 
  


#Varibale screening------------------------------
#根据手工处理的变量列表筛选一堆不用的变量，csmar的变量名不是给人类读的
d_fs<-j_fs[,names(j_fs) %in% cscode$var]

#去掉财务指标，缺失值太多了
d_fs<-d_fs[,substr(names(d_fs),1,1)!="F"] %>%
  mutate(year=as.integer(substr(Accper,1,4))) %>%
  arrange(Stkcd,desc(year)) %>%
  distinct(Stkcd,year) %>%
  #生成行业代码C仍然用2级，别的行业用1级
  mutate(INDC=as.factor(ifelse(substr(Indcd,1,1)=="C",substr(Indcd,1,2),substr(Indcd,1,1)))) %>%
  #找出国企
  mutate(SOE=substr(S0702b,1,2) %in% c("11","21","22","23")) %>%
  #找出交叉上市公司
  mutate(CROSSLIST=nchar(Crcd)>1) %>%
  #找出非标审计意见
  mutate(AUDIT=Audittyp=="标准无保留意见") %>%
  #只选A股
  filter(Stktype=="A") %>%
  #去掉部分没用的数据
  select(-S0702b,-Crcd,-Audittyp,-B0f1213000,-Indcd,-Stktype)


#remove used data.frame objects--------------

rm(list=ls(pattern="FI_"))
rm(list=ls(pattern="FS_"))
rm(list=ls(pattern="HLD_"))
rm(list=ls(pattern="TRD_"))
rm(list=ls(pattern="FIN_"))
rm(cscode)


#Varibale cleaning------------------------------
p_summary(d_fs,id="Stkcd",t="year")

#contruct readable name data.frame 
d_csm<-select(d_fs,Stkcd,year,SOE,INDC,AUDIT)
#总流动资产
d_csm$TCA<-d_fs$A001100000
#现金
d_csm$CASH<-d_fs$A001101000
#总流动负债
d_csm$TCLIAB<-d_fs$A002100000
#折旧
d_csm$DEPRE<-d_fs$D000103000
#总资产
d_csm$TA<-d_fs$A001000000
#总收入
d_csm$INCOME<-d_fs$B001100000
#固定资产
d_csm$PPE<-d_fs$A001212000
#无形资产
d_csm$INTANG<-d_fs$A001218000
#应收票据
d_csm$NOTERCV<-d_fs$A001110000
#应收款
d_csm$RCV<-d_fs$A001111000
#其他应收款
d_csm$ORCV<-d_fs$A001121000
#一年内到期的长期负债
d_csm$LIAB1Y<-d_fs$A002125000
#长期待摊费用
d_csm$LTEXP<-d_fs$A001221000
#存货
d_csm$INVENTORY<-d_fs$A001123000
#应付账款
d_csm$AP<-d_fs$A002108000
#应付税费
d_csm$TAXP<-d_fs$A002113000
#其他流动资产
d_csm$OCA<-d_fs$A001125000
#净利润
d_csm$PROFIT<-d_fs$B002000000
#营业收入
d_csm$OPINCOME<-d_fs$B001100000
#营业总成本
d_csm$OPCOST<-d_fs$B001200000
#营业税金
d_csm$OPTAX<-d_fs$B001207000
#销售费用
d_csm$SALESEXP<-d_fs$B001209000
#管理费用
d_csm$MGMTEXP<-d_fs$B001210000
#财务费用
d_csm$INTEREST<-d_fs$B001211000
#公允价值变动
d_csm$FAIRCH<-d_fs$B001301000



df<-left_join(d_csm,d_trd,by=c("Stkcd","year" = "Trdynt"))


save.image(file="./reduced_ws.RData")
