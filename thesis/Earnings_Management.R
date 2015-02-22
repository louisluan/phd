load("~/CSMAR/rdata//MCS_INDEX.RData")
load("~/CSMAR/rdata/csmar_cleaned.RData")
source("~/Documents/phd/thesis/data_util.R")
#需要先加载plyr然后再加载dplyr，不然会产生一系列未知问题，如lag()函数分组工作不正常
require(plyr)
require(dplyr)

#Earnings Management Data preparation--------------

em<-p_balance(dr,"Stkcd","year",2007,2013) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(TACC_TA=TT_ACCRUAL/lag(TT_ASSET),
         ONE_TA=1/lag(TT_ASSET),
         DREV_TA=(CORE_INCOME-lag(CORE_INCOME))/lag(TT_ASSET),
         PPE_TA=lag(PPE_ORIG)/lag(TT_ASSET),
         REC=RECV+NOTERCV+ORCV,
         REC_TA=(REC-lag(REC))/lag(TT_ASSET),
         IAL_TA=INTANG/lag(TT_ASSET),
         DIF_TA=DREV_TA-REC_TA,
         DIF_WC=RECV-lag(RECV)+INVENTORY-lag(INVENTORY) -
           (ACC_PAYA-lag(ACC_PAYA))-(TAX_PAYA-lag(TAX_PAYA)) +
           OTH_CURASST-lag(OTH_CURASST)
         ) %>%
  winsor_df
  



#Time Series Modified Jones Model(Dechow,1995)--------------------
fm_ts <-TACC_TA ~ ONE_TA + DREV_TA + PPE_TA + 0

js2011 <-group_lm(filter(em,year<2011),by="Stkcd",fm_ts)
js2012 <-group_lm(filter(em,year<2012),by="Stkcd",fm_ts)
js2013 <-group_lm(filter(em,year<2013),by="Stkcd",fm_ts)
js11df <-ldply(js2011$mod,coef)
js12df <-ldply(js2012$mod,coef)
js13df <-ldply(js2013$mod,coef)

js11df<-cbind(js2011$Stkcd,js11df)
js11df$year<-2011
names(js11df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")
js12df<-cbind(js2012$Stkcd,js12df)
js12df$year<-2012
names(js12df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")
js13df<-cbind(js2013$Stkcd,js13df)
js13df$year<-2013
names(js13df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")

jsts <-rbind(js11df,js12df)
jsts <-rbind(jsts,js13df)
jsts <- na2zero_df(jsts)
jsts <- arrange(jsts,Stkcd,year)

JONES <- filter(em,year>=2011) %>%
  left_join(jsts) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(MODI_NDA=b_ONE_TA*ONE_TA+b_DREV_TA*(DREV_TA-REC_TA)+b_PPE_TA*PPE_TA,
               MODI_DA=winsor(TACC_TA-MODI_NDA)
               )

summary(JONES$MODI_DA)

#Cross Sectional Jones Model(Lu,1997)-------


fm_cs <-TACC_TA ~ ONE_TA + DIF_TA + PPE_TA + IAL_TA  + 0
cs <-group_lm(filter(em,year>=2011),by="year",fm_cs)
cs2011<-resid(cs$mod[[1]])
cs2012<-resid(cs$mod[[2]])
cs2013<-resid(cs$mod[[3]])

cs11df<-select(em,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2011) %>%
  mutate(LU_DA=winsor(cs2011))

cs12df<-select(em,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2012) %>%
  mutate(LU_DA=winsor(cs2012))

cs13df<-select(em,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2013) %>%
  mutate(LU_DA=winsor(cs2013))

jscs <-bind_rows(cs11df,cs12df) %>%
  bind_rows(cs13df) %>%
  arrange(Stkcd,desc(year)) %>%
  right_join(JONES) %>%
  group_by(Stkcd)


JONES<-jscs

rm(list=ls(pattern="js"))
rm(list=ls(pattern="cs"))
summary(JONES$LU_DA)

#Dichev and Dechow Model----------------

DD<-select(em,Stkcd,year,DIF_WC,OP_CFLOW) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(F1_CFLOW=lead(OP_CFLOW),
         L_CFLOW=lag(OP_CFLOW),
         F_CFLOW=ifelse(is.na(F1_CFLOW),OP_CFLOW,F1_CFLOW)
         ) %>%
  select(-F1_CFLOW) %>%
  winsor_df

fm_roll <- DIF_WC ~ F_CFLOW + OP_CFLOW + L_CFLOW + 0

dd<-group_roll_lm(DD,fm_roll,by="Stkcd",windows = 3,align = "right")

  

