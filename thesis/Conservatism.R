load("~/CSMAR/rdata/csmar_cleaned.RData")
source("~/Documents/phd/thesis/data_util.R")

#Conservatism data preparation-------------
DD <- arrange(dr,Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(EXPN=OP_COST+MGMT_EXP+SALES_EXP+IMP_LOSS,
         COREREV=OP_INCOME+OP_TAX-FAIR_CH,
         EARN_MKT=(CORE_PROFIT+OP_TAX-FAIR_CH)/lag(MKT_CAP),
         LEV=TT_LIAB/lag(MKT_CAP),
         SIZE=log(TT_ASSET),
         E_P=COREREV/lag(MKT_CAP),
         D=as.integer(YRET>=0),
         L_EXPN=lag(EXPN),
         F1_EXPN=lead(EXPN),
         F_EXPN=ifelse(is.na(F1_EXPN),EXPN,F1_EXPN) ) %>%
  select(-F1_EXPN) %>%
  winsor_df(p=0.05) %>%
  ungroup() %>%
  p_balance(id="Stkcd",from=2007,to=2013)
  

#Matching (unconditional) conservatism---------


fm_match <- COREREV ~ L_EXPN + EXPN + F_EXPN + 0
DD1 <- select(DD,Stkcd,year,COREREV,L_EXPN,EXPN,F_EXPN )
DD_match <- group_roll_lm(DD1,by="Stkcd",fm_match,windows = 3,align = "center")
DD2<-group_roll_beta2df(DD1,DD_match)

CS<- mutate(DD2,
             CSMATCH=winsor(COREREV - B_L_EXPN*L_EXPN + B_EXPN*EXPN + B_F_EXPN*F_EXPN) )%>%
  select(Stkcd,year,CSMATCH) %>%
  arrange(Stkcd,year) %>%
  filter(year>=2011) 

#C-Score (conditional) model-------------------------

fm_cs <- E_P ~ D + YRET + YRET:(SIZE + M_B + LEV) +
  I(D*YRET)+I(D*YRET):( SIZE + M_B + LEV)+
  SIZE + M_B + LEV + D:SIZE + D:M_B + D:LEV

DD3<-select(DD,Stkcd,year,YRET,E_P,M_B,COREREV:D) %>%
  filter(year>2007,E_P)


DD_cs <- group_lm(DD3,by="year",fm_cs)


DD4<-group_lm_beta2df(DD3,DD_cs,by="year")

DD5<-mutate(DD4,
            GSCORE=B_YRET + B_YRET_SIZE * SIZE+ B_YRET_M_B * M_B +
              B_YRET_LEV * LEV,
            CSCORE=B_I_D_YRET_ + B_SIZE_I_D_YRET_ * SIZE + 
              B_M_B_I_D_YRET_ * M_B +B_LEV_I_D_YRET_ * LEV) %>%
  select(Stkcd,year,GSCORE,CSCORE) %>%
  arrange(Stkcd,year)
 
CS <- left_join(CS,DD5)


#Core Revenue Volatility (unconditional) Model------------

DD6<-select(DD,Stkcd,year,COREREV)

DD_vol <- group_roll_func(DD6,by="Stkcd",windows = 5,align = "right",func = sd,na.rm=T)
DD_volres <-group_roll_beta2df(DD6,DD_vol,windows = 5)  


CS <- left_join(CS,DD_volres)

names(CS)[7] <- "SDREV"

rm(list=ls(pattern = "DD"))

save(CS,file="~/CSMAR/rdata/Conservatism_Data.RData")



