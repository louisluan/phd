setwd("~/Documents/phd/thesis/")
source("data_util.R")
source("file_util.R")
setwd("~/CSMAR/")
q<-f_list_file()

f_readtable_list(q)

d_fs<- left_join(FS_Combas,FS_Comins) %>%
  left_join(FS_Comscfd) %>%
  left_join(FS_Comscfi) %>%
  filter(substr(Accper,6,10)=="12-31") %>%
  left_join(FI_T1) %>%
  left_join(FI_T2) %>%
  left_join(FI_T3) %>%
  left_join(FI_T4) %>%
  left_join(FI_T5) %>%
  left_join(FI_T6) %>%
  left_join(FI_T7) %>%
  left_join(FI_T8) %>%
  left_join(FI_T9) %>%
  left_join(FI_T10) %>%
  left_join(FIN_Audit) %>%
  left_join(HLD_Contrshr) %>%
  left_join(HLD_Copro) 

d_fs<-arrange(d_fs,Stkcd,Accper) %>%
  mutate(year=as.integer(as.factor(Accper)))

df_fs<-d_fs[!duplicated(d_fs[,c("Stkcd","year")]),]  


TRD_Week<-rbind(TRD_Week,TRD_Week_1)
TRD_Week<-rbind(TRD_Week,TRD_Week_2)

remove(TRD_Week_1)
remove(TRD_Week_2)
remove(d_fs)

cscode<-read.csv("./cscode.txt",stringsAsFactors=F)
csmar_fs<-df_fs[,names(df_fs) %in% cscode$var]

rm(list=ls(pattern="FI_"))
rm(list=ls(pattern="FS_"))
rm(list=ls(pattern="HLD_"))

csmar_fs[is.na(csmar_fs)]<-0
csmar_fs$SOE<-substr(csmar_fs$S0702b,1,2) %in% c("11","21","22","23")
csmar_fs$CROSSLIST<-nchar(csmar_fs$Crcd)>3
csmar_fs$AUDIT<-csmar_fs$Audittyp=="标准无保留意见"


d_fs<-csmar_fs[,substr(names(csmar_fs),1,1)!="F"] %>%
  arrange(Stkcd,desc(Accper)) %>%
  mutate(year=(as.ordered(Accper))) %>%
  mutate(INDC=as.factor(ifelse(substr(Indcd,1,1)=="C",substr(Indcd,1,2),substr(Indcd,1,1)))) %>%
  filter(Stktype=="A") %>%
  select(-S0702b,-Crcd,-Audittyp,-B0f1213000,-Indcd)
  
rm(cscode)
rm(csmar_fs)
rm(df_fs)

p_summary(d_fs,id="Stkcd",t="year")


save.image(file="./reduced_ws.RData")
