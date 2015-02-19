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
p_summary(df_fs,id="Stkcd",t="year")

TRD_Week<-rbind(TRD_Week,TRD_Week_1)
TRD_Week<-rbind(TRD_Week,TRD_Week_2)

remove(TRD_Week_1)
remove(TRD_Week_2)
remove(d_fs)

cscode<-read.csv("./cscode.txt",stringsAsFactors=F)
csmar_fs<-df_fs[,cscode$var]

rm(ls(pattern="FI_"))

save.image(file="./reduced_ws.RData")
