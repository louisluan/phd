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





D <- left_join(dm,select(dr,Stkcd,year,AUDIT))
D$AUDIT[is.na(D$AUDIT)]<-F

d <- select(D,Stkcd,year)
d$mgmt_op <- D$sentiment 
d$csr_con <- D$csrscore>4
d$ic_cap <- D$icscore
d$cyber_con <- rowSums(select(D,STRATEGY,CULTURE,HR,PUB,HORNOR)) >= 3
d$ctrl_cap <- rowSums(select(D,FINCORP,BUDGET,ERP,MIS)) >= 2
d$brand_con <- rowSums(select(D,BRAND,LOGO,SUBNAME)) >= 2
d$mkt_inno <- rowSums(select(D,WEIXIN,WEIBO)) >= 1
d$mgmt_dplm <- D$MGMTMASTER/ifelse(D$MGMTNUM==0,999,D$MGMTNUM)
d$mgmt_dplm[d$mgmt_dplm>1]=1
d$mgmt_dplm<-winsor(d$mgmt_dplm,0.05)
d$mgmt_exp <- D$MGMTAVGAGE
d$mgmt_exp<-winsor(d$mgmt_exp,0.05)
d$mgmt_chg <- D$MGMTCH
d$mgmt_ovsea <- D$MGMTSEA/ifelse(D$MGMTNUM==0,999,D$MGMTNUM)
d$mgmt_ovsea[d$mgmt_ovsea>1]=1
d$mgmt_ovsea<-winsor(d$mgmt_ovsea,0.05)
d$audit_unq <- D$AUDIT
pca<-prcomp(select(d,mgmt_op:audit_unq))

o_descriptive(as.data.frame(select(d,mgmt_op:audit_unq)),
              head="MCS Index Components",
              file="./MCS_INDEX_COMPONENTS_DESC.htm"
              )



pca_sum <- summary(pca)

stargazer(as.matrix(pca_sum$importance),type="html",out="MCS_INDEX_PCA.htm",
          align=T,column.sep.width="8pt",digits=2,flip=T)

png(filename="./MCS_INDEX_PCA.png",width=800,height=600,units="px",bg="white")
plot(pca,type="l",main="MCS Index Principal Components Analysis")
dev.off()


MCSIDX <- pca$x[,1]
MCS <- cbind(d,MCSIDX) %>%
  select(Stkcd,year,MCSIDX) %>%
  left_join(dr)

rm(list=ls(pattern="d"))
rm(list=ls(pattern="D"))

save(MCS,file="./MCS_INDEX.RData")

