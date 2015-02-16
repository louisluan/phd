source("~/Documents/phd/thesis/file_util.R")
require(dplyr)
require(stringr)
require(tm)
require(jiebaR)
require(wordcloud)

setwd("~/Documents/data/fs")

#---------MD&A data preparation for manual classification---------------
#read in all MD&A texts
all_mgmt<-f_reader(f_list_mgmt())
#word split
all_mgmt$txt<-f_split(all_mgmt$txt)
#generate a sentiment placeholder and assign with NA
all_mgmt$senti<-NA
#choose random sample from mgmt texts
sample_mgmt<-sample_n(all_mgmt,150)

save(sample_mgmt,file="~/Documents/phd/thesis/mgmt_sample.RData")
write.csv(sample_mgmt,file="~/Documents/phd/thesis/mgmt_sample.csv")


# generate wordclouds for sample data -------------------------------------

fm_wc2png<-function(firmid,year,strVector){
  fname<-f_id2str(firmid)
  yname<-as.character(year)
  flname<-paste(fname,yname,".png",sep = "")
  
  png(file = flname, bg = "white")
  fm_wc(fm_tdm(strVector))
  dev.off()
}
setwd("~/Documents/phd/")

fm_wc2png(sample_mgmt$corp[6],sample_mgmt$year[6],sample_mgmt$txt[1])

