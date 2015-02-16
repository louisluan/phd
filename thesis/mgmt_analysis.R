source("~/Documents/phd/thesis/file_util.R")
require(dplyr)
require(stringr)
require(tm)
require(jiebaR)

setwd("~/Documents/data/fs")

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
