require(dplyr)
require(stringr)
require(tm)
require(jiebaR)
#universal jiebaR cutter for all text splitting purpose
jcutter<-worker(user ="/Users//Luis//Documents//data//finance.txt")

#----------------helper----------------------
#function to extract firm year info
f_firm_year<-function(str){
  tmp_firm<-as.integer(substr(str,1,6))
  tmp_year<-as.integer(substr(str,8,11))
  tmp<-cbind(tmp_firm,tmp_year)
  return(tmp)
}

#funtion to align integer firm-id to string
f_id2str<-function(firmid){
  tmp<-formatC(firmid,width = 6,flag = "0")
  return(tmp)
}

#fucntion to find if there is any word match, keywords is a char vector
f_hasword<-function(keywords,strVector) {
  nk<-length(keywords)
  ns<-length(strVector)
  ncount<-rep(0,nk*ns)
  ncount<-matrix(ncount,nrow = ns,ncol = nk)
  colnames(ncount)<-keywords
  for(i in 1:nk) {
    tmp<-grep(keywords[i],strVector)
    ncount[tmp,i]<-1
  } 
  return(ncount)
}

#word cutter wrapper
jcut <- function(str="",cutter) {
  
  ftmp <- cutter<=str
  ftmp<-paste(ftmp,collapse = " ")
  return(ftmp)
}

#fucntion to do word split
f_split<-function(str) {
  
  chars <-sapply(str,jcut,cutter=jcutter,USE.NAMES = F)
  return(chars)

}


#---------------------enum file names-----------------------
#funtion wrapper for enumerate MD&A file names
f_list_mgmt<-function(f_path = "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*A\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate internal control file names
f_list_ic<-function(f_path= "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*B\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate CSR file names
f_list_csr<-function(f_path= "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*C\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate strategy file names
f_list_strategy<-function(f_path= "."){
  tmp1 <-list.files(path=f_path,recursive = T,pattern = "*D\\d{0}\\.txt$")
  tmp2 <-list.files(path=f_path,recursive = T,pattern = "*D\\d{4}\\.txt$")
  f_m<-t(sapply(tmp2,f_firm_year,simplify = T))
  t_tb<-tbl_df(as.data.frame(f_m))
    colnames(t_tb)<-c("firm","year")

  t_uniq<-t_tb[!duplicated(t_tb[c("firm")]),]
  t_uniq<-t_uniq[(!is.na(t_uniq$firm)),]
  t_uniq<-t_uniq[(!is.na(t_uniq$year)),]
  names<-paste(f_id2str(t_uniq$firm),"D",as.character(t_uniq$year),".txt",sep="")
  names<-c(tmp1,names)
  return(names) 
}

#funtion wrapper for enumerate culture file names
f_list_culture<-function(f_path= "."){
  tmp1 <-list.files(path=f_path,recursive = T,pattern = "*E\\d{0}\\.txt$")
  tmp2 <-list.files(path=f_path,recursive = T,pattern = "*E\\d{4}\\.txt$")
  f_m<-t(sapply(tmp2,f_firm_year,simplify = T))
  t_tb<-tbl_df(as.data.frame(f_m))
  colnames(t_tb)<-c("firm","year")
  
  t_uniq<-t_tb[!duplicated(t_tb[c("firm")]),]
  t_uniq<-t_uniq[(!is.na(t_uniq$firm)),]
  t_uniq<-t_uniq[(!is.na(t_uniq$year)),]
  names<-paste(f_id2str(t_uniq$firm),"E",as.character(t_uniq$year),".txt",sep="")
  names<-c(tmp1,names)
  return(names) 
}

#-------------scan in text files------------------

f_reader<-function(files) {
  #初始化一个与文本数量等长的空list
  n_fl<-length(files)
  lst<-list()
  length(lst)<-n_fl 
  #用一个df容纳所有的公司和年度信息
  corp<-sapply(files,substr,1,6,USE.NAMES = F)
  year<-sapply(files,substr,8,11,USE.NAMES = F)
  df_fs<-data_frame(corp,year)      
  #批量读入文本
  for(i in 1:n_fl){
    tmp<-scan(file=files[i],what=character(),encoding = "UTF-8",skipNul=T) %>%
      paste(collapse = " ") 
    lst[i]<-tmp
  }
  
  df_fs$txt<-unlist(lst)
  df_fs<- mutate(df_fs,tlen=nchar(txt))
  
  return(tbl_df(df_fs))
}


