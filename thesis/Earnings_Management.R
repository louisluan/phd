load("~/CSMAR/reduced_ws.RData")
source("data_util.R")

#Earnings Management Data preparation--------------





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




#joining with mcs index data

em$year<-extractyear(as.character(em$year))
d_overall<-left_join(d_nall,em)

