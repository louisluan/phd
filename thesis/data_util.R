require(dplyr)
require(lazyeval)
require(stargazer)
#dplyr包里的主要函数都有加_的版本，可以接受字符串参数
#比如下面用的filter_, group_by_
#这样就可以接受lazyeval变量，在执行时拼出来参数
#主要可以处理不同面板data.frame里面个体变量id和时间变量t命名不同的问题
#直接将字符串传递到函数里

p_balance<-function(df,id="corp",t="year",from=2011,to=2013) {
  #计算时间变量长度
  ncount<-to-from+1
  #如果时间变量是字符的，改成整数
  df[[t]]<-as.integer(df[[t]])
  #传递给dplyr::filter_的参数，方便执行期动态转换为df实际的变量名
  xt<-list(pt=as.symbol(t),pfrom=from,pto=to)
  #真正用dplyr执行筛选平衡面板的部分
  #先排序，然后根据id分组，选出程序参数里定好的时间范围
  #选出来平衡的数据，然后去掉分组，返回处理好的df，时间变为integer了
  df<-arrange_(df,.dots=list(id,t)) %>%
    group_by_(.dots=list(id)) %>%
    filter_(interp("pt>=pfrom & pt<=pto",.values = xt)) %>%
    filter(n()==ncount) %>%
    ungroup()
  return(df)
}

#描述面板数据的类型
p_summary<-function(df,id="corp",t="year"){
  df[[t]]<-as.integer(df[[t]])
  #输出是否有id和t重复的观测
  n_dup<-duplicated(df[,c(id,t)])
  if(sum(n_dup,na.rm=T)>0) {
    print("There is duplicate record,check id and t variable!")
    print(df[n_dup,])
  }
  
  #用lazyeval来让运行时决定id和t的实际名
  p_max<-interp(quote(max(var)), var = as.name(t))
  p_min<-interp(quote(min(var)), var = as.name(t))
  p_n<-interp(quote(n()))
  #参数列表
  dots<-list(p_min,p_max,p_n)
  #先排序，按照id分组，分别统计最小、最大时间值和每个id观测数量
  #然后再生成一个组合的字符变量pattern 小-大-长度
  #再次根据pattern分组，然后统计计算总数
  p<-arrange_(df,.dots=list(id,t)) %>%
    group_by_(.dots=list(id)) %>%
    summarise_(.dots = setNames(dots, c("min_t","max_t","obs_t"))) %>%
    mutate(pattern=paste(as.character(min_t),as.character(max_t),as.character(obs_t),sep="-")) %>%
    group_by(pattern) %>%
    summarise(style=n())
  
  
  #输出时间分布类型
  print(p)
  barplot(p$style,names.arg=p$pattern)
  #return(p)
}

#相关系数矩阵，下Pearson上Spearman
corr<-function(x){
  p_cor<-cor(x);
  s_cor<-cor(x,method="spearman");
  p_cor[upper.tri(p_cor)==TRUE]<-s_cor[upper.tri(s_cor)==TRUE];
  return(p_cor);
}

#描述性统计输出
o_descriptive<-function(...,head="Descriptive Statistics",file="./desc.htm"){
  stargazer(...,
            type="html",title=head,
            out.header=TRUE,summary.logical=FALSE,
            digits=2,median=TRUE,
            out=file
  )
}

#相关系数矩阵输出
o_corr<-function(...,head="Correlations",file="./cor.htm"){
  stargazer(...,
            type="html",title=head,
            out.header=TRUE,summary.logical=FALSE,
            digits=2,median=TRUE,
            out=file,
            notes="Left:Pearson,Right:Spearman"
  )
}
#回归结果输出
o_reg<-function(...,head="Regression Result",ylab,ctrl,ctlab,file="./reg.htm"){
  stargazer(...,type="html",title=head,
            report="vc*p",digits=3,
            dep.var.labels=vlab,
            model.names=FALSE,model.numbers=TRUE,
            header=FALSE,
            omit=ctrl,omit.labels=ctlab,
            flip=TRUE,out.header=TRUE,out=file)
}
 
#Winsorize函数，缩尾用
winsor<-function(x,p=0.01) {
  if(length(p) != 1 || p < 0 || p > 0.5) {
    stop("bad p-value for winsorization")
  }
  lim <- quantile(x, probs=c(p, 1-p))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  return(x)
}