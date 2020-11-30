library(xts)
library(zoo)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyquant)
library(dplyr)
library(tibble)
library(rvest)
library(tidyr)
library(forcats)


stock=SP500$adjusted
#data and log return of stocks and GSPC between 2016-2018
threeyearstock=stock['2016-01-04/2018-12-26']
threeyearGSPC=SP500$index['2016-01-04/2018-12-26']
tclass(threeyearstock)='Date'
tyreturn=diff(log(threeyearstock))[-1]
GSPCreturn=diff(log(threeyearGSPC))[-1]
autoplot.zoo(GSPCreturn)

#asset number N
N=ncol(tyreturn)
#days
D=nrow(tyreturn)

betas=CAPM.beta(Ra=tyreturn,Rb=GSPCreturn,Rf=0)
alphas=CAPM.alpha(Ra=tyreturn,Rb=GSPCreturn,Rf=0)
beta_alpha=cbind(t(betas),t(alphas),t(alphas/betas))
colnames(beta_alpha)=c('beta','alpha','alpha/beta')

#plot the highest alpha top15
# x%>%g() pipe operator: equals to g(x)
b_a=as.data.frame(beta_alpha)
b_a_aplhaD=head(arrange(b_a,desc(alpha)),10)
bin=rownames(b_a_aplhaD)#get the index name
#final version of alpha desc
b_a_alD=data.frame(bin,b_a_aplhaD$beta,b_a_aplhaD$alpha,b_a_aplhaD$`alpha/beta`)
colnames(b_a_alD)=c('name','beta','alpha','alpha/beta')
b_a_alD$name=fct_inorder(b_a_alD$name)
ggplot(b_a_alD,aes(x=name,y=alpha))+geom_bar(stat='identity',fill='darkred',color='black')+labs(x='Stocks',y='Alpha',title='Order by alpha value')

#plot with beta
ggplot(b_a_alD,aes(x=name,y=beta))+geom_bar(stat='identity',fill='darkblue',color='black')+labs(x='Stocks',y='Beta',title='Corresponding Beta ordered by alpha value')

#try to evaluate these stock
data_bestAlpha=xts()
data_bestAlpha=cbind(threeyearstock$AMD,threeyearstock$LW,threeyearstock$NVDA,threeyearstock$ABMD,threeyearstock$NRG,threeyearstock$ALGM,threeyearstock$TTWO,threeyearstock$ISRG,threeyearstock$CSX,threeyearstock$ROL)

data_bestAlpha_ret=diff(log(data_bestAlpha))[-1]
PerformanceAnalytics::Return.cumulative(data_bestAlpha_ret)
#market return
PerformanceAnalytics::Return.cumulative(GSPCreturn)

#random return
set.seed(1234)
data_random=xts()
randomStock=sample(colnames(threeyearstock),replace = FALSE,size = 10)
for (i in 1:length(randomStock)){
  #iterate names i to choose wanted stock
  data_random=cbind(data_random,threeyearstock[,randomStock[i]])
}
randomStock_ret=diff(log(data_random))[-1]
PerformanceAnalytics::Return.cumulative(randomStock_ret)

GSPCret=PerformanceAnalytics::Return.cumulative(GSPCreturn)
randomRet=PerformanceAnalytics::Return.cumulative(randomStock_ret)
bAlphaRet=PerformanceAnalytics::Return.cumulative(data_bestAlpha_ret)

ret=cbind(GSPCret,randomRet,bAlphaRet)
rett=t(ret)
rett=cbind(rownames(rett),rett)
colnames(rett)=c('names','cumulative return')
rett=as.data.frame(rett)
rett$names=fct_inorder(rett$names)
ggplot(rett,aes(x=`names` ,y=`cumulative return`))+geom_bar(stat='identity',fill='darkred',color='black')+labs(x='Stocks',y='return',title='cumulative returns')
# 
# #portfolio
# portfolio_fun=function(dataset){
#   prices <- dataset
#   N <- ncol(prices)
#   return(rep(1/N, N))
# }
# adjusted=data_bestAlpha
# data_bestAlpha1=list(adjusted,index)
# names(data_bestAlpha1)[1]='adjusted'
# names(data_bestAlpha1)[2]='index'

#try clustering
library(factoextra)
sharpe=colMeans(tyreturn)/sqrt(diag(var(tyreturn)))
b_a_s=cbind(t(betas),t(alphas),sharpe)
colnames(b_a_s)=c('beta','alpha','sharpe')
b_a_s=na.omit(b_a_s)
kmeans=kmeans(b_a_s,centers = 4,iter.max = 10,nstart = 1)
fviz_cluster(kmeans,data=b_a_s)
kmeans$cluster

scatterplot3d::scatterplot3d(b_a_s,color=factor(kmeans$cluster),main = 'clustering')

