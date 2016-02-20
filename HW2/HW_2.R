#Non-Parametric Homework 2
#author: rgalbo


#problem 1

#load in data
group1 = c(2.9736,0.9448,1.6394,0.0389,1.2958)
group2 = c(0.7681,0.8027,0.2156,0.0740,1.5076)
group3 = c(4.8249,2.2516,1.5609,2.0452,1.0959)

group = rep(1:3, c(5,5,5))
Y = c(group1,group2,group3)

dat = data.frame(Y,group)

#evaluate group means
library(plyr)
ddply(dat,~group,summarise,mean=mean(Y))

library(ggplot2)
#create boxplt to view data
qplot(factor(group), Y, data = dat, geom = "boxplot",fill = factor(group))

#fit model for anova f-test
fit = lm(Y ~ group, dat)
anv = anova(fit)
fval = anv[1,4]
pval = anv[1,5]
anv
#groups are not significantly different

#permutation f-test
n = length(dat$Y)
nsim = 10000
emptyFstatVec = rep(NA,nsim)
for (i in 1:nsim){
  datPermute = dat
  datPermute$Y = dat$Y[sample(1:n,n)]
  lmOut = lm(Y~group,data=datPermute)
  anovaOut = anova(lmOut)
  emptyFstatVec[i] = anovaOut[1,4]
}
#variable cleanup
FstatVec = emptyFstatVec
permPval = sum(fval<FstatVec)/length(FstatVec)
permPval
pval

#histogram with fval
p = qplot(FstatVec, geom="histogram",bins = 50,fill=I("grey"), col=I("blue"),
          xlab = 'F-Values',main = 'F-distribution') 
p + geom_vline(xintercept = fval, col=I("RED"),linetype = 'longdash')

#kruskal wallis test
#create rank colun
dat$ranks = rank(dat$Y)
Rbar = tapply(dat$ranks, dat$group, mean)
n<-tapply(dat$ranks,dat$group,length)
N = length(dat$ranks)
#calculate kwStat
kwStat = 12/(N*(N+1))*sum(n*(Rbar-(N+1)/2)^2)

#permutation test with kwstat
n = tapply(dat$ranks,dat$group,length)
N = length(dat$ranks)
nsim = 10000
kwVec = rep(NA,nsim)
for (i in 1:nsim){
  datPermuteKru = dat
  datPermuteKru$ranks = dat$ranks[sample(1:N,N)]
  Rbar = tapply(datPermuteKru$ranks,datPermuteKru$group,mean)
  kwVec[i] = 12/(N*(N+1))*sum(n*(Rbar-(N+1)/2)^2)
}
pvalKW = sum(kwStat<kwVec)/nsim
#permutation p-value
pvalKW

kruskal.test(Y,group,data=dat)
#right on the edge of significance


p = qplot(kwVec, geom="histogram",bins = 50,fill=I("grey"), col=I("blue"),
          xlab = 'Kruskal Wallis',main = 'Kruskal Wallis Permutation Distribution') 
p + geom_vline(xintercept = kwStat, col=I("RED"),linetype = 'longdash')


#Problem 2
#read data
dat2 = read.csv('http://bit.ly/Zg51IA')
#boxplot
qplot(factor(group), score, data = dat2, geom = "boxplot",fill = factor(group))
#kruskal-wallis
kruskal.test(dat2$score,dat2$group)

#bonferroni cut-off
k<-5
alpha<-0.05
#cut-off
alpha/(k*(k-1)/2)

#with many groups 

#problem 3
#Here is the Data
site1<-c(46,28,46,37,32,41,42,45,38,44)
site2<-c(42,60,32,42,45,58,27,51,42,52)
site3<-c(38,33,26,25,28,28,26,27,27,27)
site4<-c(31,30,27,29,30,25,25,24,27,30)

groups = rep(1:4, c(10,10,10,10))
sites = c(site1,site2,site3,site4)
#create data
dat3 = data.frame(sites,groups)
#boxplot
qplot(factor(groups), sites, data = dat3, geom = "boxplot",fill = factor(groups))
#kruskal wallis
kruskal.test(sites,groups,data=dat3)

#check bonferroni cut-off
k<-4
alpha<-0.05
#cut-off
alpha/(k*(k-1)/2)

