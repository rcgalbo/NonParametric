#Non parametric homework 3

#problem 1
#create hay data
dat = data.frame(kg=c(1.5,2.1,1.9,2.8,1.4,1.8,
                     1.8,2.0,2.0,2.7,1.6,2.3,
                     1.9,2.5,2.5,2.6,2.1,2.4),
                date = factor(rep(c("Sept 1","Sept 15","Sept30"),each=6)),
                block = factor(rep(1:6,3)))

#a)
#randomized block design with permutation
diffs = tapply(dat$kg,datPermute$date,mean)
tst_stat = abs(max(diffs)-min(diffs))

nsims = 1000
maxDiffVec = rep(NA,nsims)

for (i in 1:nsims){
  datPermute<-dat
  #Shuffle WITHIN blocks
  datPermute$kg = unlist(tapply(datPermute$kg,datPermute$block,
                                  function(x){sample(x,length(x),replace=FALSE)}))
  
  #fit the linear model
  diffs = tapply(datPermute$kg,datPermute$block, mean)
  
  #Compute the max absolute difference
  maxDiffVec[i] = abs(max(diffs)-min(diffs))
}
hist(maxDiffVec)
abline(v=tst_stat,col="red",lwd=5)

p_val = sum(maxDiffVec <= tst_stat)/nsims

#b)
friedman.test(dat$kg,dat$date,dat$block)
#not significant, fail to reject


#c)
#randomized block design with anova

f = c("Sept 1","Sept 15","Sept30")
k = 3
n = 6

kg = dat$kg

#matching treatment
tm = gl(k, 1, n*k, factor(f))

#blocking factor
blk = gl(n, k, k*n)

#creat anova model using vectors
av = aov(kg ~ tm + blk)
summary(av)
#since the p-vales are all insignificant we can reject the null hypothesis that the means are the same


#d)
l1 = lm(kg ~ date, data=dat)
anova(l1)

#calculate the means
tapply(dat$kg, dat$date, mean)

#use the aov function as input for tukey
a1 = aov(kg ~ date, data = dat)
#calculate diffs using tukey
posthoc = TukeyHSD(x = a1, which = 'date', conf.level=0.95)
#plot the tukey results
plot(posthoc)

#get test stat for permutation test
max_diff = max(posthoc$date[1:3])

#tukey permutation test with for loop
n = length(dat$kg)
nsim = 1000
tukey_dist = rep(NA,nsim)
for (i in 1:nsim){
  dat_cp = dat
  perm = sample(dat$kg, n, replace = FALSE)
  dat_cp$kg = perm
  tukey_dist[i] = max(abs(TukeyHSD(aov(kg ~ date, data = dat_cp), which = 'date', conf.level=0.95)$date[1:3]))
}

#plot the distribution of max pairwise mean differences
hist(tukey_dist)
abline(v=max_diff,col="red",lwd=5)

#calculate the pvalue for tukey
p_val = sum(tukey_dist>=max_diff)/nsim
#can see that this test is not significant



#tukey_test function
tukeyPerm = function(var, group, nsims = 10000){
  
  #create function for generating test statistics
  max_difference = function(var, group){
    dist_i = max(abs(apply(combn(tapply(var, group, mean),2), 2, diff)))
    return(dist_i)
  }
  
  #test statistic
  tst_stat = max_difference(var,group)
  
  #create a function for permuting the mean
  permute_means = function(var,group){
    new = sample(var,length(var),replace = FALSE)
    return(max_difference(new, group))
  }
  
  #create the distribution
  X = rep(c(var,group),nsim)
  tukey_distribution = aaply(X,permute_means(X))
  
  
  p_val = sum(tukey_distribution > tst_stat)/nsims
  
  return(p_val)
}

tukeyPerm(dat$kg,dat$date,1000)


#2

ozone = read.csv("C:\\Users\\rgalbo\\Documents\\NonParametric\\HW3\\ozone.csv")
ozone


#3




#6
#load data for correlation
age = c(3,7,15,24,85,180,360)
strength = c(2500,3200,4300,5300,5900,6700,6900)

#a)
#plot the non-linear data
plot(age,strength)

#vector of correlation methods
corTypes = c("pearson", "kendall", "spearman")

#vector for storing correlation
corr_coefs = list()

for (method in corTypes){
  corr_coefs[method]=cor(age, strength, method = method)
}

#b)
#conduct all correlation tests
cor.test(age,strength, method = "pearson")
cor.test(age,strength, method = "spearman")
cor.test(age,strength, method = "kendall")

#permutation test for pearson
rho = cor(age,strength)

#compute probability using permutation test
nsims = 1000
rhoVec<-rep(NA,nsim)
  for (i in 1:nsim){
    strengthPerm<-sample(strength,length(strength),replace=FALSE)
    rhoVec[i]<-cor(age,strengthPerm)
  }

#plot histogram and test statistic
hist(rhoVec,xlim=c(-1,1))
abline(v=rho,col="red",lwd=5)

#calc p-val
sum(rhoVec>=abs(rho))/nsim+sum(rhoVec<=-abs(rho))/nsim
#significant at alpha =0.05, reject null hypothesis


