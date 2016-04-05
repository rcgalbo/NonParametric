#Nonparametric
#author: rgalbo
# homewor 4

#1
#load data
mat<-matrix(c(4,5,14,7),ncol=2)
rownames(mat)<-c("Made First","Missed First")
colnames(mat)<-c("Made Second","Missed Second")


#a) using McNemar's 
tst = (abs(mat[1,2]-mat[2,1])-1)**2/(mat[1,2]+mat[2,1])

n<-mat[1,2]+mat[2,1]

#pvalues
2*(1-pbinom(mat[1,2]-1,n,.5))

#using test
mcnemar.test(mat,correct=TRUE)

#can see that there is no significant difference 
#seen from using the McNemar test at alpha = 0.5 level

#2
#load data
rabbits<-c(55,140,91,122,111,185,203,101,
              76,145,95,101,196,45,299,226,
              65,70,196,72,121,171,151,113,
              112,67,276,125,100,81,122,71,
              158,78,162,128,96,79,67,119)

#use bootstrapping to sample from the data
#function boots that takes in data, and number of simulations
#returns the MSE
boots = function(data, nsims){
  #function for preforming bootstrap sampling
  #calc theta hat
  theta_hat = mean(data)
  
  #sample the provided data to get theta instances
  sampler = function(data){
    boot_theta = mean(sample(data,length(data),replace = TRUE))
    return(boot_theta)
  }
  
  #generate bootstrap_thetas
  boot_thetas = replicate(nsims,sampler(data))
  mse_est = mean((boot_thetas - theta_hat )^2)
  
  #bootstrap standard error
  e_hat = sum(boot_thetas)/nsims
  bias = e_hat - theta_hat
  
  #bootstrap var
  var_boot = var(boot_thetas)
  
  #calculate confidence intervals
  xbar = mean(data)
  s = sd(data)
  T_dist =  c(xbar-qt(0.975,n-1)*s/sqrt(n),xbar+qt(0.975,n-1)*s/sqrt(n))
  
  confidence_boot = function(data){
    n = length(data)
    new = sample(data,n,replace = TRUE)
    mu_i = mean(new)
    s_i = sd(new)
    tboot = (mu_i-xbar)/s_i/sqrt(n)
    return(tboot)
  }
  
  tboots = replicate(nsims,confidence_boot(data))
  tq = quantile(tboots,c(0.025,0.975))
  
  
  ##bias corrected confidence intervals lookup
  #bootstrap confidence interval??
  
  #calculate bootstrap interval
  cint = c(xbar-tq[2]*s/sqrt(n),xbar-tq[1]*s/sqrt(n))
  
  results = data.frame(c(mse_est,bias,var_boot,cint),
                       row.names = c('Bootstrap MSE','Bias','Bootstrap Var',
                                     'CI Lower','CI Upper'))
  
  return(results)
}

boots(rabbits, 10000)

#problem 3

#generate data
normdat = rnorm(15,6,36)
#calculate sample mean
mu = mean(normdat)
#get variance
v = var(normdat)

boots(normdat,1000)
