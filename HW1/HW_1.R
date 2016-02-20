#Non-Parametric
#Homework 1

#problem1
#test scores
testScores <- c(79,74,88,80,80,66,65,86,84,80,78,72,71,74,86,96,77,
                81,76,80,76,75,78,87,87,74,85,84,76,77,76,74,85,74,76,
                77,76,74,81,76)

testScores_med <- median(testScores)
length(testScores)
#a) summarize data
summary(testScores)

#fancy boxplot
library(plotly)
plot_ly(y = testScores, type = "box")

"b) null and alternative hyp @ alpha == 0.05 for single sided binomial test
Ho: median = 70
Ha: median > 70
"
diff <- testScores - 70
table(sign(diff))

#by hand
1 - pbinom(37,40,0.5)

#using binomial function
binom.test(38, 40, alternative = 'greater')

#problem2
dat <- data.frame(score = c(3,4,6,3,1,2,1,1), 
                  treatment = c(rep('T1',4),rep('T2',4)))

scoreMeans <- tapply(dat$scor, dat$treatment, mean)
scoreMeans

obsDiffMeans <- diff(scoreMeans)

#permutation test
#by hand

library(perm)
permTS(dat$score~dat$treatment, alternative='greater',exact=TRUE)

#problem3
siblings<-data.frame(hometown=c(rep("rural",24),rep("urban",17)),
                     siblings=c(3,2,1,1,2,1,3,2,2,2,2,5,1,4,1,1,1,1,6,2,2,
                                2,1,1,1,0,1,1,0,0,1,1,1,8,1,1,1,0,1,1,2))

#Wilcoxon Rank Sum
siblings$rank<-rank(siblings$siblings)
W1<-sum(siblings$rank[siblings$hometown=='rural'])
W1

set.seed(1234)
nsims<-10000
rankSumPerms<-rep(NA,nsims)

for (i in 1:nsims){
  rankSumPhekerms[i]<- sum(sample(1:41,24,replace=FALSE))
}

#pvalue
sum(rankSumPerms>=W1)/nsims

#cant do full permutstion by hand because 42 choose 20 permutations
#checking using permutation test

#problem 4
#create data set w/ two groups of similar values with one that has large outliers 

prob4 <- data.frame(group=c(rep('uno',4),rep('dos',4)),value=c(1:7,45))

wilcox.test(value~group,data = prob4)
t.test(value~group, data=prob4)

"can see that the outlier in the data set threw off the two sample t-test
while the Wilcoxon rank-test still had significance"

#problem 5
#Treatment 1 data
trt1 <- c(21.9,20.2,19.4,20.3,19.6,20.4,18.4,20.1,22.0,18.9)
#Treatment 2 data
trt2 <- c(20.2,13.8,21.8,19.2,19.6,25.5,17.0,17.6,19.5,22.2)

ansari.test(trt1,trt2)
