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
eosinophil<-c(55,140,91,122,111,185,203,101,
              76,145,95,101,196,45,299,226,
              65,70,196,72,121,171,151,113,
              112,67,276,125,100,81,122,71,
              158,78,162,128,96,79,67,119)

#use bootstrapping to sample from the data
nsims = 1000
