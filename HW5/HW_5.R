#Non Parametric
#Homework 5 - predicting with trees -> random forest

train = read.csv("hw5/data/smogData_train.csv")
#rename o3 so the model doesnt get pissy
names(train)[2] = 'ozone'

#load test data for prediction test
test = read.csv("HW5/DATA/smogData_test.csv")
names(test)[2] = 'ozone'

#primary exploration of the data
#lets plot some of this smog and see what we see
plot(train)

library(corrplot)
#lot of graphs, but alot of signal here
train_cors = cor(train[,1:10], method = 'kendall')
corrplot(train_cors, method = "number")

#corr to 03: temp, vh, ibh, ibt
#high corr pairs: temp+vh, temp+ibt, ibh+ibt, vh+ibt

#build simple tree model
library(tree)
basic_tree = tree(ozone~temp+vh+ibh+ibt, data=train)

plot(basic_tree)
text(basic_tree)

#see if any other variables are included
full_tree = tree(ozone~.,data=train)

plot(full_tree)
text(full_tree)

mse = mean((predict(full_tree, test) - test$ozone)^2)

#gets mse with standard parameters
#gives a baseline for checking the prediction of random forest

library(randomForest)

rf = randomForest(ozone~., train,ntree=10000)
plot(rf)

rf_mse = mean((predict(rf, test) - test$ozone)^2)
importance(rf)

#build a random forest ensemble?
#don't know if an ensemble of ensemble methods works but lets see

rf1 <- randomForest(ozone~., train,ntree=10000, norm.votes=FALSE)
rf2 <- randomForest(ozone~., train,ntree=10000, norm.votes=FALSE)
rf3 <- randomForest(ozone~., train,ntree=10000, norm.votes=FALSE)
rf.all <- combine(rf1, rf2, rf3)

rf.all_mse = mean((predict(rf.all, test) - test$ozone)^2)
 
#doesn't really provide any increase in MSE, slight decrease

#looking at parameter optimization
#start with nodesize
MSE_rand = rep(NA,30)
for( i in 1:30){
  temp_opt = randomForest(ozone~., train,ntree=1000, nodesize = i)
  MSE_rand[i] = mean((predict(temp_opt, test) - test$ozone)^2)
}

min(MSE_rand)
plot(MSE_rand)
#obviously larger end node has larger MSE, less granular

#now lets look at mtry - Number of variables randomly sampled as candidates at each split default sqrt(p)
MSE_rand = rep(NA,11)
for (i in 1:11){
  temp_opt = randomForest(ozone~., train,ntree=1000, nodesize = 1, mtry = i)
  MSE_rand[i] = mean((predict(temp_opt, test) - test$ozone)^2)
}
plot(MSE_rand)


rf_opt = randomForest(ozone~., train,ntree=1000, nodesize = 1, mtry = 2)
rf_opt_mse = mean((predict(rf_opt, test) - test$ozone)^2)
