library(randomForest)
library(ISLR)

set.seed(1)

train = sample(1:nrow(Boston),nrow(Boston)/2)

bag.Boston=randomForest(medv ~.,data=Boston,subset=train,mtry=13,importance=TRUE)

bag.Boston

test_pred_bag.boston=predict(bag.Boston,newdata=Boston[-train,])

test_mse=mean((test_pred_bag.boston-Boston[-train,]$medv)^2)

test_mse

# random forest implimentation # 

rf.Boston=randomForest(medv ~.,data=Boston,subset=train,importance=TRUE)

rf.Boston

test_pred_rf.boston=predict(rf.Boston,newdata=Boston[-train,])

test_rfmse=mean((test_pred_rf.boston-Boston[-train,]$medv)^2)

test_rfmse

# importance comparision for bagging and rf models # 

importance(rf.Boston)
importance(bag.Boston)

varImpPlot(rf.Boston)
varImpPlot(bag.Boston)

# boosting algorithm #

library(gbm)

set.seed(1)

boost.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian", n.trees = 5000,interaction.depth = 4 )

summary(boost.boston)

par(mfrow=c(1,2))

plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

test_pred_boosting.boston = predict(boost.boston,newdata=Boston[-train,],n.trees = 5000)

test_boostingMSE = mean((test_pred_boosting.boston-Boston[-train,]$medv)^2)

test_boostingMSE 


# using a different shrinkage parameter #

boost1.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian", n.trees = 5000,interaction.depth = 4 ,shrinkage = .2)

summary(boost1.boston)

par(mfrow=c(1,2))

plot(boost1.boston,i="rm")
plot(boost1.boston,i="lstat")

test_pred_boosting1.boston = predict(boost1.boston,newdata=Boston[-train,],n.trees = 5000)

test_boosting1MSE = mean((test_pred_boosting1.boston-Boston[-train,]$medv)^2)

test_boosting1MSE 
