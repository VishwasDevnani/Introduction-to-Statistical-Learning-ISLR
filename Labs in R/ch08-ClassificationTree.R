library(ISLR)
library(tree)

attach(Carseats)

head(Carseats)

High = ifelse(Sales <= 8,"No","Yes")

head(Carseats)

tree.Carseats =tree(High~.-Sales,Carseats)

summary(tree.Carseats)

plot(tree.Carseats)

text(tree.Carseats,pretty = 0)

tree.Carseats

# lets validate model by train test split #

set.seed(2)

train = sample(1:nrow(Carseats),200)

train_Data = Carseats[train,]

tree.Carseats_train = tree(High~.-Sales,train_Data)

test_data = Carseats[-train,]

test.pred = predict(tree.Carseats_train,test_data,type='class')

table(test.pred,test_data$High)

# now lets perform cross validation to achieve optimum level of tree complexity

set.seed(3)

cv.carseats=cv.tree(tree.Carseats,FUN=prune.misclass)

summary(cv.carseats)                    

cv.carseats

par(mfrow=c(1,2))

plot(cv.carseats$size,cv.carseats$dev,type = 'b')
plot(cv.carseats$k,cv.carseats$dev,type='b')

# we observe that optimal complexity = 12 #

# lets prune the tree now # 

prune.Carseats = prune.tree(tree.Carseats,best=12)

par(mfrow=c(1,1))

plot(prune.Carseats)
text(prune.Carseats,pretty=0)


# validating pruned tree now #

test.pred_pruned=predict(prune.Carseats,test_data,type='class')

table(test.pred_pruned,test_data$High)

# nice 86.5 % test data accuracy #




