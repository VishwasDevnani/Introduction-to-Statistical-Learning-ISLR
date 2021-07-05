library(MASS)

set.seed(1)

train_boston = sample(1:nrow(Boston),nrow(Boston)/2)

train_data_boston = Boston[train_boston,]

test_data_boston =Boston[-train_boston,]

tree.boston = tree(medv ~., train_data_boston)

summary(tree.boston)

plot(tree.boston)
text(tree.boston)

# lets get the best tree complexity level with the help of cross validation #

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# choosing the best tree complexity = 7 to make predictiona and see the test deviance # 

prune.tree = prune.tree(tree.boston,best=7)

test_boston_pred = predict(prune.tree, test_data_boston)

dev_test = mean((test_boston_pred-test_data_boston$medv)^2)

dev_test
