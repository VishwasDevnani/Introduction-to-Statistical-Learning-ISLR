set.seed(1)

library(ISLR)

library(leaps)

attach(Hitters)

names(Hitters)                                   #names of columns

sum(is.na(Hitters))                             # count no. of nas

Hitters = na.omit(Hitters)                      # removing NA items from Hitters


x= model.matrix(Salary ~.,data=Hitters)[,-1]    # preparing data in a manner that is accepted by glmnet on of 
# the utilities of model.matrix is that it expands factors into dummy variables
y=Hitters$Salary
y

library(glmnet)

grid = 10^seq(10,-2,length = 100)  # very high to very low positive values 

ridge.mod = glmnet(x,y,alpha=0,lambda=grid) # 

dim(coef(ridge.mod))

# below is the demo to show how coefficent vary as you decrease gamma tuning paramter for ridge regression#

ridge.mod$lambda[10]

coef(ridge.mod)[,10]

ridge.mod$lambda[50]

coef(ridge.mod)[,50]

ridge.mod$lambda[70]

coef(ridge.mod)[,70]


# predict function here can be utilised to calculate coefficient in somewhere middle #

predict(ridge.mod,s=75,type='coefficients')[1:20,]

set.seed(1)

# splitting in train and test # 

train = sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]


# ride regression fitting #


ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12) # this fits for all the values in the grid


# description of threshold parameter is related to coordinate descent algorithm 

# Convergence threshold for coordinate descent. Each inner coordinate-descent loop 
# continues until the maximum change in the objective after any coefficient update 
# is less than thresh times the null deviance. Defaults value is 1E-7.

ridge.pred = predict(ridge.mod,s=4,newx=x[test,])

mean((ridge.pred-y.test)^2)

# test mse in case we just use mean train target data to predict the case #

mean((mean(y[train])-y.test)^2)

# same result shall be replicated if we use very high tuning parameter #

ridge.pred1=predict(ridge.mod,s=1e10,newx=x[test,])

mean((ridge.pred1-y.test)^2)

# we observe exact match #

# futher lets move to checking benefit at lambda =4 in comparision with lambda =0 which is simple least squares #

ridge.pred2 = predict(ridge.mod, s=0, newx=x[test,])

mean((ridge.pred2-y.test)^2)

# we see that it is indeed producing better result than least squares with less MSE #

# let's now use cross validation to choose the parameter instead of random testing different values #

set.seed(1)

cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

bestlam = cv.out$lambda.min

bestlam

# now lets check our best lambda. how it performs on test #

ridge.pred3 = predict(ridge.mod,s=bestlam,newx=x[test,])

mean((ridge.pred3-y.test)^2)

# we observe there is a futher drop in test MSE #

# lets us now use full training data and calculate the coefficients #

ridge.bestmod = glmnet(x,y,lambda =bestlam,alpha=0)

ridge.bestmod$beta
ridge.bestmod$a0


# lets also try method given in ISLR

ridge.bestmod1 = glmnet(x,y,alpha=0)

predict(ridge.bestmod1,type='coefficients',s=bestlam)[1:20,]

############### THE LASSO #############

#   fitting lasso model # 

lasso.mod = glmnet(x[train,],y[train],alpha=1)

plot(lasso.mod)

# we observe that depending on the value of tuning parameter coefficients
# of many variables are zeros

# let's now choose the best lasso model and compare it to ridge regression # 

set.seed(1)

cv.out = cv.glmnet(x[train,],y[train],alpha=1)

bestlamLasso = cv.out$lambda.min

bestlamLasso

lasso.pred = predict(lasso.mod,s=bestlamLasso,x[test,])

mean((lasso.pred-y.test)^2)

# this is slightly higher than what we have observed for ridge but still better than
# null model and least squares # 

# lets now check out run the model on full train data and check out the coefficients #

out = glmnet(x[train,],y[train],alpha =1)

predict(out,type = 'coefficients', s= bestlamLasso )[1:20,]

# we see that as opposed to ridge regression lasso has performed variable selection 

# meaning that some of the coefficients have converged to zero.