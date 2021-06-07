library(ISLR)
set.seed(1)


########## Validation set approach #####

train = sample(392,196)

lm.fit = lm(mpg~horsepower,data=Auto,subset=train)

pred = predict(lm.fit,newdata= Auto)

mean((Auto$mpg - pred)[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)

pred2 = predict(lm.fit2,newdata= Auto)

mean((Auto$mpg - pred2)[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)

pred3 = predict(lm.fit3,newdata= Auto)

mean((Auto$mpg - pred3)[-train]^2)


############### LOOCV ################

glm.fit = glm(mpg ~ horsepower, data=Auto)

coef(glm.fit)

lm.fit = lm(mpg ~ horsepower, data=Auto)

coef(lm.fit)


library(boot)

glm.fit = glm(mpg ~ horsepower, data = Auto)

cv.err = cv.glm(Auto, glm.fit)

cv.err$delta

###--polynomial CV #

## 

cv.error = rep(0,5)

for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}


###################### Kfold CrossValidation ######################

set.seed(17)

cv.error.10 = rep(0,5)

for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10)$delta[1]
}

cv.error.10

# we again observe that there is no significant improvement in MSE post order = 2 #

################# Bootstrap ###################

# estimating accuracy of a statistic #

alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)           # estimating the proportion of division of investments # 


set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))   # estimating the proportion of division of investments usin random sampling with replacement # 

boot(Portfolio,alpha.fn,R=1000)  


################### bootstrap to estimate linear regression coefficient ###############

boot.fn = function(data,index){
  return(coef(lm(mpg ~ horsepower, data = data,subset=index)))
}

boot.fn(Auto,1:392)

set.seed(1)

boot.fn(Auto,sample(392,392,replace=T))

boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,R=1000)

coef(lm(mpg ~ horsepower,data=Auto))

boot.fn = function(data,index){
  
  return(coefficients(lm(mpg ~ horsepower+I(horsepower^2),data=data)))

}

set.seed(1)

boot(Auto,boot.fn,R=100)

summary(lm(mpg ~ horsepower+I(horsepower^2),data=Auto))


