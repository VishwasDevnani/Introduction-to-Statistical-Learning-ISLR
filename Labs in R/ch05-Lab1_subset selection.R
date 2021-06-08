library(ISLR)
library(leaps)

attach(Hitters)

names(Hitters)

# handling missing values #

sum(is.na(Hitters))

Hitters = na.omit(Hitters)

# subset selection using in built library #

subset_reg.hitters = regsubsets(Salary ~., data= Hitters, nvmax=19 )

subset_reg.summary = summary(subset_reg.hitters)

# in summary * means that particular variable is added to the model 

# analysis of the result #

names(subset_reg.summary)

subset_reg.summary$bic  # as per bic we should select model with 6 variables #

subset_reg.summary$rsq # we see there is an steady increase in rsq as we increase no of variables 

subset_reg.summary$cp # we see that going by cp we should select 10 variables 

# now analysis visually with the help of plots 

par(mfrow=c(2,2))

plot(subset_reg.summary$rss ,xlab="Variables ",ylab=" RSS",
     type="l")

plot(subset_reg.summary$adjr2 ,xlab="Variables ",ylab=" adjusted r2",
     type="l")

which.max(subset_reg.summary$adjr2)

points(11,subset_reg.summary$adjr2[11],col="red")

plot(subset_reg.summary$cp ,xlab="Variables ",ylab=" Cp",
     type="l")

which.min(subset_reg.summary$cp)

points(10,subset_reg.summary$cp[10],col="blue")

plot(subset_reg.summary$bic ,xlab="Variables ",ylab=" bic",
     type="l")

which.min(subset_reg.summary$bic)

points(6,subset_reg.summary$bic[6],col="blue")

par(mfrow=c(2,2))

plot(subset_reg.hitters,scale="r2")
plot(subset_reg.hitters,scale="adjr2")
plot(subset_reg.hitters,scale="Cp")
plot(subset_reg.hitters,scale="bic")


# based on bic lets check out coefficients of best model #

coefficients(subset_reg.hitters,6)

# using methods forward selection, backward elimination etc. 

fit.fwd = regsubsets(Salary ~.,data = Hitters, nvmax = 19, method="forward")

summary(fit.fwd)

fit.bwd = regsubsets(Salary ~.,data = Hitters, nvmax = 19, method="backward")

summary(fit.bwd)

# variable selection using validation set approach #

set.seed(1)

train = sample(c(TRUE, FALSE),nrow(Hitters),rep=TRUE)

test = (!train)

train_regsub =regsubsets(Salary ~.,data = Hitters[train,],nvmax=19)

test.mat=model.matrix(Salary ~. ,data=Hitters[test,])


# storing validation errors for each for the 19 best models 

validation_err = rep(NA,19)

for (i in 1:19){
  coefi = coef(train_regsub,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  validation_err[i]=mean((Hitters[test,]$Salary-pred)^2)
}

validation_err

which.min(validation_err)

coef(train_regsub,7)


# defining a predict function that shall be later used to  cross-validation to choose best model

prediction_subset_Selection = function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


k=10

set.seed(1)

folds = sample (1:k,nrow(Hitters),replace = TRUE)

cv.errors = matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))


# writing loop for cross validation predictions #

for (j in 1:k){
  best.fit= regsubsets (Salary~.,data=Hitters [folds !=j,],nvmax =19)
  for (i in 1:19) {
    pred=prediction_subset_Selection(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i] = mean((Hitters[folds==j,]$Salary-pred)^2)
    print(cv.errors[j,i])
  }
}


mean.cv.errors = apply(cv.errors,2,mean)

mean.cv.errors

par(mfrow =c(1,1))
plot(1:19,mean.cv.errors ,type="b")


# selected model by cross validation # 

reg.best = regsubsets(Salary ~.,data= Hitters, nvmax=19)
coef(reg.best,10)
