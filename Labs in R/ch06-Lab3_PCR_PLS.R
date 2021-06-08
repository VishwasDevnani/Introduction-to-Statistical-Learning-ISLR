library(pls)

set.seed(2)

set.seed(1)

library(ISLR)

attach(Hitters)

names(Hitters)                                   #names of columns

sum(is.na(Hitters))                             # count no. of nas

Hitters = na.omit(Hitters)   
pcr.fit = pcr(Salary ~.,data = Hitters, scale = TRUE,
              Validation="CV")

summary(pcr.fit)

# let us check the validation plot to understand #

validationplot(pcr.fit,val.type = "MSEP")

# we see that there is a monotonic decrease in MSEP - mean square error of

# prediction (validation set)

# hence it is suggesting that we should rather use least squares 

# lets now perform pcr on training set and test on test set # 

set.seed(1)

train = sample(1:nrow(Hitters),nrow(Hitters)/2)
test= (-train)

x= model.matrix(Salary ~.,data=Hitters)[,-1]    # preparing data in a manner that is accepted by glmnet on of 
# the utilities of model.matrix is that it expands factors into dummy variables
y=Hitters$Salary
y

pcr.fit1 = pcr(Salary ~. , data= Hitters , subset=train, scale = TRUE,
               validation = "CV")

validationplot(pcr.fit1,val.type = "MSEP")

# we visually observe that MSEP is minimum at M = 5 ( principal components)

pcr.pred = predict(pcr.fit1,x[test,],ncomp = 5)

mean((pcr.pred-y.test)^2)

# we see that it performs better than least squares on 'original variabels'

# lets generate final model using full training data #

pcr.fit2 = pcr(y~x,scale=TRUE, ncomp=5)

summary(pcr.fit2)


########### PLS #############

# PCR only
# attempts to maximize the amount of variance explained in the predictors,
# while PLS searches for directions that explain variance in both the predictors
# and the response.

# above is quoted from ISLR key difference between PCR and PLS #

set.seed(1)

pls.fit = plsr(Salary ~.,data = Hitters, subset = train, scale = TRUE,
              Validation="CV")

summary(pls.fit)

validationplot(pls.fit,val.type = 'MSEP')

# we  see there is a monotonic decrease in msep

pls.fit = plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,Validation="CV")

summary(pls.fit)

validationplot(pls.fit,val.type = 'MSEP')

# even in this case we see that there is a monotic decrease in MSEP #

