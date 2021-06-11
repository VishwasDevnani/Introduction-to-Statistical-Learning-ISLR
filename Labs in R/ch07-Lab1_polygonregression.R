library(ISLR)

attach(Wage)

head(Wage)

fit = lm(wage ~ poly(age,4) , data = Wage)

summary(fit)

fit_raw = lm(wage ~ poly(age,4, raw=T) , data = Wage)

summary(fit_raw)

fit_rawa = lm(wage ~ age+I(age^2)+I(age^3)+I(age^4) , data = Wage)

summary(fit_rawa)

fit_rawb = lm(wage ~ cbind(age,age^2,age^3,age^4) , data = Wage)

summary(fit_rawb)


# all bottom three solutions are giving coefficients of age ^ x #

# first model is giving coefficient of orthogonal polynomials #

agelims=range(age)
age.grid=seq(from = agelims[1],to=agelims[2])
pred_wage=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(pred_wage$fit+2*pred_wage$se.fit,pred_wage$fit-2*pred_wage$se.fit)

# plotting the prediction on new data and observing its band #

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))

plot(age,wage,xlim=agelims,cex=.5,col='darkgrey')

title("Degree - 4 polynomial", outer = T)

lines(age.grid,pred_wage$fit,lwd=2,col="blue")

matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# using second type of poynomial regression #

pred_wage_raw=predict(fit_raw,newdata=list(age=age.grid),se=TRUE)

max(abs(pred_wage$fit - pred_wage_raw$fit))

# differ models comparision using ANOVA #

fit.1= lm(wage~age ,data=Wage)
fit.2= lm(wage~poly(age ,2) ,data=Wage)
fit.3= lm(wage~poly(age ,3) ,data=Wage)
fit.4= lm(wage~poly(age ,4) ,data=Wage)
fit.5= lm(wage~poly(age ,5) ,data=Wage)

anova(fit.1,fit.2,fit.3,fit.4,fit.5)

coef(summary(fit.5))

##

fit.1 = lm(wage~ education+age,data=Wage)

fit.2 = lm(wage~ education+poly(age,2),data=Wage)

fit.3 = lm(wage~ education+poly(age,3),data=Wage)

anova(fit.1,fit.2 ,fit.3)

##

fit = glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds= predict(fit,newdata=list(age=age.grid),se=T)

# we are getting logit predictions hence we need to tranfrom the preditions to actually get probabilities

pfit=exp(preds$fit)/(1+exp(preds$fit))

se.bands.logit = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

# we could not use "response" function to calculate probabilities as that would result in SEs with negative probabilties

#  lets plot confidence interval #

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))

points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")    #jitter is used to add some amount of noise, here it is used to avoid overcrowding

lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# now using the step function to fit wage with respect to age 

table(cut(age,4))

fit = lm(wage ~ cut(age,5),data= Wage)

summary(fit)

plot(fit)



