
library(splines)

library(ISLR)

attach(Wage)

head(Wage)


fit = lm (wage ~ bs(age,knots = c(25,40,60)), data = Wage )


agelims=range(age)
age.grid=seq(from = agelims[1],to=agelims[2])


# prediction on ag.grid #

pred_bs = predict(fit, newdata = list(age=age.grid),se=T)

# plotting prediction and confidence interval #

plot(age.grid , pred_bs$fit,lwd=2)
lines(age.grid, pred_bs$fit+2*pred_bs$se.fit,lty="dashed")
lines(age.grid, pred_bs$fit-2*pred_bs$se.fit,lty="dashed")


# fitting model by just specifying number of degree of freedoms instead of defining knot points#

# lets see how to do that # 

dim(bs(age,knots = c(25,40,60)))

dim(bs(age,df=6))

attr(bs(age,df=6),"knots")

# we can also fit a natural spline instead # 

fit2 = lm(wage~ns(age,df=4), data=Wage)

pred_ns = predict(fit2, newdata = list(age=age.grid),se=T)

# plotting prediction and confidence interval #

plot(age.grid , pred_ns$fit,lwd=2)
lines(age.grid, pred_ns$fit+2*pred_bs$se.fit,lty="dashed")
lines(age.grid, pred_ns$fit-2*pred_bs$se.fit,lty="dashed")

# lets now fit a smooth spline #

# first fit a model with df=16 #

fit_s= smooth.spline(age,wage,df=16)
plot(age,wage,xlim = agelims,cex=.5,col="darkgrey")
lines(fit_s,col="red",lwd=2)

# lets now choose df based on cross validation #

fit_s_cv=smooth.spline(age,wage,cv=T)
pred_s2= predict(fit_s,newdata=Wage)
#plot(age,wage,xlim = agelims,cex=.5,col="darkgrey")
lines(fit_s_cv,col="blue",lwd=2)

legend("topleft",legend=c("16 DF","6.8 DF"),col = c("red","blue"),lty=1,lwd=2,cex=.8)

# Now its time for local regression fit which basically uses information from its neighbourhood points #

fit_s= loess(wage ~ age , span=.2 , data = Wage)
plot(age,wage,xlim = agelims,cex=.5,col="darkgrey")
lines(age.grid, predict(fit_s , newdata=data.frame(age=age.grid)) ,col="red",lwd=2)

# lets now choose df based on cross validation #

fit_s2= loess(wage ~ age , span=.5 , data = Wage)
lines(age.grid, predict(fit_s2 , newdata=data.frame(age=age.grid)) ,col="blue",lwd=2)

legend("topleft",legend=c(".2 span",".5 span"),col = c("red","blue"),lty=1,lwd=2,cex=.8)


