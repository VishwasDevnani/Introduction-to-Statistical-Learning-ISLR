library(splines)

library(ISLR)

attach(Wage)

head(Wage)

# gam with natural spline #

gam1 = lm( wage ~ ns(year,4)+ns(age,5)+education, data= Wage)

library(gam)

# gam with smooth spline #

gam.m3= gam(wage ~ s(year,4)+s(age,5)+education, data= Wage)

par(mfrow=c(1,3))

plot(gam.m3,se=T,col="blue")

plot.Gam(gam1 , se=TRUE , col ="red ")  # caution Gam and not gam

# lets try two more different models and compare models using anova #

gam.m1=gam(wage ~ s(age,5)+education, data= Wage)   # without year 
gam.m2=gam(wage ~ year + s(age,5)+education, data= Wage) # with linear year function 
anova(gam.m1,gam.m2,gam.m3)

# we see that we should choose linear model over model independent of year and  quadratic function of year

plot(gam.m2,se=T,col="blue")

# lets see summaries now of these models #

summary(gam.m2)

# prediction can also be made using the generic predict function #

pred_gam=predict(gam.m2,newdata = Wage)

# now utilizing local regression method #

fit.m4=gam(wage ~ s(year,df=4) + lo(age,span=.7)+education, data= Wage)

# with interaction effect now # 

fit.m5=gam(wage ~ lo(year,age,span=.5)+education, data= Wage)

library(akima)

plot(fit.m5)



############ logistic regression GAM ##################

gam.lr = gam(I(wage>250) ~ year + s(age,df=5)+education,data = Wage,family = binomial())

par(mfrow=c(1,3))

plot.Gam(gam.lr,se=T,col="red")

table(education,I(wage>250))


gam.lr2 = gam(I(wage>250) ~ year + s(age,df=5)+education,data = Wage,family = binomial(),subset=(education!="1. < HS Grad"))

par(mfrow=c(1,3))

plot.Gam(gam.lr2,se=T,col="blue")

