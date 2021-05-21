x <- c(1,2,3,5)
print(x)
y <- c(10,0,20,.5)
print(y)
print(length(y))
z=x+y
print(z)
z1 = x-y
print(z1)
print(ls()) # list of all the objects saved so far 
# lets now remove these saved objects so far
rm(list=ls())
{r}
# Matrix and some matrix operations 
m = matrix(data=c(1,2,3,4),nrow=2,ncol=2)
print(m)
# by default matrix is populated by successively filling coulmns
# lets say we want to poulate the matrix by filling rows instead
m2 = matrix(data=c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)
print(m2)
# performing sqrt on all the elements of the matrix 
m3 = sqrt(m2)
print(m3)
{r}
# Random Number generaton
# Lets first start with using rnorm which generates random no.s from a normal distribution with mean = 0 and sd =1 (default values).
x = rnorm(50)
print(x)
y = x+rnorm (50,mean=50,sd=.1)
print(y)
# now lets use cor function to calculate correlation between x and y 
print(cor(x,y))
# as we observe that the rnorm creates different set of no.s in each run 
# what if we want to reproduce our result! we may face a difficulty
# for this purpose
set.seed(1)
x = rnorm(100)
print(x)
y = x+rnorm (100,mean=50,sd=.1)
print(y)
# now lets use cor function to calculate correlation between x and y 
print(cor(x,y))
{r}
# Lets check out some graphics # plot
X = rnorm(100)
Y = rnorm(100)
plot(X,Y,xlab='X axis data',ylab='Y axis Data',main='randomly generated data')
# we can also save the plot in pdf form 
pdf("figure.pdf")
plot(X,Y,col="green")
dev.off()
{r}
# Contours 
x1 = seq(1,10)
print(x1)
x1=seq(-pi,pi,length=50)
y1 = x1
f=outer(x1,y1,function(x1,y1)cos(1)/(1+x1^2))
contour(x1,y1,f)
contour(x1,y1,f,nlevels = 45,add=T)
fa = (f-t(f))/2
contour(x1,y1,fa,nlevels=15)
{r}
# heat map
image(x1,y1,fa)
{r}
# producing a 3d plot
persp(x1,y1,fa)
persp(x1,y1,fa,theta=30)
persp(x1,y1,fa,theta=30,phi=20)
persp(x1,y1,fa,theta=30,phi=70)
persp(x1,y1,fa,theta=30,phi=40)
{r}
# lets now start with data indexing 
# lets create a matrix and then try to access desired elements or subsets of this matrix
A = matrix(1:16,4,4)
# dimension 
print(A)
print(dim(A))
print(A[2,3]) # accessing 2nd row and 3rd column
# lets say we want to access a submatrix instead of an element
print(A[1:3,1:2])
print(A[,1:2])
print(A[-c(1,3),])
{r}
# loading Data
Auto =read.table("Auto.data",header=T,na.string="?")
head(Auto)
print(dim(Auto))
print(names(Auto))
{r}
# boxplot
attach(Auto) # to enable us access columns by their names 
plot(cylinders,mpg)
# lets treat cylinder as a set of qualitative variables are it is a small set of integers
cylinders=as.factor(cylinders)
plot(cylinders,mpg,col='red',varwidth=T,horizontal=T) # higher width generally indicates higher no. of data points 
{r}
# histogram 
hist(mpg,col=2,breaks=25)
{r}
# Scatter plot 
pairs(Auto)
pairs(~ mpg+displacement+horsepower+weight+acceleration,Auto) # in case you want to select only a few of the column
{r}
#Data Summary
print(summary(Auto))
print(summary(cylinders))
