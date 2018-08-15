rm(list=ls())

################ Question 1 #####################
sigma <- 2
b1 <- 1
b2 <- 0.5

# (a)

n <- 200
e <- rnorm(n, 0, sigma^2)
x <- rgamma(n, 2)
y <- b1 + (b2 * x) + e

# (b)
logL <- function(b) {

  return (sum((1/sqrt(2*pi*(b[3]^2)))*exp(-(((y-b[1]-(b[2]*x))^2)/(2*(b[3]^2))))))
  
}

initial <- c(0.1, 0.1, 0.1)
mle <- optim(initial, logL, hessian=TRUE)

mle$par
se = sqrt(solve(mle$hessian))

# (c)
W <- cbind(y, x)
B <- 200
bparmat <- NULL

for (b in 1:B) {
  brow <- sample(1:n, n, replace=T)
  bW <- W[brow, ]
  y <- bW[, 1]
  x <- bW[, 2]
  bnls <- optim(initial, logL)
  bparmat <- rbind(bparmat, bnls$par)
}
sd(bparmat[,1])
k = bparmat[,1]
bsenlse <- c(sd(bparmat[, 1]), sd(bparmat[, 2]), sd(bparmat[, 3]))
bsenlse

# (d)
bsenlse
mle$par
summary(lm(y ~ x))$coef




################ Question 2 #####################
rm(list=ls())
# (a)
library(np)
data <- read.csv("y.csv")

bw <- npudistbw(data[, 1])
d1 <- npudist(bw)
plot(d1)

bw <- npudensbw(data[, 1], bwmethod="cv.ls")
d1 <- npudens(bw)
plot(d1)

# (b)
install.packages("gmm")
library(gmm)

X <- data[, 1]
b <- c(0.1, 0.1)

G <- function(b, X) {
  m1 <- X - (b[1] / b[2])
  m2 <- ((b[1] * (b[1] + 1)) / b[2]^2) - (m1^2)
  gmat <- cbind(m1, m2)
  return(gmat)
}

gmm1 <- gmm(G, X, b, type="cue")
gmm1$coefficients
summary(gmm1)

# (c)
n = 100
W <- data
B <- 200
initial <- c(0.1, 0.1)
bparmat <- NULL

for (b in 1:B) {
  brow <- sample(1:n, n, replace=T)
  X <- W[brow, ]
  gmm1 <- gmm(G, X, initial, type="cue")
  bparmat <- rbind(bparmat, gmm1$coefficients)
}

se <- c(sd(bparmat[, 1]), sd(bparmat[, 2]))

bw <- npudistbw(bparmat[, 1])
d1 <- npudist(bw)
plot(d1)

bw <- npudistbw(bparmat[, 2])
d1 <- npudist(bw)
plot(d1)

################ Question 3 #####################

# (a)
rm(list=ls())
data <- read.csv("ipod.csv")
par(mfrow=c(1,3))
# (b)
ols <- lm(PRICE~BIDRS, data = data)
plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="OLS")
points(data$BIDRS,ols$fitted.values)

install.packages("FNN"); library(FNN)

plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="k=4")
knn=knn.reg(data$BIDRS,y=data$PRICE,k=4)
lines(data$BIDRS,knn$pred)


plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="k=2")
knn1=knn.reg(data$BIDRS,y=data$PRICE,k=2)
lines(data$BIDRS,knn1$pred)
plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="k=3")
knn3=knn.reg(data$BIDRS,y=data$PRICE,k=3)
lines(data$BIDRS,knn3$pred)
plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="k=4")
knn6=knn.reg(data$BIDRS,y=data$PRICE,k=4)
lines(data$BIDRS,knn6$pred)
plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="k=5")
knn9=knn.reg(data$BIDRS,y=data$PRICE,k=5)
lines(data$BIDRS,knn9$pred)

install.packages("np"); 
library(np)

plot(PRICE~BIDRS,data=data,pch=20,col="grey",main="NP Reg")
bw=npregbw(PRICE~BIDRS,data=data,bwmethod="cv.ls")
smooth=npreg(bw)
lines(data$BIDRS,predict(smooth))

# (c)



################ Question 4 #####################

data <- read.csv("psidps3.csv")

# (a)
osl <- lm(wagert~.,data = data)

# (b)
install.packages("lars")
library(lars)

y=data$wagert
x=as.matrix(data)[,-5]
lasso<-lars(y=y,x=x,type="lasso",trace=TRUE)
summary(lasso)
lasso
plot(lasso)
coef(lasso)


# (c)

cv.lasso=cv.lars(y=y,x=x,type="lasso",trace=TRUE)
cv.lasso$cv   
cv.lasso$cv.error  

limit=min(cv.lasso$cv)+cv.lasso$cv.error[which.min(cv.lasso$cv)]

jset=which(cv.lasso$cv<limit)

jstar=which.min(cv.lasso$cv[which(cv.lasso$cv<limit)])
t=cv.lasso$index[jstar]

lassobeta=coef(lasso,s=t,mode="fraction")
which(lassobeta>0|lassobeta<0)

lasso.yhat=predict(lasso,newx=x,s=t,mode="fraction")$fit

lassoR2=sum((lasso.yhat-mean(y))^2)/sum((y-mean(y))^2)
lassoR2


