rm(list=ls())
#Question 1
#a
setwd("D:/R Work Directory")
data <- read.csv("SAT.csv")

#b
attach(data)
par(mfrow = c(2,2))
hist(GPA, main = "Histogram of GPA")
hist(SAT, main = "Histogram of SAT")
hist(APMath, main = "Histogram of APMath")
hist(APEng, main = "Histogram of APEng")

#c

par(mfrow = c(1,1))
plot(GPA, SAT, xlab = "GPA", ylab = "SAT", main = "GPA vs SAT Scores")
abline(lm(SAT~GPA), col = "red")
lines(lowess(GPA,SAT),col = "blue")

#d
const <- matrix(1, nrow = 65, ncol = 1, byrow = TRUE)
OSL<-cbind2(const,as.matrix(data))
X<-OSL[,c(1,4:7,10:11)]
y<-OSL[,8]
OSLBeta<-(solve(t(X)%*%X))%*%t(X)%*%y
OSLBeta
reg <- lm(GPA ~ SAT + APMath + APEng + ESL + gender + race)
summary(reg)
print(summary(reg))


#Question 2
#a
Player1Payoff = function(p1,p2) {
  switch(p1,
         switch(p2,
                {return(0)},
                {return(-1)},
                {return(1)}
         ), 
         switch(p2,
                {return(1)},
                {return(0)},
                {return(-1)}
         ),
         switch(p2,
                {return(-1)},
                {return(1)},
                {return(0)}
         )
         
  )
}

#b
p1expectedpayoff = function(r1,p1,r2,p2) {
  return((-r1*p2 + r1*(1-r2-p2) + p1*r2 + -p1*(1-r2-p2) + (1-r1-p1)*-r2 + (1-r1-p1)*p2))
}

p1expectedpayoff(0.3,0.4,0.4,0.3)

#c
gameresult = function(p1,p2) {
  switch(p1,
         switch(p2,
                {return(0)},
                {return(2)},
                {return(1)}
         ), 
         switch(p2,
                {return(1)},
                {return(0)},
                {return(2)}
         ),
         switch(p2,
                {return(2)},
                {return(1)},
                {return(0)}
         )
         
  )
}

simulation = function(r1,p1,r2,p2,snum) {
  p1seq = seq(1,snum)
  p2seq = seq(1,snum)
  result = seq(1,snum)
  for (i in 1:snum) {
    x<-runif(1,0,1)
    y<-runif(1,0,1)
    if(x <= r1) {
      p1seq[i] <- 1
    }
    else {
      if(x <= r1+p1 && x > r1) {
        p1seq[i] <- 2
      }
      else {
        p1seq[i] <- 3
      }
    }
    if(y <= r2) {
      p2seq[i] <- 1
    }
    else {
      if(y <= r2+p2 && y > r2) {
        p2seq[i] <- 2
      }
      else {
        p2seq[i] <- 3
      }
    }
  }
  for (i in 1:snum) {
    result[i] <- gameresult(p1seq[i],p2seq[i])
  }
  simulationresult <- as.matrix(data.frame(p1 = p1seq, p2 = p2seq, result = result))
  return(simulationresult)
}

p1expectedpayoff(0.3,0.3,0.4,0.3)
record <- simulation(0.3,0.3,0.4,0.3,5000)
record
player2actions <- matrix(record[,2], nrow = 5000, ncol = 1)
#d

betaGO=function(r1,p1,snum) {
  probabilities <- matrix(c(1,1/3,1/3),nrow = 1,ncol = 3,byrow = TRUE)
  probabilities2 <- matrix(1,nrow = 1,ncol = 3,byrow = TRUE)
  if(snum < 10 || snum == 10) {
    record<-simulation(r1,p1,1/3,1/3,snum)
    return(list(record,probabilities))
  }
  if(snum %% 10 == 0) {
  record<-simulation(r1,p1,1/3,1/3,10)
  for(i in 1:((snum/10)-1)) {
  p2=(sum(record[,1] == 1)/nrow(record))
  r2=(sum(record[,1] == 3)/nrow(record))
  probabilities2[,1]<-probabilities2[,1]+1
  probabilities2[,2]<-r2
  probabilities2[,3]<-p2
  probabilities<-rbind2(probabilities,probabilities2)
  record2<-simulation(r1,p1,r2,p2,10)
  record<-rbind2(record,record2)
  }
  }
  else {
    record<-simulation(r1,p1,1/3,1/3,10)
    if (snum > 20){
    for(i in 1:((snum%/%10)-1)) {
      p2=(sum(record[,1] == 1)/nrow(record))
      r2=(sum(record[,1] == 3)/nrow(record))
      probabilities2[,1]<-probabilities2[,1]+1
      probabilities2[,2]<-r2
      probabilities2[,3]<-p2
      probabilities<-rbind2(probabilities,probabilities2)
      record2<-simulation(r1,p1,r2,p2,10)
      record<-rbind2(record,record2)
    }
    }
    p2=(sum(record[,1] == 1)/nrow(record))
    r2=(sum(record[,1] == 3)/nrow(record))
    probabilities2[,1]<-probabilities2[,1]+1
    probabilities2[,2]<-r2
    probabilities2[,3]<-p2
    probabilities<-rbind2(probabilities,probabilities2)
    record2<-simulation(r1,p1,r2,p2,snum%%10)
    record<-rbind2(record,record2)
  }
  p2=(sum(record[,1] == 1)/nrow(record))
  probabilities2[,1]<-probabilities2[,1]+1
  probabilities2[,1] + 1
  probabilities2[,2]<-r2
  probabilities2[,3]<-p2
  probabilities<-rbind2(probabilities,probabilities2)
  return(list(record,probabilities))
}

runningPayoff = function(record) {
  payoff<-matrix(0,nrow = nrow(records), ncol = 3)
  for(i in 1:nrow(records)) {
    if(records[i,3] == 0) {
      payoff[i,1] = 0
    }
    if(records[i,3] == 1) {
      payoff[i,1] = -1
    }
    if(records[i,3] == 2) {
      payoff[i,1] = 1
    }
  }
  for(i in 1:nrow(records)) {
    if(records[i,3] == 0) {
      payoff[i,1] = i
      payoff[i,2] = 0
      payoff[i,3] = sum(payoff[,2])/payoff[i,1]
    }
    if(records[i,3] == 1) {
      payoff[i,1] = i
      payoff[i,2] = -1
      payoff[i,3] = sum(payoff[,2])/payoff[i,1]
    }
    if(records[i,3] == 2) {
      payoff[i,1] = i
      payoff[i,2] = 1
      payoff[i,3] = sum(payoff[,2])/payoff[i,1]
    }
  }
  return(payoff)
}

results<-betaGO(0.4,0.3,100000)
records<-results[[1]]
probabilities<-results[[2]]
betaGoStrategy<-matrix(c(probabilities[,1]*10,probabilities[,2],probabilities[,3],1-probabilities[,2]-probabilities[,3]),ncol = 4, nrow = nrow(probabilities))
colnames(betaGoStrategy) <- c("Number of Games Played","rock", "paper", "scissor")
payoff<-runningPayoff(records)
par(mfrow = c(2,2))
plot(betaGoStrategy[,1],betaGoStrategy[,2],main = "BetaGo Strategy", xlab = "Number of Games", ylab = "Probability of Playing Rock")
abline(h = 0.3,col = "red")
plot(betaGoStrategy[,1],betaGoStrategy[,3],main = "BetaGo Strategy", xlab = "Number of Games", ylab = "Probability of Playing Paper")
abline(h = 0.4,col = "red")
plot(betaGoStrategy[,1],betaGoStrategy[,4],main = "BetaGo Strategy", xlab = "Number of Games", ylab = "Probability of Playing Scissors")
abline(h = 0.3,col = "red")
plot(payoff[,1],payoff[,3], main = "BetaGo Payoff", xlab = "Number of Games", ylab = "Payoff")
abline(h = 0.01,col = "red")

#Q3

#a
library(MASS)
bivn <- mvrnorm(100, mu = c(0, 0), Sigma = matrix(c(1, .9, .9, 1), 2))

plot(bivn[,1],bivn[,2], xlab = "e", ylab = "u", main = "e VS u")
fig <- lowess(bivn[,1], bivn[,2], f=2/3, iter=3)
lines(fig$x, fig$y, col = "red")


#b
b0 = matrix(1,nrow = 100, ncol = 1)
y0 = matrix(1,nrow = 100, ncol = 1)
z0 = matrix(gamma(5),nrow = 100, ncol = 1)
xi = matrix(0,nrow = 100, ncol = 1)

for (i in 1:100) {
  xi[i] = (z0[i,1]-bivn[i,2])/b0[i,1]
}

xi2 = matrix(0,nrow = 100, ncol = 1)

for (i in 1:100) {
  xi2[i] = (y0[i,1]-bivn[i,1])/b0[i]
}

#c
yi<-lm(y0[,1] ~ b0[,1] + bivn[,1])
summary(yi)

zi<-lm(z0[,1] ~ b0[,1] + bivn[,2])
summary(zi)



#Q4

#a
u(x,y) {
  px = 1
  py = 2
  w = 10
  px*x+py*y <= w
  v = x^2*y^3
}


#b
matrixX = matrix(seq(0.01, 10, 0.01),nrow = 1, ncol = 1000)
matrixY = matrix(seq(0.01, 5, 0.01), nrow = 1, ncol = 500)

bundles = matrix(0, nrow = 1000, ncol = 500)

#c
for (i in 1:1000) {
  for (j in 1:500) {
    if(isWithinBudget(matrixX[i],matrixY[j], 1, 2, 10)) {
      bundles[i,j]<-matrixX[i]^2*matrixY[j]^3
    }
  }
}
p = as.data.frame(bundles)
library(xlsx)
write.xlsx(p, "D:/R Work Directory/Bundles.xlsx")
write.table(bundles, "D:/R Work Directory/bundles.txt", sep = ",")
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(matrixX, matrixY, bundles)


#d
max(bundles)
x = as.matrix(which(bundles == max(bundles),arr.ind = TRUE)[1,1]*0.01)
y = as.matrix(which(bundles == max(bundles),arr.ind = TRUE)[1,2]*0.01)
z = matrix(c(x,y,max(bundles)),nrow = 1, ncol = 3)
colnames(z) <- c("x*", "y*","v")
z

  #e
isWithinBudget = function(x, y, px, py, w) {
  z = px*x+py*y
  if(z <= w) {
    return (TRUE)
  }
  else {
    return (FALSE)
  }
}

utilities = function(u, px, py, w) {
  maxX = w/px
  maxY = w/py
  matrixX = matrix(seq(0.01, maxX, 0.01), nrow = 1, ncol = maxX*100)
  matrixY = matrix(seq(0.01, maxY, 0.01), nrow = 1, ncol = maxY*100)
  bundles = matrix(0, nrow = maxX*100, ncol = maxY*100)
  for (i in 1:maxX*100) {
    for (j in 1:maxY*100) {
      if(isWithinBudget(matrixX[i],matrixY[j], px, py, w)) {
        bundles[i,j]<-u((matrixX[i]),(matrixY[j]))
      }
    }
  }
  x = (which(bundles == max(bundles),arr.ind = TRUE))

  return(matrix(c(x[1]*0.01,x[2]*0.01,max(bundles)),nrow = 1, ncol = 3, byrow = FALSE))
}

maxUtility1 <-utilities(function(x,y) x^2*y^3, 1, 2, 10)
colnames(maxUtility1) <- c("x*", "y*","v")
maxUtility1

#f
maxUtility2 <-utilities(function(x,y) (x*y)^0.5, 5, 4, 100)
colnames(maxUtility2) <- c("x*", "y*","v")
maxUtility2

#g
demand = matrix(0, nrow = 1, ncol = 9)
n = matrix(seq(1,5, 0.5),nrow = 1, ncol = 9)
for (i in 1:9) {
  
  demand[1,i] <- utilities(function(x,y) sqrt(x*y), n[i], 4, 100)[1,3]
}
demand
plot(demand,n, main = "Demand Curve of X", xlab = "Quantity Demanded", ylab = "Price of X")
lines(lowess(demand,n),col = "blue")





