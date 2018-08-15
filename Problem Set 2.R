rm(list=ls())
#Q1

Stock_Price <- read.csv("D:/R Work Directory/Homework Data/stock_price.csv", header = TRUE, sep = ",")

#a
logPE = data.frame(log(Stock_Price$peratio))
logPE<-setNames(logPE,"logPE")
Stock_Prices = cbind2(Stock_Price,logPE)
write.csv(summary(Stock_Prices), "Stock Price.csv")
stargazer(Stock_Prices, type = "text")

#b
m1 <- lm(peratio~div, data = Stock_Prices)
m2 <- lm(peratio~div+earn, data = Stock_Prices)
m3 <- lm(peratio~div+earn+risk, data = Stock_Prices)
m4 <- lm(logPE~div, data = Stock_Prices)
m5 <- lm(logPE~div+earn, data = Stock_Prices)
m6 <- lm(logPE~div+earn+risk, data = Stock_Prices)

library(stargazer)
stargazer(m1,m2,m3,m4,m5,m6,type="text")

#c
plot(peratio~div, data = Stock_Prices, xlab="div", ylab="Price-Equity Ratio",main="Adjusted R2",pch=20,col="black")
points(Stock_Price$div,m1$fitted.values,pch=8, col = "red")

plot(peratio~div, data = Stock_Prices, xlab="div", ylab="Price-Equity Ratio",main="Adjusted R2",pch=20,col="black")
points(Stock_Price$div,m2$fitted.values,pch=8, col = "red")

plot(peratio~div, data = Stock_Prices, xlab="div", ylab="Price-Equity Ratio",main="Adjusted R2",pch=20,col="black")
points(Stock_Price$div,m3$fitted.values,pch=8, col = "red")

plot(logPE~div, data = Stock_Prices, xlab="div", ylab="log(Price-Equity Ratio)",main="Adjusted R2",pch=20,col="black")
points(Stock_Price$div,m4$fitted.values,pch=8, col = "red")

plot(logPE~div, data = Stock_Prices, xlab="div", ylab="log(Price-Equity Ratio)",main="Adjusted R2",pch=20,col="black")
points(Stock_Price$div,m5$fitted.values,pch=8, col = "red")

plot(logPE~div, data = Stock_Prices, xlab="div", ylab="log(Price-Equity Ratio)",main="Adjusted R2",pch=20,col="grey")
points(Stock_Price$div,m6$fitted.values,pch=8, col = "red")

#d
anova(m1,m3)
anova(m4,m6)


#Q2
cigarette <- read.csv("D:/R Work Directory/Homework Data/cigarette.csv", header = TRUE, sep = ",")


diff_c=cigarette$C2006-cigarette$C2000

m1 <- lm(diff_c~TAX, data = cigarette)

plot(m1)

library(stargazer)
stargazer(m1,type="text")

#Tax had a negetive effect on consumption, the effect was however, not significant.

#Q3

hospital <- read.csv("D:/R Work Directory/Homework Data/hospital_choice.csv", header = TRUE, sep = ",")

m1 <- lm(Ducla~income, data = hospital)
m2 <- lm(Ducla~income+abs(distance), data = hospital)
m3 <- lm(Ducla~income+abs(distance)+old, data = hospital)

library(stargazer)
stargazer(m1,m2,m3,type="text")

anova(m1,m2)
anova(m2,m3)

#m2 seems to be the best model


#Q4

drug <- read.csv("D:/R Work Directory/Homework Data/drug_price.csv", header = TRUE, sep = ",")

logP=log(drug$p.r)

m1 <- lm(p.r~GDP.r, data = drug)
m2 <- lm(p.r~GDP.r+cv.r, data = drug)
m3 <- lm(p.r~GDP.r+cv.r+p.comp, data = drug)
m4 <- lm(p.r~GDP.r+cv.r+p.comp+patent, data = drug)
m5 <- lm(p.r~GDP.r+cv.r+p.comp+patent+p.control, data = drug)

m6 <- lm(logP~GDP.r, data = drug)
m7 <- lm(logP~GDP.r+cv.r, data = drug)
m8 <- lm(logP~GDP.r+cv.r+p.comp, data = drug)
m9 <- lm(logP~GDP.r+cv.r+p.comp+patent, data = drug)
m10 <- lm(logP~GDP.r+cv.r+p.comp+patent+p.control, data = drug)

library(stargazer)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type="text")

#Q5

women <- read.csv("D:/R Work Directory/Homework Data/women_educ.csv", header = TRUE, sep = ",")

m1 <- lm(wage~educ, data = women[women$inlf==1,])
m2 <- lm(wage~educ+hours, data = women[women$inlf==1,])
m3 <- lm(wage~educ+hours+exper, data = women[women$inlf==1,])
m4 <- lm(wage~educ+hours+exper+unem, data = women[women$inlf==1,])
m5 <- lm(wage~educ+hours+exper+unem+city, data = women[women$inlf==1,])
m6 <- lm(wage~educ+hours+exper+unem+city+expersq, data = women[women$inlf==1,])

m7 <- lm(lwage~educ, data = women[women$inlf==1,])
m8 <- lm(lwage~educ+hours, data = women[women$inlf==1,])
m9 <- lm(lwage~educ+hours+exper, data = women[women$inlf==1,])
m10 <- lm(lwage~educ+hours+exper+unem, data = women[women$inlf==1,])
m11 <- lm(lwage~educ+hours+exper+unem+city, data = women[women$inlf==1,])
m12 <- lm(lwage~educ+hours+exper+unem+city+expersq, data = women[women$inlf==1,])

m1 <- lm(lwage~educ, data = women[women$inlf==1,])
m2 <- lm(lwage~educ+faminc, data = women[women$inlf==1,])
m3 <- lm(lwage~educ+faminc+exper, data = women[women$inlf==1,])
m4 <- lm(lwage~educ+faminc+exper+mtr, data = women[women$inlf==1,])
m5 <- lm(lwage~educ+faminc+exper+mtr+motheduc+fatheduc, data = women[women$inlf==1,])
m6 <- lm(lwage~educ+faminc+exper+mtr+motheduc+fatheduc+city, data = women[women$inlf==1,])


library(stargazer)
stargazer(m1,m2,m3,m4,m5,m6,type="text")
m1 <- lm(lwage~educ+faminc+motheduc+fatheduc, data = women[women$inlf==1,])
stargazer(m1,type="text")

stargazer(m7,m8,m9,m10,m11,m12,type="text")



#Q6

library(plm)
train <- read.csv("D:/R Work Directory/Homework Data/jtrain.csv", header = TRUE, sep = ",")

varlist = "hrsemp"
for (i in varlist){
  trains=train[complete.cases(train[[i]]),]}
ptrains<-pdata.frame(trains,index=c("fcode","year"))

OSL2<-lm(hrsemp~grant+employ+d88+d89+1, data = ptrains)
ppool<-plm(hrsemp~grant+employ+d88+d89+1,data=ptrains,model="pooling")
pfe<-plm(hrsemp~grant+employ+d88+d89+1,data=ptrains,model="within",effect="time")
pre<-plm(hrsemp~grant+employ+d88+d89+1,data=ptrains,model="random")
stargazer(OSL,ppool,pfe,pre,type="text")

phtest(pfe,pre)

OSL<-lm(hrsemp~grant+employ+1, data = ptrains)
ppool<-plm(hrsemp~grant+employ+1,data=ptrains,model="pooling")
pfe<-plm(hrsemp~grant+employ+1,data=ptrains,model="within",effect="time")
pre<-plm(hrsemp~grant+employ+1,data=ptrains,model="random")
stargazer(OSL,ppool,pfe,pre,type="text")

phtest(pfe,pre)

OSL1<-lm(hrsemp~grant+1, data = ptrains)
ppool<-plm(hrsemp~grant+1,data=ptrains,model="pooling")
pfe1<-plm(hrsemp~grant+1,data=ptrains,model="within",effect="individual")
pre<-plm(hrsemp~grant+1,data=ptrains,model="random")
stargazer(OSL,ppool,pfe,pre,type="text")

phtest(pfe,pre)
anova(OSL,OSL2)

######################### Question 7 ##########################
#### (a)
S <- 1000
n <- 100

#### (b)
b1 <- 1
b2 <- 0.1

ts <- function (S, n) {
  result <- lapply(1:S, function (d) {
    x <- rnorm(n=n, mean=2, sd=4)
    e <- rnorm(n=n, mean=0, sd=1)
    y <- b1 + (b2 * x) + e
    
    ols <- coef(summary(lm(y ~ x)))
    ols['x', ]
  })
  
  do.call(rbind, result)
}

df <- ts(S, n)
t <- df[, 't value']
plot(density(t), xlab = "t-test statistic", ylab = "Density", main = "t-test Density")

#### (c)
qs <- qt(c(.025, .975), df=n-1)
qs
#### (d)
reject <- c(t[t < qs[0]], t[t > qs[1]])
length(reject)

power.test <- function (b2, df, n) {
  delta <- mean(df[, 'Estimate']) - b2
  se <- mean(df[, 'Std. Error'])
  alpha <- 0.05
  power.t.test(n=n, delta=delta, sd=se, sig.level=alpha, type='one.sample')
}

power.test(b2, df, n)

#### (e)
n <- 1000
df <- ts(S, n)
power.test(b2, df, n)






