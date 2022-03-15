rm(list=ls())

library(ggplot2)
library(dplyr)
library(lmtest)
library(lme4)
library(car)
options("scipen" = 100 , "digits" = 4)

factor3 <-read.csv("C:\\Users\\Pranav\\Desktop\\F-F_Research_Data_Factors.CSV")
factor5 <-read.csv("C:\\Users\\Pranav\\Desktop\\F-F_Research_Data_5_Factors_2x3.csv")
portfolios<- read.csv("C:\\Users\\Pranav\\Desktop\\25_Portfolios_5x5.CSV")
portfolios <- portfolios[-1]
mkt <- factor3$Mkt.RF+factor3$RF
Excess_returns <- portfolios - factor3$RF
   mean_returns <- colMeans(Excess_returns)
mean_returns <- matrix(mean_returns , nrow = 25, ncol = 1)
colnames(mean_returns) <- 'Excess_ret_means'
rownames(mean_returns) <- colnames(portfolios)

Matrix_returns <- t(matrix(mean_returns , 5))
rownames(Matrix_returns) <- c('M1','M2','M3','M4','M5')
colnames(Matrix_returns) <- c('B1','B2','B3','B4','B5')
#2
#E(Rj-rf ) = a + BE(Rm-Rf)
alphas <- NULL
Betas <- NULL
Errors <- NULL

for (i in 1:ncol(Excess_returns)) {
   regression <- lm(Excess_returns[,i]~factor3$Mkt.RF)
   alphas <-append(alphas,regression$coefficients[1])
   Betas <-append(Betas,regression$coefficients[2])
   Errors<-append(Errors,regression$residuals)
}

matrix_alpha<-matrix(alphas,ncol = 5 ,nrow = 5)
colnames(matrix_alpha) <- c('M1','M2','M3','M4','M5')
rownames(matrix_alpha) <- c('B1','B2','B3','B4','B5')
matrix_alpha<- t(matrix_alpha)

GRS1 <- GRS.test::GRS.test(Excess_returns,factor3$Mkt.RF)$GRS.stat
GRS1

#3 

r0 <- lm(mean_returns~ Betas)
myH0 <- c("(Intercept)=0")
linearHypothesis(r0,myH0)
mean(factor3$Mkt.RF)
myH1 <-  c('Betas = 0.5285')
linearHypothesis(r0, myH1)
OLS_se <- summary(r0)$coefficients[,2]

#as P value of F stat is small we reject the null 
#### Question 4. ####
#Using Joint Hypothesis testing method 
#install.packages("GRS.test")
#4 
#first step

beta_FM <- matrix(data = NA , ncol = ncol(portfolios) , nrow = (nrow(portfolios)-36))
for (t in 37:nrow(portfolios)) {
   for (i in 1:length(portfolios)) {
      begin <- t-36
      end<- t-1
      j <- i
      regression <- lm(portfolios[begin:end,i]-factor3$RF[begin:end] ~ 
                          factor3$Mkt.RF[begin:end])
      beta_FM[begin,j] <- regression$coefficients[2]
   }
}
beta_FM <- matrix(beta_FM,nrow = 624, ncol = 25)

#2step

t1 <- NULL
t1 <- Excess_returns[37:660,]
t1 <- as.matrix(t1)
alphas2 <- NULL
Betas2 <- NULL
Errors2 <- NULL
for (i in 1:nrow(t1)) {
   r1 <- lm(t1[i,]~ beta_FM[i,])
   alphas2 <-append(alphas2,r1$coefficients[1])
   Betas2 <- append(Betas2,r1$coefficients[2])
   Errors2 <- append(Errors2,r1$residuals)
}

summary(r1)

#coeff and se 

FM_se <- summary(r1)$coefficients[,2]


#Comparing Standard Errors

SE <- cbind(OLS_se,FM_se)

#additional shit 

estimator0 <- sum(alphas2)/nrow(t1)
estimator1 <- sum(Betas2)/nrow(t1)

#inference 

inference <- estimator0/sqrt(var(alphas2))
inference2 <- estimator1/sqrt(var(Betas2))

#CAMP

mean(factor3$Mkt.RF)
r2 <- lm(rowMeans(t1)~ Betas2)
summary(r2)
myH0 <- c("(Intercept)=0")
linearHypothesis(r2,myH0)
myH0 <- c("Betas2=0.5285")
linearHypothesis(r2,myH0)

#5)

Mktdata1 <- as.matrix(factor3[2:4])
Mktdata2 <-  as.matrix(factor5[2:6])
alphas_fm3 <- NULL
Betas_fm3 <- NULL
Errors_fm3 <- NULL
for (i in 1:ncol(Excess_returns)) {
   regression <- lm(Excess_returns[,i]~Mktdata1)
   alphas_fm3 <-append(alphas_fm3,regression$coefficients[1])
   Betas_fm3 <-append(Betas_fm3,regression$coefficients[2])
   Errors_fm3<-append(Errors_fm3,regression$residuals)
}

matrix_alphas_fm3<-matrix(alphas_fm3,ncol = 5 ,nrow = 5)
colnames(matrix_alphas_fm3) <- c('M1','M2','M3','M4','M5')
rownames(matrix_alphas_fm3) <- c('B1','B2','B3','B4','B5')
matrix_alphas_fm3<- t(matrix_alphas_fm3)
GRS2 <- GRS.test::GRS.test(Excess_returns,Mktdata1)
GRS2$GRS.stat




alphas_fm5 <- NULL
Betas_fm5 <- NULL
Errors_fm5 <- NULL
for (i in 1:ncol(Excess_returns)) {
   r3 <- lm(Excess_returns[,i]~Mktdata2)
   alphas_fm5 <-append(alphas_fm5,r3$coefficients[1])
   Betas_fm5 <-append(Betas_fm5,r3$coefficients[2])
   Errors_fm5<-append(Errors_fm5,r3$residuals)
}
matrix_alphas_fm5<-matrix(alphas_fm5,ncol = 5 ,nrow = 5)
colnames(matrix_alphas_fm5) <- c('M1','M2','M3','M4','M5')
rownames(matrix_alphas_fm5) <- c('B1','B2','B3','B4','B5')
matrix_alphas_fm5<- t(matrix_alphas_fm5)
GRS3 <- GRS.test::GRS.test(Excess_returns,Mktdata2)
GRS3$GRS.stat  
