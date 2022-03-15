rm(list=ls())

library(ggplot2)
library(dplyr)
library(car)
options("scipen" = 100 , "digits" = 4)
setwd("D:\\BSE\\BSE Material\\AP\\Problem sets")
factor3 <-read.csv("F-F_Research_Data_Factors.CSV")
#factor5 <-read.csv("C:\\Users\\Pranav\\Desktop\\F-F_Research_Data_5_Factors_2x3.csv")
portfolios<- read.csv("25_Portfolios_5x5.CSV")
portfolios <- portfolios[-1]
pca <- prcomp(portfolios,scale. = T)
#eigen values
variance <- (pca$sdev)^2
loadings<- pca$rotation
scores <- pca$x
rownames(loadings)<-colnames(portfolios)
varPercent <- variance/sum(variance) * 100
barplot(varPercent, xlab='PC', ylab='Percent Variance',
        names.arg=1:length(varPercent), las=1, col='blue',ylim = c(0,100),) 
abline(h=1/ncol(portfolios)*100 , col = 'red')
round(loadings, 2)[ , 1:2]
correlations <- cor(portfolios,scores[,])
corrplot::corrplot(correlations,method ='shade')
cor_FF <- cor(scores[,1],factor3[2:4])
corrplot::corrplot(cor_FF)
corrplot::corrplot(cor(factor3[2:4],portfolios))
#5

Excess_returns <- portfolios - factor3$RF
#E(Rj-rf ) = a + B(factor)
alphas <- NULL
Betas <- NULL
Errors <- NULL
PC1 <- scores[,1]
for (i in 1:ncol(Excess_returns)) {
  regression <- lm(Excess_returns[,i]~PC1)
  alphas <-append(alphas,regression$coefficients[1])
  Betas <-append(Betas,regression$coefficients[2])
  Errors<-append(Errors,regression$residuals)
}

matrix_alpha<-matrix(alphas,ncol = 5 ,nrow = 5)
colnames(matrix_alpha) <- c('M1','M2','M3','M4','M5')
rownames(matrix_alpha) <- c('B1','B2','B3','B4','B5')
matrix_alpha<- t(matrix_alpha)

#E(Rj-rf ) = a + BE(Rm-Rf)
alphas2 <- NULL
Betas2 <- NULL
Errors2 <- NULL

for (i in 1:ncol(Excess_returns)) {
  regression <- lm(Excess_returns[,i]~factor3$Mkt.RF)
  alphas2 <-append(alphas2,regression$coefficients[1])
  Betas2 <-append(Betas2,regression$coefficients[2])
  Errors2<-append(Errors2,regression$residuals)
}

matrix_alpha2<-matrix(alphas2,ncol = 5 ,nrow = 5)
colnames(matrix_alpha2) <- c('M1','M2','M3','M4','M5')
rownames(matrix_alpha2) <- c('B1','B2','B3','B4','B5')
matrix_alpha2<- t(matrix_alpha2)

#6
beta_FM <- matrix(data = NA , ncol = ncol(portfolios) , nrow = (nrow(portfolios)-36))
for (t in 37:nrow(portfolios)) {
  for (i in 1:length(portfolios)) {
    begin <- t-36
    end<- t-1
    j <- i
    regression <- lm(Excess_returns[begin:end,i] ~ 
                       factor3$RF[begin:end])
    beta_FM[begin,j] <- regression$coefficients[2]
  }
}
beta_FM <- matrix(beta_FM, ncol = 25)

#2 
t1 <- NULL
t1 <- Excess_returns[37:1131,]
t1 <- as.matrix(t1)
alphasFM <- NULL
Betas_FM <- NULL
Errors2 <- NULL
for (i in 1:nrow(t1)) {
  r1 <- lm(t1[i,]~ beta_FM[i,])
  alphasFM <-append(alphasFM,r1$coefficients[1])
  Betas_FM <- append(Betas_FM,r1$coefficients[2])
  Errors2 <- append(Errors2,r1$residuals)
}
#PC
beta_PC <- matrix(data = NA , ncol = ncol(portfolios) , nrow = (nrow(portfolios)-36))
for (t in 37:nrow(portfolios)) {
  for (i in 1:length(portfolios)) {
    begin <- t-36
    end<- t-1
    j <- i
    regression <- lm(Excess_returns[begin:end,i] ~ 
                       PC1[begin:end])
    beta_PC[begin,j] <- regression$coefficients[2]
  }
}
beta_PC <- matrix(beta_PC, ncol = 25)
#2PC

alphasPC <- NULL
Betas_PC <- NULL
ErrorsPC <- NULL
for (i in 1:nrow(t1)) {
  r2 <- lm(t1[i,]~ beta_PC[i,])
  alphasPC <-append(alphasPC,r2$coefficients[1])
  Betas_PC <- append(Betas_PC,r2$coefficients[2])
  ErrorsPC <- append(ErrorsPC,r2$residuals)
}
FM_se <- summary(r1)$coefficients[,2]
PC_se <- summary(r2)$coefficients[,2]

library(rstatix)

t.test(x = alphasFM) #H0:PC_se =0
t.test(x =alphasPC) #H0: FM_se =0
t.test(x = Betas_PC-Betas_FM) #Ho: Pc_se - FM_se =0
