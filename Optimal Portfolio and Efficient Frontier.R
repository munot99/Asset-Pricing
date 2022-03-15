Myfile<-"C:/Users/Pranav/Desktop/17_Industry_Portfolios.CSV"
Mydata<- read.csv(Myfile , skip = 11)
library(dplyr)
library(ggplot2)
Mydata[] <- lapply(Mydata, as.numeric)
df<- as.data.frame(Mydata[,2:ncol(Mydata)])/100
options("scipen"=100, "digits"=4)
#a
matrix <- cov(df)
E_R<-cbind(colMeans(df ,na.rm = TRUE))


std_dev <- diag(matrix)^0.5
M_1 <- cbind(rep(1,length(E_R)))
w1 <- as.matrix((solve(matrix)%*%E_R)/as.numeric(t(M_1)%*%solve(matrix)%*%(E_R)))
w2 <-as.matrix((solve(matrix)%*%M_1)/as.numeric(t(M_1)%*%solve(matrix)%*%M_1))
covariance <- cov(w1,w2)



#b
p<-data.frame(seq(-4,4, by= 0.01))
porta <- (as.matrix(df) %*% w1)
portb <- (as.matrix(df) %*% w2)
mean_porta <- mean(as.matrix(df) %*% w1)
mean_portb <- mean(as.matrix(df) %*% w2)
Ex_R = NULL
vs = NULL
sd = NULL
for (i in p){
  x =i*mean_porta + (1-i)*mean_portb
  Ex_R = append(Ex_R,x)
  v =(i^2)*var(porta)+((1-i)^2)*var(portb) + (2*i*(1-i))*cov(porta,portb)
  vs = append(vs,v)
  s = sqrt(vs)
  sd = append(sd,s)
}

comb_port <- as.data.frame(cbind(p,Ex_R,vs,sd))
colnames(comb_port)<- c('p','Returns','Var','Stddev')


ggplot() + 
  geom_point( aes(x= Stddev, y= Returns), comb_port,color="#EAAA00", size=0.3, alpha=0.5)+
  theme_pubclean()+scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(panel.border = element_rect(colour = "black" , fill = NA , size = 0.5))+
  ggtitle("frontier 1")

port_2 <- filter(comb_port , sd <= 0.05)
colnames(port_2)<- c('p','Returns','Var','Stddev')

ggplot(port_2, aes(x=`Std dev`, y= Returns))+
  geom_point( color="lightblue", size=1, alpha=1) +
  theme_minimal() +ggtitle(" Frontier 2")
#C
ggplot() + 
  geom_point( aes(x= Stddev, y= Returns), comb_port,color="#EAAA00", size=0.3, alpha=0.5)+
  geom_point( aes(x= Stddev, y= Returns), port_2,color="purple", size=0.5, alpha=0.6)+
  theme_pubclean()+scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(panel.border = element_rect(colour = "black" , fill = NA , size = 0.5))+
  ggtitle("frontier 2")


#d
rf <- 0.004

wf <- as.matrix(solve(matrix)%*%(E_R - rf))/as.numeric(t(M_1)%*%solve(matrix)%*%(E_R - rf))
port_rf <- (as.matrix(df) %*% wf)


Ex_Rf <- NULL
Rp <- mean(port_rf - rf)
sd <- NULL
sigma_values <- seq(0.02,0.06 , by = 0.005)
for (i in sigma_values){
  x2 = rf + Rp*i/ as.numeric(sqrt(var(port_rf)))
  Ex_Rf = append(Ex_Rf,x2)
  s2 <- as.numeric(sqrt(var(Ex_Rf)))
  sd <- append(sd,s)
}

port_Rf <- as.data.frame(cbind(Ex_Rf,sigma_values))
colnames(port_Rf)<- c('Returns','Stddev')


ggplot() + 
  geom_line(aes(x= Stddev, y= Returns),port_Rf, color="light blue", size=1.1, alpha=1)+
  geom_point( aes(x= Stddev, y= Returns), comb_port,color="#EAAA00", size=0.3, alpha=0.5)+
  geom_point( aes(x= Stddev, y= Returns), port_2,color="purple", size=0.5, alpha=0.6)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme_pubclean()+
  theme(panel.border = element_rect(colour = "black" , fill = NA , size = 0.5))+
  ggtitle("Frontier with Risk Free Rate")

#f)
alpha = 4
P <- (mean(port_rf)-rf)/(as.numeric(2*alpha*var(port_rf)))

P

#g)
r6 <- 0.006

w6 <- as.matrix(solve(matrix)%*%(E_R - r6))/as.numeric(t(M_1)%*%solve(matrix)%*%(E_R - r6))
port_r6 <- (as.matrix(df) %*% w6)


Ex_R6 <- NULL
R6 <- mean(port_r6 - r6)
sd6 <- NULL
sigma_values <- seq(0.02,0.06 , by = 0.005)
for (i in sigma_values){
  x6 = r6 + R6*i/ as.numeric(sqrt(var(port_r6)))
  Ex_R6 = append(Ex_R6,x6)
  s6 <- as.numeric(sqrt(var(Ex_R6)))
  sd6 <- append(sd6,s6)
}
port6 <- as.data.frame(cbind(Ex_R6,sigma_values))
colnames(port6)<- c('Returns','Stddev')

r0 <- 0.00

w0 <- as.matrix(solve(matrix)%*%(E_R - r0))/as.numeric(t(M_1)%*%solve(matrix)%*%(E_R - r0))
port_r0 <- (as.matrix(df) %*% w0)


Ex_R0 <- NULL
R0 <- mean(port_r0 - r0)
sd0 <- NULL
sigma_values <- seq(0.02,0.06 , by = 0.005)
for (i in sigma_values){
  x0 = r0 + R0*i/ as.numeric(sqrt(var(port_r0)))
  Ex_R0 = append(Ex_R0,x0)
  s0 <- as.numeric(sqrt(var(Ex_R0)))
  sd0 <- append(sd0,s0)
}
port0 <- as.data.frame(cbind(Ex_R0,sigma_values))
colnames(port0)<- c('Returns','Stddev')



ggplot() + 
  geom_line(aes(x= Stddev, y= Returns),port_Rf, color="light blue", size=1.1, alpha=1)+
  geom_line(aes(x= Stddev, y= Returns),port6, color="#00C0A3", size=1.1, alpha=1)+
  geom_line(aes(x= Stddev, y= Returns),port0, color="#F2ACB9", size=1.1, alpha=1)+
  geom_point( aes(x= Stddev, y= Returns), comb_port,color="#EAAA00", size=0.3, alpha=0.5)+
  geom_point( aes(x= Stddev, y= Returns), port_2,color="purple", size=0.5, alpha=0.6)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+theme_pubclean()+
  theme(panel.border = element_rect(colour = "black" , fill = NA , size = 0.5))+
  ggtitle("Frontier with lending and borrowing")
