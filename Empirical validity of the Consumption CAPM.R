library(readxl)
df<-read_excel("D:\\Downloads\\PS1Data.xlsx")
head(df)
df_new <- df

#Calculate m 
g <- as.vector(c(2:100))
for (i in g ) {
  df_new[[paste0("m_",i)]]<- df_new$dc^-i ##why does not append work ? 
  
}
df_new$Rm <- df_new$`Rm-r`+df$r-1
gamma_values <- select(df_new, starts_with("m_"))

library(dplyr)
library(ggplot2)

covariance <- NULL
covariance <- -cov(gamma_values,df_new$Rm)
df_new$covs <- NULL
mean_r <- mean(df_new$r)
mean_RM_r <- mean(df_new$`Rm-r`)
cov_mean_r <- covariance*mean_r
plot_1 <- cbind(cov_mean_r,g)
plot_1 <- as.data.frame(plot_1)
colnames(plot_1) <- c('Cov x Mean R','G')
ggplot(plot_1, aes(x= `G`, y= `Cov x Mean R`)) + 
  geom_point()
inv_m <- 1/gamma_values
library(readxl)
df<-read_excel("D:\\Downloads\\PS1Data.xlsx")
head(df)
df_new <- df

#Calculate m 
g <- as.vector(c(2:100))
for (i in g ) {
  df_new[[paste0("m_",i)]]<- df_new$dc^-i ##why does not append work ? 
  
}
df_new$Rm <- df_new$`Rm-r`+df$r-1
gamma_values <- select(df_new, starts_with("m_"))

library(dplyr)
library(ggplot2)

covariance <- NULL
covariance <- -cov(gamma_values,df_new$Rm)
df_new$covs <- NULL
mean_r <- mean(df_new$r)
mean_RM_r <- mean(df_new$`Rm-r`)
cov_mean_r <- covariance*mean_r
cov_g <- cbind(cov_mean_r,g)
cov_g <- as.data.frame(cov_g)
colnames(cov_g) <- c('Cov x Mean R','G')

library(ggpubr)
cov_g %>%
  mutate(higlight_flag = ifelse(`Cov x Mean R` = mean_RM_r , T ,F)) %>%
  cov_g_plot<- ggplot(aes(x= `G`, y= `Cov x Mean R`)) +
  geom_point(color='darkblue')+ geom_point(,aes(color = highlight_flag)) +
  scale_color_manual(values = c('#595959', 'red'))+
  theme_pubclean()
detach("package:ggpubr", unload = TRUE)

##1 . B
inv_m <- 1/gamma_values
inv_mean_m <- sapply(inv_m,mean)
inv_mean_m <- as.data.frame(inv_mean_m)
inv_mean_gamma <- as.data.frame(cbind(inv_mean_m,g))
colnames(inv_mean_gamma) <- c('Inv_mean','G')
ggplot(inv_mean_gamma, aes(x= `G`, y= `Inv_mean`)) + 
  geom_point(color='green') + theme_pubclean()
