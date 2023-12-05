setwd("C:/Users/emanu/Downloads")
data = read.csv("Life-Expectancy-Data.csv",sep = ";")

library(tidyverse)
print(data$Life_expectancy)
hist(data$Life_expectancy) 
Yr = 2015
data_filt = data %>% filter(Year==Yr)
life_exp=data_filt$Life_expectancy

data1=data[,c(20,4:10)]


plot(data1) 

labs=dimnames(data)[[1]]
for(i in 4:10){
  boxplot(data[,i]) 
}


llife_exp=log(life_exp) 
plot(llife_exp)
plot(life_exp)

infant_death=data_filt$Infant_deaths
Under_5death=data_filt$Under_five_deaths
Adult_mortality=data_filt$Adult_mortality
Alcohol=data_filt$Alcohol
Hep_B=data_filt$Hepatitis_B
Measles=data_filt$Measles
BMI=data_filt$BMI


anal=lm(llife_exp~infant_death,Under_5death, Adult_mortality,Alcohol,Measles,BMI)
par(mfrow=c(1,4))
boxplot(anal$residuals,xlab="residuals")
plot(density(anal$resid),xlab="residuals",main="",ylab="Kernel Density")
plot(anal,which=c(1,2))
