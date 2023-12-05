
data = read.csv("C:/Users/utente1/Downloads/Life-Expectancy-Data.csv" ,sep = ";")
print(data)


library(tidyverse)
print(data)
hist(data$Life_expectancy) 
Yr = 2015
data_filt = data %>% filter(Year==Yr)
#data_filt2 = data_filt %>% filter(Region=="EU") 
life_exp=data_filt$Life_expectancy
life_exp1=round(life_exp, digits = 0)
#llife = round(log(life_exp1),digits=3)
#hist(llife) 
hist(life_exp) 
n=length(life_exp1)



life_exp1.freq=table(life_exp1)
barplot(life_exp1.freq)


#compute sample mean
obs_mean=mean(life_exp) #so that we won't lose some information
#compute observed relative frequencies


tot=sum(life_exp1.freq)
rfreq_obs=c(life_exp1.freq/tot)

nb.freq=length(rfreq_obs)


min_value <- min(life_exp1)
max_value <- max(life_exp1)
rfreq_theo <- dpois(min_value:max_value, lambda = obs_mean)

# Plot observed and theoretical relative frequencies
plot(0:(nb.freq - 1), rfreq_obs, ylab = "Relative frequency", type = "h", lwd = 1, col = "lightgrey", ylim = c(0, max(c(rfreq_obs, rfreq_theo))))
lines(0:(nb.freq + 1), rfreq_theo, lwd = 3, col = "blue")

std=sd(life_exp)
median = median(life_exp)
quantiles = c(quantile(life_exp,0.25),quantile(life_exp,0.5),quantile(life_exp,0.75))

boxplot(life_exp)



