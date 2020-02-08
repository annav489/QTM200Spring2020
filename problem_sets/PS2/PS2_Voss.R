##Question 1 
#1a 
discrimination <- matrix(c(14, 6, 7, 7, 7, 1),ncol=3,byrow=TRUE)
colnames(discrimination) <- c("Not_Stopped","Bribe_Requested","Stopped_Given_Warning")
rownames(discrimination) <- c("Upper_Class","Lower_Class")
discrimination <- as.table(discrimination)
discrimination

totals <- sum(discrimination)
#Sum Not Stopped 
not_stopped <- sum(14 + 7)
#Sum Bribe Requested 
bribe_requested <- sum(6 + 7)
#Sum Warning 
warning <- (7 + 1)
#Sum Upper Class
upper <- sum(14 + 6 + 7)
#Sum Lower Class
lower <- sum(7 + 7 + 1)

#Expected Not Stopped, Upper Class
fe1 <- (upper/totals)*not_stopped
#Expected Bribe requested, Upper Class
fe2 <- (upper/totals)*bribe_requested
#Expected Warning Given, Upper Class
fe3 <- (upper/totals)*warning
#Expected Not Stopped, Lower Class
fe4 <- (lower/totals)*not_stopped
#Expected Bribe requested, Lower Class
fe5 <- (lower/totals)*bribe_requested
#Expected Warning Given, Lower Class
fe6 <- (lower/totals)*warning

##Observed-Expected squared divided by expected
#Not Stopped, Upper Class
chisquare1 <- (14-fe1)^2/fe1
#Expected Bribe requested, Upper Class
chisquare2 <- (6-fe2)^2/fe2
#Expected Warning Given, Upper Class
chisquare3 <- (7-fe3)^2/fe3
#Expected Not Stopped, Lower Class
chisquare4 <- (7-fe4)^2/fe4
#Expected Bribe requested, Lower Class
chisquare5 <- (7-fe5)^2/fe5
#Expected Warning Given, Lower Class
chisquare6 <- (1-fe6)^2/fe6

#Test Statistic 
chi_square_stat <- sum(chisquare1,chisquare2,chisquare3,chisquare4,chisquare5,chisquare6)

#1b 
# df = (rows-1))columns-1)
df_pchisq <- (2-1)*(3-1)
pchisq(chi_square_stat, df = df_pchisq, lower.tail = FALSE)
## With a significance of alpha = 0.1, I fail to reject the null hypothesis and conclude that 
## the class and bribe solicitation are statistically independent 

#1c
f0 - fe/ sqrt(fe(1-rowtotal/total)*(1-columntotal/total))
z1 <- (14-fe1)/sqrt(fe1*(1-(upper/totals)*(1-(not_stopped/totals))))
z2 <- (6-fe2)/sqrt(fe2*(1-(upper/totals)*(1-(bribe_requested/totals))))
z3 <- (7-fe3)/sqrt(fe3*(1-(upper/totals)*(1-(warning/totals))))
z4 <- (7-fe4)/sqrt(fe4*(1-(lower/totals)*(1-(not_stopped/totals))))
z5 <- (7-fe5)/sqrt(fe5*(1-(lower/totals)*(1-(bribe_requested/totals))))
z6 <- (1-fe6)/sqrt(fe6*(1-(lower/totals)*(1-(warning/totals))))

#1d
#Standardized residuals show us how far away each observed value is from the "expectation" 
#Thus, standardized residuals can tell us the accuracy of the expected values and the significance of each cell to the chi squared statistic 


##Question 2 
download.file("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", "women.csv")
women <- read.csv("women.csv")
head(women)
#2a
#Null Hypothesis: The Reservation policy has no effect on the number of new or repaired drinking water facilities in the villages. 
#Alternative Hypothesis:The Reservation policy has an effect on the number of new or repaired drinking water facilities in the villages 

#2b 

#Standardize variation 
mean_water_y <- mean(women$water)
mean_reserved_x <- mean(women$reserved) 
sum_y <- sum(women$water)
sum_x <- sum(women$reserved)

b_hat <- sum((women$water - mean(women$water)) * (women$reserved - mean(women$reserved))) /
  sum((women$reserved - (mean_reserved_x))^2)

a_hat <- mean_water_y - (b_hat*mean_reserved_x)
#Check
lm(women$water ~ women$reserved)

#2c 
#For every increase in reservation for women leaders, there was a 9.252 increase in new or repaired drinking water facilities. 


##Question 3 
fruitfly <- read.csv("fruitfly.csv") 

#3.1
summary(fruitfly)
hist(fruitfly$lifespan) 

#3.2
library(tidyverse)
qplot(x = thorax, y = lifespan, data = fruitfly)
#Yes, there seems to be a linear correlation 

cor(fruitfly$thorax, fruitfly$lifespan, method="pearson")
#Check if null p = 0
cor.test(fruitfly$thorax, fruitfly$lifespan)

#3.3 
lm(fruitfly$lifespan~fruitfly$thorax)
#For every 1 mm increase in thorax size, lifespan increases by 144.33 days 

#3.4 
cor.test(fruitfly$lifespan,fruitfly$thorax) #P-value = 1.497e-15 
## With a p-value of 1.497e-15, I reject the null hypothesis that there is no correlation in the population between thorax and lifespan. 

#3.5 
##p-value = 1.497e-15 (see 3.4) 
#Formula based on t values: 
confint(lm(fruitfly$lifespan~fruitfly$thorax), level = 0.90)
#[118.19616,170.4700]

#3.6.1 
new_fruitfly <- fruitfly
new_fruitfly$thorax <- 0.8
#Condifence intervals around expected values of lifespan
pred_interval <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata=new_fruitfly , interval = "prediction", level = 0.95)
conf_interval <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata=new_fruitfly , interval = "confidence", level = 0.95)

#3.6.2
#Expected Values of Lifespan: 
predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata=new_fruitfly , se.fit = TRUE) 

#3.7 
fitted_lifespan <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata=new_fruitfly , se.fit = TRUE)
reg <- lm(fruitfly$lifespan~fruitfly$thorax)

new_df <- cbind(fruitfly, pred_interval, conf_interval)
names(new_df)[9] <- "fit_conf"
names(new_df)[10] <- "lwr_conf"
names(new_df)[11] <- "upr_conf"

ggplot(new_df, aes(x=thorax, y=lifespan))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")





