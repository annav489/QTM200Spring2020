install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#1a 
Prestige$professional <- ifelse(Prestige$type=="prof", 1, 0)

#1b 
presitge_regression <- lm(prestige ~ income+professional + income:professional, data = Prestige)
summary(presitge_regression)

#1c 
#Y = 21.142259 + 0.003171(incomex1) + 37.781280(professionalx2) -0.002326(incomex1)(professionalx2) 

#1d 
#For every 1 dollar increase in income, there is a 0.003171 increase in the Pineo-Porter prestige score for an occupation, 
#when type of profession is held constant. 

#1e 
#Being a professional, instead of a white or blue collar worker, leads to a 37.781280 increase in the Pineo-Porter prestige score
#for an occuptaion when income is held constant. 

#1f
Y_hat_income <- 21.142259 + 0.003171*(1000) + 37.781280*(1) -0.002326*(1000)*(1) # 59.76854
Y_hat_income_increase <- 21.142259 + 0.003171*(2000) + 37.781280*(1) -0.002326*(2000)*(1) #60.61354
Delta_y_hat_income <- Y_hat_income_increase-Y_hat_income #0.845
#A $1,000 increase in income leads to, on average, a 0.845 increase in the prestige score for professional occupations 

#1g 
Y_hat_nonprof <- 21.142259 + 0.003171*(6000) + 37.781280*(0) -0.002326*(6000)*(0) #40.16826
Y_hat_prof <- 21.142259 + 0.003171*(6000) + 37.781280*(1) -0.002326*(6000)*(1) #63.99354 
Delta_y_hat_prof <- Y_hat_prof-Y_hat_nonprof 
#A professional occupation with a $6,000 income has, on average, a 23.82528 increse in the prestige score
#than a non-professional occuption with a $6,000 income. 

#2a
#Hypothesis: Ho: B1 = 0, Ha: B1 =/ 0
test_stat1 <- (0.042- 0)/0.016
n1 <- 30
p_val1 <- 2*pt(test_stat1, n1-1, lower.tail = FALSE)
p_val1 #0.01368397
#With a significance level of 0.05 and a p-value of 0.01368397, I reject the null hypothesis and conclude that having yard signs affects vote share. 

#2b
#Hypothesis: Ho: B2 = 0, Ha: B2 =/ 0
test_stat2 <- (0.042- 0)/0.013
n2 <- 76
p_val2 <- 2*pt(test_stat2, n2-1, lower.tail = FALSE) 
p_val2 #0.001834302
#With a significance level of 0.05 and a p-value of 0.001834302, I reject the null hypothesis and conclude that living in a precint next to yard signs affects vote share. 

#2c 
#In a precinct that does not have yard signs and is not adjacent to a precint with yard signs, 
#30.2% of the vote share, on average, goes to Ken Cuccinelli. 

#2d 
#The R^2 value for this model is 0.094. This indicates that only 9.4% of the variability is explained by the model. 
#This R^2 value demonstrates that there are other, more important, confounding variables which infuence vote share. 
















