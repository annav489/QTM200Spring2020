library(faraway)
library(tidyverse)

gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal , gamble)

#a
ggplot(model1, aes(.fitted, .resid)) + 
  geom_point() +
  stat_smooth(col="red") +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted Values", y="Residuals")
#The plot has more variance near the center of the distribution of fitted values
#but their are relatively few outliers compared to the distribution of the data set as a whole. 

#b 
library(car)
qqPlot(model1, ylab=" Residuals", col = "red")
#Since the ends of the lines diverge from the line of best fit, the data might not 
#be normally distributed due to variation at the extremes. 

#c 
plot(hatvalues(model1))
abline(h = 2*5/47)
abline(h = 3*5/47)
identify(1:47, hatvalues(model1), row.names(gamble))
#The numbered points in the graph have high leverage due to their large hat values. 

#d 
outlierTest(model1)
#The p value from the outlier test is less than 0.05, so I reject the null hypothesis that there
#are no outliers. 

#e
plot(hatvalues(model1), rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex = 10*cook/max(cook))
abline(h = c(-2, 0, 2, 4, 6), lty = 2)
abline(v = c(2,3)*3/45, lty = 2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))
#The bubble labeled 24 has large cook's distance and studentized residual values. 
#This means that this point has a large amount of inlfluence on the model. Point 35, which
#has a large hat value, but a small studentized residual, shows less influence. 


