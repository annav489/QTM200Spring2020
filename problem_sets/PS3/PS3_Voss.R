incumbents <- read.csv("incumbents_subset.csv")

##1.1 
voteshare_difflog <- lm(incumbents$voteshare~incumbents$difflog)

#1.2 
plot(incumbents$difflog, incumbents$voteshare)
abline(voteshare_difflog)

#1.3
res_vd <- residuals(voteshare_difflog)

#1.4
#voteshare_difflog: 
#Y = 0.04167x + 0.57903

#2.1
presvote_difflog <- lm(incumbents$presvote~incumbents$difflog)

#2.2
plot(incumbents$difflog, incumbents$presvote)
abline(presvote_difflog)

#2.3
res_pd <- residuals(presvote_difflog)

#2.4
#presvote_difflog: 
#Y = 0.02384x + 0.50758

#3.1
voteshare_presvote <- lm(incumbents$voteshare~incumbents$presvote)

#3.2 
plot(incumbents$presvote, incumbents$voteshare)
abline(voteshare_presvote)

#3.3
#voteshare_presvote
#Y = 0.3880x + 0.4413

#4.1
residuals1_2 <- lm(res_vd~res_pd)

#4.2
plot(res_pd, res_vd)
abline(residuals1_2)

#4.3 
#residuals1_2: 
#Y = 0.2569x -4.860e-18

#5.1
lm(voteshare~difflog+presvote, data = incumbents)

#5.2 
#Y = 0.03554(difflog) + 0.25688(presvote) + 0.44864

#5.3 


