# QUIZ 3 Statistical Inference

setwd("~/Coursera/DataScience/DataScienceCoursera/06_StatisticalInference")

# QUESTION 1

#In a population of interest, a sample of 9 men yielded a sample average brain
# volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's 
#T confidence interval for the mean brain volume in this new population?

mu <- 1100
sd <- 30
n <- 9
df <- n-1
alpha <- 0.05
ans1 <- mu + c(-1,1)*abs(qt(alpha/2,df))*sd/sqrt(n)
print(round(ans1))


# QUESTION 2
# A diet pill is given to 9 subjects over six weeks. The average difference in 
# weight (follow up - baseline) is -2 pounds. What would the standard deviation 
# of the difference in weight have to be for the upper endpoint of the 95% 
# T confidence interval to touch 0?

mu <- -2
n <- 9
df <- n-1
alpha <- 0.05
ans2 <- (-mu)/(qt(1-alpha/2,df))*sqrt(n)
print(round(ans2, digits = 2))

# QUESTION 3

# In a study of emergency room waiting times, investigators consider a new and 
# the standard triage systems. To test the systems, administrators selected 20 
# nights and randomly assigned the new triage system to be used on 10 nights 
# and the standard system on the remaining 10 nights. They calculated the 
# nightly median waiting time (MWT) to see a physician. The average MWT for the 
# new system was 3 hours with a variance of 0.60 while the average MWT for the 
# old system was 5 hours with a variance of 0.68. Consider the 95% confidence 
# interval estimate for the differences of the mean MWT associated with the new 
# system. Assume a constant variance. What is the interval? Subtract in this 
# order (New System - Old System). 

# Independent group t confidence interval for independant groups
mwt_new <- 3
sd_new <- sqrt(0.6)
n_new <- 10
df_new <- n_new -1
mwt_old <- 5
sd_old <- sqrt(0.68)
n_old <- 10
df_old <- n_old -1
df <- n_new + n_old -2

sp <- sqrt((df_old * sd_old^2 + df_new*sd_new^2)/(df))
interval <- mwt_new - mwt_old + c(-1,1)*qt(0.975, df)*sp*(1/n_old + 1/n_new)^0.5
print(round(interval, digits = 2))



# QUESTION 6

# To further test the hospital triage system, administrators selected 200 nights 
# and randomly assigned a new triage system to be used on 100 nights and a 
# standard system on the remaining 100 nights. They calculated the nightly 
# median waiting time (MWT) to see a physician. The average MWT for the new 
# system was 4 hours with a standard deviation of 0.5 hours while the average 
# MWT for the old system was 6 hours with a standard deviation of 2 hours. 
# Consider the hypothesis of a decrease in the mean MWT associated with the new 
# treatment. 

mwt_new <- 4
sd_y <- 0.5
mwt_od <- 6
sd_x <- 2
n <- 100

df <- (sd_x^2/n + sd_y^2/n)^2/
    ((sd_x^2/n)^2/(n-1)+(sd_y^2/n)^2/(n-1))
alpha = 0.05
t_dist <- qt(1-alpha/2,df = n-1)

ans6 <- mwt_od - mwt_new + c(-1,1)*t_dist*(sd_x^2/n + sd_y^2/n)^0.5
print(round(ans6, digits = 3))
print("The confidence interval is well above 0 so the new system is working")


# QUESTION 7 
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill 
# and a placebo. Subjects' body mass indices (BMIs) were measured at a baseline 
# and again after having received the treatment or placebo for four weeks. The 
# average difference from follow-up to the baseline (followup - baseline) was 
# -3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The 
# corresponding standard deviations of the differences was 1.5 kg/m2 for the 
# treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI 
# over the four week period appear to differ between the treated and placebo 
# groups? Assuming normality of the underlying data and a common population 
# variance, calculate the relevant *90%* t confidence interval. Subtract in the
# order of (Treated - Placebo) with the smaller (more negative) number first.


mean_placebo <- 1
mean_pill <- -3
sd_placebo <- 1.8
sd_pill <- 1.5
n <- 9
df <- (sd_placebo^2/n + sd_pill^2/n)^2/
    ((sd_placebo^2/n)^2/(n-1)+(sd_pill^2/n)^2/(n-1))
alpha = 0.1
t_dist <- qt(1-alpha/2,df = df)
# answer from the quiz
t_dist <- qt(1-alpha/2,df = 16)

ans7 <- mean_pill - mean_placebo + c(-1,1)*t_dist*(sd_placebo^2/n + sd_pill^2/n)^0.5
print(round(ans7, digits = 3))

