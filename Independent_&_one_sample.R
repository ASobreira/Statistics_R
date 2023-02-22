
#Dataset:  kid_calories.csv

#Source: K. nan der Horst, A. Ferrage, A. Rytz (2014). "Involving Children
#in Meal Preparation," Appetite. Vol. 79, pp.18-24

#Description: Experiment conducted to determine effect of children
#participating in meal preparation on caloric intake. Two groups
#(independent samples). Treatment 1: Children participated.
#Treatment 2: Did not participate.

#Variables:
  #Treatment
  #Caloric Intake

#1st Hipoteses: Do both groups difer in terms of their caloric intake mean value?


#Packages
library(skimr)
library(readr)

#Dataset
ds01 <- read_csv("kid_calories.csv")
view(ds01)
head(ds01)
attach(ds01)

#Sample caracteristics
one <- Calories[Trt==1]
two <- Calories[Trt==2]
skim(one)
skim(two)

##Boxplot
boxplot(Calories~Trt)

##Histogram
hist(Calories)

#Variance Test
var.test(one, two)
var.test(Calories~Trt)
  #Dont Reject H0

#Data Gaussian Evaluation
scaled_Calories <- scale(Calories)
qqnorm(scaled_Calories); qqline(scaled_Calories)

shapiro.test(one)
shapiro.test(two)
  #Dont Reject H0

# T Test Independent Sample with equal variances - H0:miu1=miu2 vs H1: miu1!=miu2
t.test(one, two, paired = FALSE, var.equal = TRUE, alternative = "two.sided", conf.level = .95)

  # Reject H0 with alpha .01
  # Conf Interval 95% for diference of means= [24.04242, 145.15859]
  # t= 2.8137 -> miu1 > miu2


#2nd Hipoteses: Does 1st group  caloric intake mean value is higher than recommendation of 400 calories per meal?
## T.Test One sample - H0 miu <= 400 vs H1: miu > 400  
t.test(one, mu = 400, alternative = "greater", conf.level = .95) 
  # Dont Reject H0

## I.C 95%
q <- 2.0639
sd <- sd(one)
mean <- mean(one)
lim_inf <- mean-q * (sd/sqrt(25))
lim_sup <- mean+q * (sd/sqrt(25))
t.test(one)
#[387.768, 475.031]




 
