# Dataset:  caffeine.dat

# Source: W.J. Pasman, M.A. van Baak, A.E. Jeukendrup, A. de Haan (1995).
# "The Effect of Different Dosages of Caffeine on Endurance Performance Time",
# International Journal of Sports Medicine, Vol. 16, pp225-230.

# Description: Endurance Times for nine well-trained cyclists, on each
# of 4 doses of caffeine (0,5,9,13 mg) with 1 line per subject.

#Packages
library(skimr)
library(readr)
library(ggplot2)

#Dataset
no_caffeine <- c(36.05, 52.47, 56.55, 45.2, 35.25, 66.38, 40.57, 57.15, 28.34)
caffeine_13mg <- c(37.55, 59.3, 79.12, 58.33, 70.54, 69.47, 46.48, 66.35, 36.2)
ds03 <- data.frame(no_caffeine, caffeine_13mg)

#Sample caracteristics
skim(no_caffeine)
skim(caffeine_13mg)

##Boxplot
boxplot(no_caffeine, caffeine_13mg, 
        names=c("no caffeine", "13 mg of caffeine"),
        ylab="Endurance times", 
        col=c("snow", "snow"))

##Histogram
hist(no_caffeine)
hist(caffeine_13mg)

#Data Gaussian Evaluation
scaled_no_caffeine <- scale(no_caffeine)
scaled_caffeine_13mg <- scale(caffeine_13mg)

qqnorm(scaled_caffeine_13mg); qqline(scaled_caffeine_13mg)
qqnorm(scaled_no_caffeine); qqline(scaled_no_caffeine)


shapiro.test(scaled_caffeine_13mg)
shapiro.test(scaled_no_caffeine)

# T Test Paired Sample with equal variances - H0:miu1-miu2=d vs H1: miu1-miu2!=d
t.test(no_caffeine, caffeine_13mg, paired = TRUE)
## Or
d = no_caffeine - caffeine_13mg
t.test(d)
# Reject H0, miu1 < miu2