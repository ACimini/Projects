#Needed Packages
library(ggplot2)

setwd("C:/Users/alexc/Desktop/Projects/German Credit Risk/Data")
#Be sure to set the Working directory to lead in the necessary data

#Loads in the training data set
data = read.csv("Train_Credit_Data.csv")
data = read.csv("german_credit_data.csv")
#Histogram of ages from the applicants
hist(data$Age)

#Histogram of the credit amounts
hist(data$Credit.amount)

#Credit amount for the age of the individual
ggplot(data = data, aes(x = Age, y = Credit.amount)) + geom_point()

#Credit amount vs duration
ggplot(data = data, aes(x = Duration, y = Credit.amount)) + geom_point()

#comparison of good vs bad risk applications in the training data
ggplot(data = data, aes(x = Risk)) + geom_bar()

unique(data$Purpose)



