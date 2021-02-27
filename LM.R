library(MASS)
library(ISLR)
View(Boston)

set.seed(1)
train <- sample(1:506,400)
#range is 1 to 506 and length is 400 rest 106 are still remaning which will be provided by
tranining_data <- Boston[train,]
testing_data <- Boston[-train,]
str(Boston)
#1 to 506

plot(tranining_data$rm,tranining_data$medv)


model <- lm(medv~lstat,data = tranining_data)
summary(model)