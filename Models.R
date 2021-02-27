rm(list = ls())
library(MASS)
library(psych)
boston <- Boston
?Boston

set.seed(100)
train <- sample(1:506,400)
training_data <- Boston[train,]
testing_data <- Boston[-train,]
colnames(training_data)
pairs.panels(training_data)
model <- lm(medv~lstat+rm,data = training_data)
summary(model)
Rsqd <- 0.62

model1 <- lm(medv~log(lstat)+rm,data=training_data)
summary(model1)
Rsqd1 <- 0.69

model2 <- lm(log(medv)~log(lstat)+rm,data=training_data)
summary(model2)
Rsqd2 <- 0.67

model3 <- lm(log(medv)~log(lstat)+log(rm),data=training_data)
summary(model3)
Rsqd3 <- 0.67

model4 <- lm(medv~.,data=training_data)
summary(model4)
Rsqd4 <- 0.72

model5 <- lm(medv~. + log(lstat)-lstat,data=training_data)
summary(model5)
Rsqd5 <- 0.77

model6 <- lm(medv~. + log(rm)-rm,data=training_data)
summary(model6)
Rsqd6 <- 0.71

testing_y <- testing_data$medv

pred_y <- predict(model,testing_data)
pred_y1 <- predict(model1,testing_data)
pred_y2 <- predict(model2,testing_data)
pred_y3 <- predict(model3,testing_data)
pred_y4 <- predict(model4,testing_data)
pred_y5 <- predict(model5,testing_data)
pred_y6 <- predict(model6,testing_data)

er <- testing_y-pred_y
er_sqr <- er^2
MSE <- mean(er_sqr)
MSE

er1 <- testing_y-pred_y1
er_sqr1 <- er1^2
MSE1 <- mean(er_sqr1)
MSE1

er2 <- testing_y-exp(pred_y2)
er_sqr2 <- er2^2
MSE2 <- mean(er_sqr2)
MSE2

er3 <- testing_y-exp(pred_y3)
er_sqr3 <- er3^2
MSE3 <- mean(er_sqr3)
MSE3

er4 <- testing_y-pred_y4
er_sqr4 <- er4^2
MSE4 <- mean(er_sqr4)
MSE4

er5 <- testing_y-pred_y5
er_sqr5 <- er5^2
MSE5 <- mean(er_sqr5)
MSE5

er6 <- testing_y-pred_y6
er_sqr6<- er6^2
MSE6 <- mean(er_sqr6)
MSE6


Rsqd_ <- c(Rsqd,Rsqd1,Rsqd2,Rsqd3,Rsqd4,Rsqd5,Rsqd6)
MSE_ <- c(MSE,MSE1,MSE2,MSE3,MSE4,MSE5,MSE6)

model_val <- cbind(Rsqd_,MSE_)
model_val
