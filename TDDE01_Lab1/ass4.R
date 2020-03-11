library(readxl)
library(Metrics)
library(ggplot2)
data <- read_excel("/home/augjo318/Desktop/TDDE01_Lab1/tecator.xlsx")

# moisture is n.dist

# expected moisture is polynomial function of protein, 
# including the polynomial terms to power up

# MSE criterion

protein = data$Protein
fat = data$Fat
moisture = data$Moisture
# bild1
plot(protein, moisture)

n = dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

MSEtrain <- array(1:6)
MSEtest <- array(1:6)

# 1
model1 <- lm(Moisture ~ Protein, data = train)
trainpred <-  predict(model1, train, type = "response")
testpred <- predict(model1, test, type = "response")

MSEtrain[1] = mse(trainpred, train$Moisture)
MSEtest[1] = mse(testpred, test$Moisture)

#2
model2 <- lm(Moisture ~ Protein + I(Protein^2), data = train)
trainpred <-  predict(model2, train, type = "response")
testpred <- predict(model2, test, type = "response")

MSEtrain[2] = mse(trainpred, train$Moisture)
MSEtest[2] = mse(testpred, test$Moisture)

#3

model3 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3), data = train)
trainpred <-  predict(model3, train, type = "response")
testpred <- predict(model3, test, type = "response")

MSEtrain[3] = mse(trainpred, train$Moisture)
MSEtest[3] = mse(testpred, test$Moisture)

#4

model4 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data = train)
trainpred <-  predict(model4, train, type = "response")
testpred <- predict(model4, test, type = "response")

MSEtrain[4] = mse(trainpred, train$Moisture)
MSEtest[4] = mse(testpred, test$Moisture)

#5

model5 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5), data = train)
trainpred <-  predict(model5, train, type = "response")
testpred <- predict(model5, test, type = "response")

MSEtrain[5] = mse(trainpred, train$Moisture)
MSEtest[5] = mse(testpred, test$Moisture)

#6

model6 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5) + I(Protein^6), data = train)
trainpred <-  predict(model6, train, type = "response")
testpred <- predict(model6, test, type = "response")

MSEtrain[6] = mse(trainpred, train$Moisture)
MSEtest[6] = mse(testpred, test$Moisture)


print(MSEtrain)
print(MSEtest)
# bild 2
plot(MSEtrain,type = "o", col = "red", xlab = "Iteration", ylab = "MSE")
par(new=TRUE)
plot(MSEtest,type = "o", col = "blue", xlab = "Iteration", ylab = "MSE", axes = FALSE, ann = FALSE)


# 4.4

library(MASS)

channel1_100 <- data[,2:101]
fat = data$Fat

model <- lm(fat ~ ., channel1_100)
t <- stepAIC(model)


# 4.5, 4.6

library(glmnet)

ridge <- glmnet(as.matrix(channel1_100), data$Fat, alpha = 0, family = "gaussian")
lasso <- glmnet(as.matrix(channel1_100), data$Fat, alpha = 1, family = "gaussian")
#bild 3
plot(ridge, xvar = "lambda")
plot(lasso, xvar = "lambda")

#bild 4
crossvalidation <- cv.glmnet(as.matrix(channel1_100), data$Fat, alpha = 1, family = "gaussian")
plot(crossvalidation)


print(crossvalidation$lambda.min)
print(crossvalidation)

