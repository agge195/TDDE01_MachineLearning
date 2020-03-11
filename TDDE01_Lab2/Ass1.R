library(readxl)
library(ggplot2)
library(reshape2)
library(MASS)

data <- read.csv("/home/augjo318/Desktop/TDDE01_Lab2/australian-crabs.csv")

#1.1
CL <- data$CL
RW <- data$RW
sex <- data$sex

plot(RW, CL, col = data$sex)

#1.2
fit <- lda(sex~RW+CL, data= data)

df = data.frame(CL, RW)

pred <- predict(fit, df)

plot(RW, CL, col=pred$class)

tbl <- table(pred$class, data$sex)

MisClassification <- (tbl[1, "Male"] + tbl[2, "Female"])/(sum(tbl))

MisClassification

#1.3
fit2 <- lda(sex~RW+CL, data=data, prior= c(0.9,0.1))
pred2 <- predict(fit2, df)

plot(RW, CL, col=pred2$class)

tbl <- table(pred2$class, data$sex)

tbl

MisClassification <- (tbl[1, "Male"] + tbl[2, "Female"])/(sum(tbl))

MisClassification


#1.4
fit3 <- glm(sex~RW+CL, data = data, family = "binomial")

pred3 <- predict(fit3)

pred3 <- pred3 > 0

plot(data$RW, data$CL, col = pred3+1)
abline(coef = c(0,2), col = "black")

tbl <- table(pred3, data$sex)  

tbl
MisClassification <- (tbl[1, "Male"] + tbl[2, "Female"])/(sum(tbl))
MisClassification
