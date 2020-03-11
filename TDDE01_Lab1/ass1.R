library("kknn")
library(readxl)
data <- read_excel("/home/augjo318/Desktop/TDDE01_Lab1/spambase.xlsx")

data$Spam <- as.factor(data$Spam)
n=dim(data)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#data[data$. > 0.5,]$.<- 1

logistic <- glm(Spam ~ ., data = train, family = "binomial")

#1.2
res_train <- predict(logistic, train, type = "response")
res_test <- predict(logistic, test, type="response")

res1 <- ifelse(res_train > 0.5, 1, 0)
tbl <- table(train$Spam, res1)
colnames(tbl) = c("Predicted Non-spam", "Predicted spam")
rownames(tbl) = c("Actual Non-spam", "Actual Spam")
tbl
MisClassification_train <- (tbl[1, "Predicted spam"] + tbl[2, "Predicted Non-spam"])/(sum(tbl))
print(MisClassification_train)

res2 <- ifelse(res_test > 0.5, 1, 0)
tbl1 <- table(test$Spam, res2)

colnames(tbl1) = c("Predicted Non-spam", "Predicted spam")
rownames(tbl1) = c("Actual Non-spam", "Actual Spam")
tbl1
MisClassification_test <- (tbl1[1, "Predicted spam"] + tbl1[2, "Predicted Non-spam"])/(sum(tbl1))
print(MisClassification_test)


#1.3
res3 <- ifelse(res_train > 0.8, 1, 0)
tbl2 <- table(train$Spam, res3)
colnames(tbl2) = c("Predicted Non-spam", "Predicted spam")
rownames(tbl2) = c("Actual Non-spam", "Actual Spam")
tbl2
MisClassification_train <- (tbl2[1, "Predicted spam"] + tbl2[2, "Predicted Non-spam"])/(sum(tbl2))
print(MisClassification_train)

res4 <- ifelse(res_test > 0.8, 1, 0)
tbl3 <- table(test$Spam, res4)
colnames(tbl3) = c("Predicted Non-spam", "Predicted spam")
rownames(tbl3) = c("Actual Non-spam", "Actual Spam")
tbl3
MisClassification_test <- (tbl3[1, "Predicted spam"] + tbl3[2, "Predicted Non-spam"])/(sum(tbl3))
print(MisClassification_test)


kknn_model <- kknn(Spam ~., train, test, k = 30) 
kknn_model0 <- kknn(Spam ~., train, train, k = 30)
fit0 <- fitted(kknn_model0)
fit <- fitted(kknn_model)

tbl4 <- table(test$Spam, fit)
tbl5 <- table(train$Spam, fit0)


MisClassification <- (tbl4[1, "1"] + tbl4[2, "0"])/(sum(tbl4))
MisClassification2 <- (tbl5[1, "1"] + tbl5[2, "0"])/(sum(tbl5))

print(MisClassification)
print(MisClassification2)

kknn_model2 <- kknn(Spam ~., train, test, k =1)
kknn_model3 <- kknn(Spam ~., train, train, k =1)

fit2 <- fitted(kknn_model2)
fit3 <- fitted(kknn_model3)

tbl6 <- table(test$Spam, fit2)
tbl7 <- table(train$Spam, fit3)

MisClassification3 <- (tbl6[1, "1"] + tbl6[2, "0"])/(sum(tbl6))
MisClassification4 <- (tbl7[1, "1"] + tbl7[2, "0"])/(sum(tbl7))

print(MisClassification3)
print(MisClassification4)

