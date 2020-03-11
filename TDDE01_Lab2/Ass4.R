library("pcaMethods")
library("fastICA")
#4.1

data <- read.csv(file = "/home/augjo318/Desktop/TDDE01_Lab2/NIRSpectra.csv", sep = ";", dec = ",")

data$Viscosity = c()
res = prcomp(data)

lambda = res$sdev^2

# xlambda

sprintf("%2.3f", lambda/sum(lambda)*100)
screeplot(res)

#res$x

plot(res$x[,1], res$x[,2], xlab = "PC1", ylab = "PC2")

#4.2

U = res$rotation
plot(U[,1], main = "Traceplot, PC1")
plot(U[,2], main = "Traceplot, PC2")

#4.3
set.seed(12345)

#S <- cbind(sin(1:1000)/20, rep((((1:200)-100)/100), 5))
S<- U
A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)

a <- fastICA(data, 2) 
#a

W_p <- a$K %*% a$W 

plot(1:length(W_p[,1]), W_p[,1])
plot(1:length(W_p[,2]), W_p[,2])

plot(a$S[,1], a$S[,2])

