library(readxl)
library(ggplot2)
data <- read_excel("/home/augjo318/Desktop/TDDE01_Lab1/machines.xlsx")

LL <- function(x, theta){
  return(length(x)*log(theta) - theta*sum(x))
}

ML <- function(x) {
  return(length(x)/(sum(x)))
}

bayes <- function(x, lambda) {
  prior = lambda*exp(-lambda*theta)
  l = log(prior) + (LL(x, theta))
  
  return (l)
}

theta <- seq(from=0.00, to=3, by=0.01)

res <- LL(data$Length, theta)
maxlik <- ML(data$Length)
print(maxlik)

obs = head(data, 6)
maxlik2 <- ML(obs$Length)
print(maxlik2)

res3 <- LL(obs$Length, theta)

res4 <- bayes(data$Length, 10)

r1 <- data.frame(theta = theta, loglik = LL(data$Length, theta))

r2 <- data.frame(theta = theta, loglik = LL(obs$Length, theta))

r3 <- data.frame(theta = theta, loglik = bayes(data$Length, 10))

combine2 <- rbind(r1, r3)
combine3 <- rbind(r1, r2, r3)

ggplot(r1, aes(theta, loglik))+geom_point()
ggplot(combine2, aes(theta, loglik))+geom_point()

ggplot(combine3, aes(theta, loglik))+geom_point()


maxlik3 <- theta[which.max(res4)]
print(maxlik3)

observation2 = rexp(50, maxlik)

hist(data$Length, col="red", xlim=c(0,5), ylim=c(0,25), main = "")
hist(observation2, col="blue", xlim=c(0,5), ylim=c(0,25), main = "")

