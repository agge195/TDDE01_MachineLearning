library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10) #sample 50 points uniformly at random [0, 10]
Var
trva <- data.frame(Var, Sin=sin(Var))
trva

tr <- trva[1:25,] #training
va <- trva[26:50,] #eval

winit <- runif(31, -1, 1)
MSE <- c()

for(i in 1:10) {
  nn <- neuralnet(Sin~ Var, data=tr, hidden=10, threshold = (i/1000), startweights = winit)
  pred <- neuralnet::predict(nn, va)
  MSE[i] <- mean((va$Sin - pred)^2)
}

#Task2: You used wrong initial weights therefore the whole task is wrong, 
# also you should use the whole dataset to produce the final model

plot(MSE)
print(which.min(MSE)) # return index of lowest Mean squared error
print(MSE[which.min(MSE)])
print(which.max(MSE))
nn2 <- neuralnet(Sin~ Var, data=tr, hidden=10, threshold = (which.min(MSE)/1000), startweights = winit)
plot(nn2)
nn3 <- neuralnet(Sin~ Var, data=tr, hidden=10, threshold = (which.max(MSE)/1000), startweights = winit)
plot(prediction(nn2)$rep1) # best
points(trva, col = "red")