library(rstan)
set.seed(123)

N <- 20; a <- 0.5; b <- 3; T <- 1:N/10
Y <- rnorm(N, mean=a+b*T, sd=1)
data <- list(N=N, T=T, Y=Y)

fit <- stan(file='model1.stan', data=data)
