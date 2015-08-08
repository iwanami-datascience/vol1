library(rstan)
set.seed(123)

N <- 20; a <- 0.5; b <- 3; T <- 1:N/10
Y <- rnorm(N, mean=a+b*T, sd=1)
data <- list(N=N, T=T, Y=Y)

stanmodel <- stan_model(file='model1.stan')
fit <- sampling(
   stanmodel,
   data=data,
   pars=c('a','b'),
   init=function() {
      list(a=runif(1,-1,1), b=runif(1,-1,1), sigma=1)
   },
   seed=1234,
   iter=1000, warmup=200, thin=2, chains=3
)