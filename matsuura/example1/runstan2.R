library(rstan)
set.seed(123)

N <- 20; a <- 0.5; b <- 3; T <- 1:N/10
Y <- rnorm(N, mean=a+b*T, sd=1)
T_new <- seq(from=0, to=2.5, by=0.05)
data <- list(N=N, T=T, Y=Y, N_new=length(T_new), T_new=T_new)

# stanmodel <- stan_model(file='model2.stan')
stanmodel <- stan_model(file='model2-updated.stan')
fit <- sampling(stanmodel, data=data, iter=1000, warmup=200, chains=3)