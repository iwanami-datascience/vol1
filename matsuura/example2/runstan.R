library(rstan)

d <- read.csv('input/data-season.txt', header=TRUE)
T <- nrow(d)
T_next <- 8
data <- list(T=T, T_next=T_next, Y=d$Y)

stanmodel <- stan_model(file='model.stan')
fit <- sampling(
   stanmodel, data=data, pars=c('mu_all','s_all','y_next','s_mu','s_s','s_r'),
   iter=10200, warmup=200, thin=10, chains=3,
   seed=123
)