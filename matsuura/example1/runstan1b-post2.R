la <- rstan::extract(fit)
b_smp <- la$b
quantile(b_smp, prob=c(0.025, 0.975))