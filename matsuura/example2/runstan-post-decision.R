la <- rstan::extract(fit)
x_smp <- la$y_next[,1]

loss_function <- function(x){
   sum(ifelse(x < x_smp, 2*(x_smp-x), 1-exp(-(x-x_smp))))
}

y_decision <- optim(median(x_smp), loss_function, method='Brent', lower=5, upper=50)$par