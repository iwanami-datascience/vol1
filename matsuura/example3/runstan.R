library(rstan)

m <- as.matrix(read.csv('input/data-plate.txt', header=FALSE))
Ni <- 16
Nj <- 24
T  <- 96
rownames(m) <- 1:Ni
colnames(m) <- 1:Nj
d_melt <- reshape2::melt(m)
colnames(d_melt) <- c('Row','Column','Y')
d_design <- read.csv('input/data-plate-design.txt', header=FALSE)

data <- list(Ni=Ni, Nj=Nj, Y=m, T=T, T_index=d_design)
loess_res <- loess(d_melt$Y ~ d_melt$Row + d_melt$Column, span=0.1)
smoothed <- matrix(loess_res$fitted, nrow=Ni, ncol=Nj, byrow=FALSE)

stanmodel <- stan_model(file='model.stan')
fit <- sampling(
   stanmodel,
   data=data,
   init=function() {
      list(r=smoothed, s_r=1, s_y=1, s_beta=1, beta=rnorm(T, mean=0, sd=0.1))
   },
   iter=5200, warmup=200, thin=5,
   seed=1234, chains=3
)