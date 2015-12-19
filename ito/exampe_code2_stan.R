X <- 1961:1990
Y <- read.csv("nenrin.data",header=F)[[1]]

library(rstan)

model2 <-stan_model(file='nenrin2.stan')
fit1 <- sampling(model2, data = list(N = length(Y), y = Y),
                 iter = 3000, warmup=800, chains = 3,seed=71)
save(model2, file='mode2.rdata')

print(fit1, digit=2)
summary1 <- summary(fit1)
#print(summary1)

library(ggplot2)

if (Sys.info()["sysname"] == "Darwin") { # OS X
  base.family <- "HiraKakuProN-W3"
} else {
  base.family <- ""
}

ribbon.alpha <- 0.3
ribbon.fill <- "black"

p1 <- ggplot(data.frame(Year = X, Width = Y)) +
  geom_point(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = Width)) +
  xlab("year") +
  ylab("width (mm)") +
  ylim(0, 10) +    
  theme_classic(base_family = base.family)
print(p1)

alpha <- get_posterior_mean(fit1, "alpha")[, "mean-all chains"]
alpha95 <- apply(extract(fit1, "alpha")$alpha, 2, quantile, c(0.025, 0.975))

p2 <- ggplot(data = data.frame(Year = X,
                               Width =Y,
                               alpha = alpha,
                               a025 = alpha95[1,],
                               a975 = alpha95[2,]))
p2<-p2 + geom_line(aes(x = Year, y = alpha)) +
  geom_point(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = alpha), colour = "blue") +
  geom_ribbon(aes(x = Year, ymin = a025, ymax = a975), fill = "blue", alpha = 0.5) +
  theme_classic(base_family = base.family)+
  xlab("year") +  ylab("width (mm)") 

print(p2)