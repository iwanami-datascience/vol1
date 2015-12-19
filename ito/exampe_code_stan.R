X <- 1961:1990
Y <- c(4.71, 7.70, 7.97, 8.35, 5.70,
       7.33, 3.10, 4.98, 3.75, 3.35,
       1.84, 3.28, 2.77, 2.72, 2.54,
       3.23, 2.45, 1.90, 2.56, 2.12,
       1.78, 3.18, 2.64, 1.86, 1.69,
       0.81, 1.02, 1.40, 1.31, 1.57)

library(rstan)

model1 <-stan_model(file='nenrin1.stan')
fit1 <- sampling(model1,data = list(N = length(Y), y = Y),
                 iter = 3000, warmup=800, chains = 3,seed=71)
save(model1, file='model.rdata')
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
                                alpha = alpha,
                                a025 = alpha95[1,],
                                a975 = alpha95[2,]))
p2<-p2 + geom_line(aes(x = Year, y = alpha)) +
  geom_point(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = alpha), colour = "red") +
  geom_ribbon(aes(x = Year, ymin = a025, ymax = a975), fill = "red", alpha = 0.5) +
  theme_classic(base_family = base.family)+
  xlab("year") +  ylab("width (mm)") 

print(p2)

