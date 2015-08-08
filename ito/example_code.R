##
## 岩波データサイエンス1
##

##
## 年輪幅のモデリング
##

## 年と年輪幅(mm)
X <- 1961:1990
Y <- c(4.71, 7.70, 7.97, 8.35, 5.70,
       7.33, 3.10, 4.98, 3.75, 3.35,
       1.84, 3.28, 2.77, 2.72, 2.54,
       3.23, 2.45, 1.90, 2.56, 2.12,
       1.78, 3.18, 2.64, 1.86, 1.69,
       0.81, 1.02, 1.40, 1.31, 1.57)

## JAGSによる状態空間モデルのパラメーター推定
library(rjags)

## ローカルレベルモデル
## 初期値
inits <- list()
inits[[1]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123,
                   alpha = rep(1, length(Y)), sigma = c(0.5, 1))
inits[[2]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 1234,
                   alpha = rep(2, length(Y)), sigma = c(1, 1.5))
inits[[3]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 12345,
                   alpha = rep(3, length(Y)), sigma = c(1.5, 0.1))
inits[[4]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123456,
                   alpha = rep(4, length(Y)), sigma = c(0.1, 0.5))

model1 <- jags.model("nenrin1.bug.txt",
                     data = list(N = length(Y), y = Y),
                     inits = inits,
                     n.chains = 4, n.adapt = 3000)
fit1 <- coda.samples(model1,
                     variable.names = c("alpha", "sigma"),
                     n.iter = 3000, thin = 3)
gelman.diag(fit1)
summary1 <- summary(fit1)
print(summary1)

## トレンドモデル
inits <- list()
inits[[1]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123,
                   alpha = rep(9, length(Y)), sigma = c(2, 1))
inits[[2]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 1234,
                   alpha = rep(8, length(Y)), sigma = c(1, 1.5))
inits[[3]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 12345,
                   alpha = rep(7, length(Y)), sigma = c(1.5, 0.1))
inits[[4]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123456,
                   alpha = rep(6, length(Y)), sigma = c(3, 0.5))

model2 <- jags.model("nenrin2.bug.txt",
                     data = list(N = length(Y), y = Y),
                     inits = inits,
                     n.chains = 4, n.adapt = 6000)
fit2 <- coda.samples(model2,
                     variable.names = c("alpha", "sigma"),
                     n.iter = 10000, thin = 10)
gelman.diag(fit2)
summary2 <- summary(fit2)

## 対数正規分布
model3 <- jags.model("nenrin3.bug.txt",
                     data = list(N = length(Y), y = Y),
                     n.chains = 4, n.adapt = 5000)
fit3 <- coda.samples(model3,
                     variable.names = c("alpha", "sigma"),
                     n.iter = 70000, thin = 70)
gelman.diag(fit3)
summary3 <- summary(fit3)

## グラフ
library(ggplot2)

## Macではフォントにヒラギノ角ゴシックProN W3を指定
if (Sys.info()["sysname"] == "Darwin") { # OS X
  base.family <- "HiraKakuProN-W3"
} else {
  base.family <- ""
}
ribbon.alpha <- 0.3
ribbon.fill <- "black"

## 図2
p1 <- ggplot(data.frame(Year = X, Width = Y)) +
  geom_point(aes(x = Year, y = Width)) +
  geom_line(aes(x = Year, y = Width)) +
  xlab("年") +
  ylab("年輪幅 (mm)") +
  ylim(0, 10) +    
  theme_classic(base_family = base.family)
print(p1)

## 図3
alpha.range <- match("alpha[1]", varnames(fit1)):match("alpha[30]", varnames(fit1))
alpha.mean <- summary1$statistics[alpha.range, "Mean"]
alpha.up <- summary1$quantiles[alpha.range, "97.5%"]
alpha.low <- summary1$quantiles[alpha.range, "2.5%"]

p2 <- ggplot(data.frame(Year = X, Width = Y,
                        Width.mean = alpha.mean,
                        Width.up = alpha.up,
                        Width.low = alpha.low)) +
  geom_ribbon(aes(x = Year, ymax = Width.up, ymin = Width.low),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_line(aes(x = Year, y = Width.mean), size = 1) +
  geom_line(aes(x = Year, y = Width)) +
  geom_point(aes(x = Year, y = Width), size = 2) +
  xlab("年") +
  ylab("年輪幅 (mm)") +
  ylim(0, 10) +    
  theme_classic(base_family = base.family)
print(p2)

## 図4
alpha.range <- match("alpha[1]", varnames(fit2)):match("alpha[30]", varnames(fit2))
alpha.mean <- summary2$statistics[alpha.range, "Mean"]
alpha.up <- summary2$quantiles[alpha.range, "97.5%"]
alpha.low <- summary2$quantiles[alpha.range, "2.5%"]

p3 <- ggplot(data.frame(Year = X, Width = Y,
                        Width.mean = alpha.mean,
                        Width.up = alpha.up,
                        Width.low = alpha.low)) +
  geom_ribbon(aes(x = Year, ymax = Width.up, ymin = Width.low),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_line(aes(x = Year, y = Width.mean), size = 1) +
  geom_line(aes(x = Year, y = Width)) +
  geom_point(aes(x = Year, y = Width), size = 2) +
  xlab("年") +
  ylab("年輪幅 (mm)") +
  ylim(-0.1, 10) +    
  theme_classic(base_family = base.family)
print(p3)

## 図5
alpha.range <- match("alpha[1]", varnames(fit3)):match("alpha[30]", varnames(fit3))
alpha.mean <- summary3$statistics[alpha.range, "Mean"]
alpha.up <- summary3$quantiles[alpha.range, "97.5%"]
alpha.low <- summary3$quantiles[alpha.range, "2.5%"]

p4 <- ggplot(data.frame(Year = X, Width = Y,
                        Width.mean = exp(alpha.mean),
                        Width.up = exp(alpha.up),
                        Width.low = exp(alpha.low))) +
  geom_ribbon(aes(x = Year, ymax = Width.up, ymin = Width.low),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_line(aes(x = Year, y = Width.mean), size = 1) +
  geom_line(aes(x = Year, y = Width)) +
  geom_point(aes(x = Year, y = Width), size = 2) +
  xlab("年") + ylab("年輪幅 (mm)") +
  ylim(0, 10) +    
  theme_classic(base_family = base.family)
print(p4)


##
## 発見率を考慮したモデル
##

## データ生成
set.seed(31415)

nt <- 50        # 観測回数
nr <- 5         # 1回の観測時にカウントする回数
p <- 0.6        # 発見確率
log.lambda <- numeric(nt)
log.lambda[1] <- log(30)        # 初期値
for (t in 2:nt) {
  log.lambda[t] <- log.lambda[t - 1] + rnorm(1, 0, 0.1)
}

## 真の個体数の生成
N <- rpois(nt, exp(log.lambda))

## 観測値の生成
Nobs <- sapply(1:nt, function(t) rbinom(nr, N[t], p))

##
library(rjags)

inits <- list(list(p = 0.1, N = rep(100, nt)),
              list(p = 0.3, N = rep(100, nt)),
              list(p = 0.5, N = rep(100, nt)),
              list(p = 0.7, N = rep(100, nt)))

model <- jags.model("N.bug.txt",
                    data = list(nt = nt, nr = nr, Nobs = t(Nobs)),
                    inits = inits,
                    n.chains = 4, n.adapt = 1000)
fit <- coda.samples(model,
                    variable.names = c("lambda", "N", "p", "sigma"),
                    n.iter = 10000, thin = 10)
gelman.diag(fit)
summary(fit)

## グラフ
library(ggplot2)

if (Sys.info()["sysname"] == "Darwin") { # OS X
  base.family <- "HiraKakuProN-W3"
} else {
  base.family <- ""
}
point.alpha <- 0.5
ribbon.alpha <- 0.3
ribbon.fill <- "black"

## 「真の個体数の期待値」と「真の個体数」と「観測値」
p1 <- ggplot(data.frame(Time = 1:nt, N = N, Nexp = exp(log.lambda))) +
  geom_line(aes(x = Time, y = N)) +
  geom_line(aes(x = Time, y = Nexp), linetype = 2) +
  geom_point(data = data.frame(Time = rep(1:nt, each = nr),
                               Nobs = c(Nobs)),
             aes(x = Time, y = Nobs),
             alpha = point.alpha)
print(p1)

## 図6
pos <- match("lambda[1]", varnames(fit))
post.lambda <- summary(fit[, pos:(pos + nt - 1)])
lambda <- data.frame(t = 1:nt,
                     orig = exp(log.lambda),
                     mean = post.lambda$statistics[, "Mean"],
                     upper = post.lambda$quantiles[, "97.5%"],
                     lower = post.lambda$quantiles[, "2.5%"])

p2 <- ggplot(data.frame(Time = 1:nt, N = N)) +
  geom_ribbon(data = lambda, aes(x = t, ymin = lower, ymax = upper),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_line(data = lambda, aes(x = t, y = mean), size = 1) +
  geom_line(aes(x = Time, y = N)) +
  geom_point(data = data.frame(Time = rep(1:nt, each = nr),
                               Nobs = c(Nobs)),
             aes(x = Time, y = Nobs),
             alpha = point.alpha) +
  xlab("時間") + ylab("個体数") +
  theme_classic(base_family = base.family)
print(p2)



##
## CARモデル
##

## 久保(1992)のデータ
## http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/spatial/Y.RData
load("Y.RData")

## GLMM
library(lme4)
group <- as.factor(seq_along(Y))
fit1 <- glmer(Y ~ 1 + (1|group), family = "poisson")
Y.fit1 <- exp(fixef(fit1) + ranef(fit1)$group[, 1])

## CARBayes
library(CARBayes)

## 隣接行列の作成
n <- length(Y)
W <- matrix(0, nrow = n, ncol = n)
for (i in 2:n) {
  W[i, i - 1] <- W[i - 1, i] <- 1
}

## 擬似乱数系列の指定
set.seed(1234)

## あてはめ
fit2 <- S.CARiar(Y ~ 1, family = "poisson", W = W,
                 burnin = 2000, n.sample = 32000, thin = 10)

## 結果表示
print(fit2)
summarise.samples(fit2$samples$beta,
                  quantiles = c(0.025, 0.5, 0.975))
summarise.samples(fit2$samples$phi,
                  quantiles = c(0.025, 0.5, 0.975))
## 収束診断
library(coda)
geweke.diag(fit2$samples$beta)
geweke.diag(fit2$samples$phi)

plot(fit2$samples$beta, las = 1)
plot(fit2$samples$phi[,49], las = 1)

## OpenBUGS
library(R2OpenBUGS)

adj <- c(2, c(sapply(2:49, function(i) c(i - 1, i + 1))), 49)
weights <- rep(1, length(adj))
num <- c(1, rep(2, 48), 1)

## bugs()関数に渡す引数は、各自の環境に応じて設定する。
os.type <- Sys.info()["sysname"]
if (os.type == "Darwin") { # OS X
  useWINE <- TRUE
  WINE <- "/Applications/Wine.app/Contents/Resources/bin/wine"
  WINEPATH <- "/Applications/Wine.app/Contents/Resources/bin/winepath"
  OpenBUGS <- paste(Sys.getenv("HOME"),
                    ".wine/drive_c/Program Files", 
                    "OpenBUGS/OpenBUGS323/OpenBUGS.exe", sep = "/")
} else {
  useWINE <- FALSE
  WINE <- NULL
  WINEPATH <- NULL
  OpenBUGS <- NULL
}
inits <- list(list(S = rep(0, 50), beta = 4, sigma = 1),
              list(S = rep(0, 50), beta = 2, sigma = 2),
              list(S = rep(0, 50), beta = 1, sigma = 4))

fit.bugs <- bugs(model.file = "CAR_bug.txt",
                 data = list(N = length(Y), Y = Y,
                             adj = adj, weights = weights, num = num),
                 inits = inits,
                 parameters.to.save = c("S", "beta", "sigma"),
                 n.chains = 3,
                 n.iter = 21000, n.burnin = 1000, n.thin = 20,
                 OpenBUGS.pgm = OpenBUGS,
                 debug = FALSE,
                 useWINE = useWINE,
                 WINE = WINE,
                 WINEPATH = WINEPATH)
print(fit.bugs)

## 状態空間モデルによるCAR
library(rjags)

## 初期値
inits <- list()
inits[[1]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123,
                   alpha = rep(1, length(Y)), sigma = 0.5)
inits[[2]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 1234,
                   alpha = rep(2, length(Y)), sigma = 1)
inits[[3]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 12345,
                   alpha = rep(3, length(Y)), sigma = 1.5)
inits[[4]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123456,
                   alpha = rep(4, length(Y)), sigma = 0.1)

model <- jags.model("CAR_ssm_bug.txt",
                     data = list(N = length(Y), y = Y),
                     inits = inits,
                     n.chains = 4, n.adapt = 3000)
fit <- coda.samples(model,
                     variable.names = c("alpha", "lambda", "sigma"),
                     n.iter = 10000, thin = 10)
gelman.diag(fit)
summary <- summary(fit)
print(summary)

## 2次元のCARBayes
library(CARBayes)
library(coda)
library(ggplot2)

data <- read.csv("Qglauca.csv")
set.seed(1234)

## 隣接行列
n.x <- length(levels(as.factor(data$X)))
n.y <- length(levels(as.factor(data$Y)))
n.sites <- n.x * n.y
W <- matrix(0, nrow = n.sites, ncol = n.sites)
for (x in 0:(n.x - 1)) {
  for (y in 1:n.y) {
    if (x > 0)       W[x * n.y + y, (x - 1) * n.y + y] <- 1
    if (x < n.x - 1) W[x * n.y + y, (x + 1) * n.y + y] <- 1
    if (y > 1)       W[x * n.y + y, x * n.y + y - 1] <- 1
    if (y < n.y)     W[x * n.y + y, x * n.y + y + 1] <- 1
  }
}

## Intrinsic Conditional Autoregressive model
fit.iar <- S.CARiar(N ~ 1, family = "poisson", data = data, W = W,
                    burnin = 2000, n.sample = 32000, thin = 10)
geweke.diag(fit.iar$samples$beta)
plot(fit.iar$samples$beta)

y.est <- apply(fit.iar$samples$fitted, 2, quantile,
               probs = c(0.025, 0.5, 0.975))
df.iar <- data.frame(x = 1:n.sites,
                     y = data$N,
                     ymed = y.est[2, ],
                     ymax = y.est[1, ],
                     ymin = y.est[3, ])
# 観測値と、事後分布の中央値、95%信用区間
p.iar <- ggplot(df.iar) +
    geom_point(aes(x = x, y = y)) +
    geom_pointrange(aes(x = x, y = ymed, ymax = ymax, ymin = ymin),
                    colour = "red", alpha = 0.3, shape = 3) +
    xlab("Index") + ylab("N") + ylim(0, 9)
print(p.iar)


## グラフ
library(ggplot2)

if (Sys.info()["sysname"] == "Darwin") { # OS X
  base.family <- "HiraKakuProN-W3"
} else {
  base.family <- ""
}
point.alpha <- 0.5
ribbon.alpha <- 0.3
ribbon.fill <- "black"

## 図7
library(ggplot2)
df <- data.frame(j = seq_along(Y), Y = Y, Yhat = Y.fit1)
p1 <- ggplot(df) +
  geom_point(aes(x = j, y = Y), size = 2.5) +
  geom_line(aes(x = j, y = Yhat), size = 0.5) +
  xlab("位置") + ylab("個体数") +
  ylim(0, 25) +
  theme_classic(base_family = base.family)
print(p1)

## 図8
Y.fit <- apply(fit2$samples$fitted, 2, mean)
Y.ci <- apply(fit2$samples$fitted, 2, quantile, c(0.025, 0.975))

df2 <- data.frame(j = seq_along(Y), Y = Y, Yhat = Y.fit,
                  Yupper = Y.ci["97.5%", ], Ylower = Y.ci["2.5%", ])
p2 <- ggplot(df2) +
  geom_ribbon(aes(x = j, ymax = Yupper, ymin = Ylower),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_point(aes(x = j, y = Y), size = 2.5) +
  geom_line(aes(x = j, y = Yhat), size = 0.5) +
  xlab("位置") + ylab("個体数") +
  ylim(0, 25) +
  theme_classic(base_family = base.family)
print(p2)

## 図9
pos.alpha1 <- match("lambda[1]", rownames(summary$statistics))
pos.alpha50 <- match("lambda[50]", rownames(summary$statistics))
Y.fit <- summary$statistics[pos.alpha1:pos.alpha50, "Mean"]
Y.ci <- summary$quantiles[pos.alpha1:pos.alpha50, c("2.5%", "97.5%")]

df3 <- data.frame(j = seq_along(Y), Y = Y, Yhat = Y.fit,
                  Yupper = Y.ci[, "97.5%"], Ylower = Y.ci[, "2.5%"])
p3 <- ggplot(df3) +
  geom_ribbon(aes(x = j, ymax = Yupper, ymin = Ylower),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_point(aes(x = j, y = Y), size = 2.5) +
  geom_line(aes(x = j, y = Yhat), size = 0.5) +
  xlab("位置") + ylab("個体数") +
  ylim(0, 25) +
  theme_classic(base_family = base.family)
print(p3)

## 図10
p.map <- ggplot(data) +
  scale_y_reverse(breaks = c(0, 5, 10)) +
  coord_fixed(ratio = 1) +
  geom_tile(aes(x = X, y = Y, fill = N)) +
  xlab("株数X") + ylab("株数Y") +
  scale_fill_gradient(name = "株数", low = "grey90", high = "grey10") +
  theme_bw(base_family = base.family) %+replace%
  theme(legend.title = element_text(family = base.family))
print(p.map)

## 図11
p.map.iar <- ggplot(df.iar) +
  scale_y_reverse(breaks = c(0, 5, 10)) +
  coord_fixed(ratio = 1) +
  geom_tile(aes(x = data[x, ]$X,
                y = data[x, ]$Y, fill = ymed)) +
  xlab("X") + ylab("Y") +
  scale_fill_gradient(name = "株数", low = "grey90", high = "grey10") +
  theme_bw(base_family = base.family) %+replace%
  theme(legend.title = element_text(family = base.family))
print(p.map.iar)

  