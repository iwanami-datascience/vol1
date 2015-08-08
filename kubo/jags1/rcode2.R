load("data.RData") # データ d の読みこみ
N.beta <- 3 # beta[1], beta[2], beta[3]
list.data <- list( # データ
  mean.Y = d$mean.Y,
  X = d$X, Age = d$Age,
  N = nrow(d), N.beta = N.beta
)
# パラメーターの初期値
list.inits <- list(beta = rep(0, N.beta), sd = 1)

# 事後分布からのサンプリングの詳細
n.burnin <- 1000
n.chain <- 3
n.thin <- 10
n.iter <- n.thin * 3000

model <- jags.model(
  file = file.model, data = list.data,
  inits = list.inits, n.chain = n.chain
)
update(model, n.burnin) # burn in

# 推定結果を post.mcmc.list に格納する
post.mcmc.list <- coda.samples(
  model = model,
  variable.names = c("mu", names(list.inits)),
  n.iter = n.iter,
  thin = n.thin
)
