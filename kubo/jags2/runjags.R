library(rjags)
library(R2WinBUGS) # to use write.model()

# JAGS MCMC の結果を格納している mcmc.list を bugs オブジェクトに変更する関数
source("http://hosho.ees.hokudai.ac.jp/~kubo/ce/r/mcmc.list2bugs.R")

model.bugs <- function()
{
	for (i in 1:2) { # age
		for (j in 1:N.pref) {
			mean.Y[i, j] ~ dnorm(mu[i, j], Tau.se[i, j])
			mu[i, j] <- (
				beta[1] + r[1, j]
				+ (beta[2] + r[2, j] + beta[3] * X[i, j]) * Age[i, j]
			)
		}
	}
	for (k in 1:N.beta) {
		beta[k] ~ dunif(-1.0E+4, 1.0E+4)
	}
	for (re in 1:N.r) {
		for (j in 1:N.pref) {
			r[re, j] ~ dnorm(0, tau[re])
		}
		tau[re] <- 1 / (sd[re] * sd[re])
		sd[re] ~ dunif(0, 1.0E+4)
	}
}
file.model <- "model.bug.txt"
write.model(model.bugs, file.model)

load("data.RData") # to read d
N.beta <- 3
N.r <- 2
N.pref <- length(unique(d$pref))
to2d <- function(v) t(matrix(v, N.pref, 2))
list.data <- list(
	mean.Y = to2d(d$mean.Y),
	Tau.se = to2d((sqrt(d$N) / d$sd.Y)^2),
	X = to2d(d$X), Age = to2d(d$Age), 
	N.beta = N.beta, N.r = N.r, N.pref = N.pref
)
list.inits <- list(
	beta = c(mean(d$mean.Y), 0, 0),
	r = matrix(rnorm(N.r * N.pref, 0, 1), N.r, N.pref),
	sd = rep(1, N.r)
)
n.burnin <- 1000
n.chain <- 3
n.thin <- 10
n.iter <- n.thin * 3000

model <- jags.model(
	file = file.model, data = list.data,
	inits = list.inits, n.chain = n.chain
)
update(model, n.burnin) # burn in
post.mcmc.list <- coda.samples(
	model = model,
	variable.names = names(list.inits),
	n.iter = n.iter,
	thin = n.thin
)
post.bugs <- mcmc.list2bugs(post.mcmc.list)
file <- "post.RData"
cat("# save to", file, "...\n")
save(post.mcmc.list, post.bugs, file = file)
