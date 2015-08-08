mport numpy as np
import pymc as pm

@pm.deterministic
def f(alpha,beta,X):
    return beta*X + alpha

alpha = pm.Normal('alpha', mu=0, tau=1/400)
beta  = pm.Normal('beta', mu=0, tau=1/400)
sigma = pm.Uniform('sigma', lower=0)

mu=f(alpha,beta,X)
Y = pm.Normal('y', mu=mu, tau=1/(sigma*sigma), observed=True)
model=pm.MCMC([alpha,beta,sigma,Y])

trace = pm.sample(itenum, step, start=start)

pm.traceplot(trace)
