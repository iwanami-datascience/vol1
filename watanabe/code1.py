import matplotlib.pyplot as plt 
import numpy as np
N=40
X=np.random.uniform(10,size=N)
Y=X*30+4+np.random.normal(0,16,size=N)
plt.plot(X,Y,"o")


import pymc as pm
import time
from pymc.backends.base import merge_traces

multicore=False 
saveimage=False

itenum=1000 
t0=time.clock() 
chainnum=3

with pm.Model() as model:
	alpha = pm.Normal('alpha', mu=0, sd=20)
	beta = pm.Normal('beta', mu=0, sd=20)
	sigma = pm.Uniform('sigma', lower=0)
	y = pm.Normal('y', mu=beta*X + alpha, sd=sigma, observed=Y)
	start = pm.find_MAP()
	step = pm.NUTS(state=start)

with model:
	if(multicore):
		trace = pm.sample(itenum, step, start=start,
					njobs=chainnum, random_seed=range(chainnum), progress_bar=False)
	else:
		ts=[pm.sample(itenum, step, chain=i, progressbar=False) for i in range(chainnum)] 
		trace=merge_traces(ts)
	if(saveimage): 
		pm.traceplot(trace).savefig("simple_linear_trace.png")
	print "Rhat="+str(pm.gelman_rubin(trace))
		
t1=time.clock()
print "elapsed time="+str(t1-t0)

#trace
if(not multicore):
	trace=ts[0] 
with model:
	pm.traceplot(trace,model.vars)

pm.forestplot(trace)

import pickle as pkl
with open("simplelinearregression_model.pkl","w") as fpw:
	pkl.dump(model,fpw)
with open("simplelinearregression_trace.pkl","w") as fpw:
	pkl.dump(trace,fpw)
with open("simplelinearregression_model.pkl") as fp: 
	model=plk.load(fp)
with open("simplelinearregression_trace.pkl") as fp: 
	trace=plk.load(fp)

