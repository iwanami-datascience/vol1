import matplotlib.pyplot as plt 
import numpy as np

import pandas as pd
data=pd.read_csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv")

#種子の数yごとに集計、グラフとして表示すると
plt.bar(range(9),data.groupby('y').sum().id)
data.groupby('y').sum().T

#dataの制限
Y=np.array(data.y)[:6]

import numpy as np
import pymc as pm import theano.tensor as T 
def invlogit(v):
	return T.exp(v)/(T.exp(v)+1)

with pm.Model() as model_hier: 
	s=pm.Uniform('s',0,1.0E+2) 
	beta=pm.Normal('beta',0,1.0E+2)
	r=pm.Normal('r',0,s,shape=len(Y)) 
	q=invlogit(beta+r) 
	y=pm.Binomial('y',8,q,observed=Y)
	
	step = pm.Slice([s,beta,r])
	trace_hier = pm.sample(10000, step)

with model_hier:
	pm.traceplot(trace_hier, model_hier.vars)
