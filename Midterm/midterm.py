from numpy.lib.function_base import append
import sensemakr as smkr
from sensemakr import bias_functions
import pandas as pd
import numpy as np
import statsmodels.api as sm
import random as rd
from sklearn.preprocessing import OneHotEncoder

darfur = smkr.load_darfur()
y=darfur['peacefactor'].values.tolist()
d=darfur['directlyharmed'].values.tolist()
f=darfur['female'].values.tolist()
encoder=OneHotEncoder()
reshaped=np.array(darfur['village']).reshape(-1, 1)
va=encoder.fit_transform(reshaped).toarray()
v=[]
for i in range(0,len(va)):
    v.append(list(va[i]))
xa=darfur[['herder_dar','farmer_dar','age','pastvoted']].values.tolist()
xa=sm.add_constant(xa)
x=[]
for i in range(0,len(xa)):
    x.append(list(xa[i]))

'''
X=[]
for i in range(0,len(d)):
    X.append([d[i]]+[f[i]]+v[i]+x[i])

est=sm.OLS(y,X).fit()
print(est.params[0])
'''

'''
X=[]
for i in range(0,len(y)):
    X.append([f[i]]+v[i]+x[i])
est1=sm.OLS(y,X).fit()
y_=[]
for i in range(0,len(y)):
    y_.append(np.array(y[i])-(np.array(est1.params)*np.array(X[i])).sum())

est2=sm.OLS(d,X).fit()
d_=[]
for i in range(0,len(d)):
    d_.append(np.array(d[i])-(np.array(est2.params)*np.array(X[i])).sum())

est3=sm.OLS(y_,d_).fit()
print(est3.params)
'''

'''
rd.seed(123)
b=1000
n=len(y)
ATE=[]
for i in range(0,b):
    dumX=[]
    dumy=[]
    for j in range(0,n):
        ind=rd.randint(0,n-1)
        dumX.append([d[ind]]+[f[ind]]+v[ind]+x[ind])
        dumy.append(y[ind])
    est=sm.OLS(dumy,dumX).fit()
    ATE.append(est.params[0])
ATEmean=np.mean(ATE)
ATEsd=np.std(ATE,ddof=1)
ATEq1=np.percentile(ATE,2.5)
ATEq2=np.percentile(ATE,97.5)
ATE_CI_1=[ATEmean-1.96*ATEsd,ATEmean+1.96*ATEsd]
ATE_CI_2=[ATEq1,ATEq2]
print(ATE_CI_1)
print(ATE_CI_2)
'''


'''
X=[]
for i in range(0,len(y)):
    X.append([d[i]]+[f[i]]+[d[i]*f[i]]+v[i]+x[i])
est=sm.OLS(y,X).fit()
Ef=np.mean(f)
ATE=est.params[0]+est.params[2]*Ef
print(ATE)
'''


'''
rd.seed(123)
b=1000
n=len(y)
ATE=[]
for i in range(0,b):
    dumX=[]
    dumy=[]
    dumf=[]
    for j in range(0,n):
        ind=rd.randint(0,n-1)
        dumX.append([d[ind]]+[f[ind]]+[d[ind]*f[ind]]+v[ind]+x[ind])
        dumy.append(y[ind])
        dumf.append(f[ind])
    est=sm.OLS(dumy,dumX).fit()
    Ef=np.mean(dumf)
    ATE.append(est.params[0]+est.params[2]*Ef)
ATEq1=np.percentile(ATE,2.5)
ATEq2=np.percentile(ATE,97.5)
ATE_CI=[ATEq1,ATEq2]
print(ATE_CI)
'''


'''
rd.seed(123)
b=1000
n=len(y)
tau=[]
for i in range(0,b):
    dumX=[]
    dumy=[]
    for j in range(0,n):
        ind=rd.randint(0,n-1)
        dumX.append([d[ind]]+[f[ind]]+v[ind]+x[ind])
        dumy.append(y[ind])
    est=sm.OLS(dumy,dumX).fit()
    tau.append(est.params[0]-0.2*0.2)
tauq1=np.percentile(tau,2.5)
tauq2=np.percentile(tau,97.5)
tau_CI=[tauq1,tauq2]
print(tau_CI)
'''


'''
X1=[]
for i in range(0,len(d)):
    X1.append([d[i]]+[f[i]]+v[i]+x[i])
est1=sm.OLS(y,X1).fit()
y_=[]
for i in range(0,len(y)):
    y_.append(np.array(y[i])-(np.array(est1.params)*np.array(X1[i])).sum())
SD_Y_DW=np.std(y_,ddof=1)

X2=[]
for i in range(0,len(d)):
    X2.append([f[i]]+v[i]+x[i])
est2=sm.OLS(d,X2).fit()
d_=[]
for i in range(0,len(d)):
    d_.append(np.array(d[i])-(np.array(est2.params)*np.array(X2[i])).sum())
SD_D_W=np.std(d_,ddof=1)

bias=(((0.12*0.01)/(1-0.01))**0.5)*(SD_Y_DW/SD_D_W)
print(bias)
tau=est1.params[0]-bias
print(tau)
'''

import statsmodels.formula.api as smf
model = smf.ols(formula='peacefactor ~ directlyharmed + age + farmer_dar + herder_dar + pastvoted + female + village', data=darfur)
fitted_model = model.fit()
ae=bias_functions.adjusted_estimate(model = fitted_model, treatment = "directlyharmed", r2dz_x = 0.01, r2yz_dx = 0.12)  
print(ae)


'''
rd.seed(123)
b=1000
n=len(y)
tau=[]
for i in range(0,b):
    dumX1=[]
    dumX2=[]
    dumd=[]
    dumy=[]
    for j in range(0,n):
        ind=rd.randint(0,n-1)
        dumX1.append([d[ind]]+[f[ind]]+v[ind]+x[ind])
        dumX2.append([f[ind]]+v[ind]+x[ind])
        dumd.append(d[ind])
        dumy.append(y[ind])
    est1=sm.OLS(dumy,dumX1).fit()
    est2=sm.OLS(dumd,dumX2).fit()
    y_=[]
    for i in range(0,len(dumy)):
        y_.append(np.array(dumy[i])-(np.array(est1.params)*np.array(dumX1[i])).sum())
    SD_Y_DW=np.std(y_,ddof=1)
    d_=[]
    for i in range(0,len(dumd)):
        d_.append(np.array(dumd[i])-(np.array(est2.params)*np.array(dumX2[i])).sum())
    SD_D_W=np.std(d_,ddof=1)
    bias=(((0.12*0.01)/(1-0.01))**0.5)*(SD_Y_DW/SD_D_W)
    tau.append(est1.params[0]-bias)
tauq1=np.percentile(tau,2.5)
tauq2=np.percentile(tau,97.5)
tau_CI=[tauq1,tauq2]
print(tau_CI)
'''