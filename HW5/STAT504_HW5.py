from statsmodels.formula.api import ols
import numpy as np
import pandas as pd
np.random.seed(123)

'''
dt=pd.read_csv("C:/Users/niuzc/Desktop/model3.csv")
est=ols("y~d",data=dt).fit()
print(est.summary())
'''

'''
dt=pd.read_csv("C:/Users/niuzc/Desktop/tecator.csv")
col=list(dt.columns)
fml='fat~'+'+'.join(col[1:])
est=ols(formula=fml,data=dt).fit()
mse=np.mean(np.array(est.resid)**2)
print(mse)
'''


dt=pd.read_csv("C:/Users/niuzc/Desktop/tecator.csv")
set=np.random.randint(5,size=215)
mse=[]
col=list(dt.columns)
fml='fat~'+'+'.join(col[1:])
for i in range(5):
    train=set!=i
    test=set==i
    est=ols(formula=fml,data=dt[train]).fit()
    y_=est.predict(dt[test])
    mse.append(np.mean((np.array(dt[test]['fat'])-y_)**2))
print(np.mean(mse))
