'''import statsmodels.api as sm
import numpy as np
np.random.seed(123)

for lbd in range(0,11):
    n_of_sci=0
    n_of_rci=0
    for i in range(1000):
        X=[];Y=[]
        for j in range(100):
            x=np.random.normal(loc=0, scale=1)
            y=np.random.normal(loc=0.5+x, scale=1+lbd*x**2)
            X.append(x)
            Y.append(y)
        X=sm.add_constant(X)
        est=sm.OLS(Y,X).fit()
        sci=est.conf_int(alpha=0.05)[1]
        rci=est.get_robustcov_results(cov_type='HC0').conf_int(alpha=0.05)[1]
        if sci[0]<=1 and sci[1]>=1:
            n_of_sci+=1
        if rci[0]<=1 and rci[1]>=1:
            n_of_rci+=1
    p_of_sci=n_of_sci/1000
    p_of_rci=n_of_rci/1000
    print('lbd='+str(lbd)+', '+'p_of_sci='+str(p_of_sci)
    +', '+'p_of_rci='+str(p_of_rci))
        '''

'''
import numpy as np
import statsmodels.api as sm
import pyreadstat
df, meta = pyreadstat.read_dta('C:/Users/niuzc/Desktop/bm.dta')
x=df['black'].values.tolist()
m=df['female'].values.tolist()
n=df['yearsexp'].values.tolist()
y=df['call'].values.tolist()
X=[]
for i in range(0,len(x)):
    X.append([x[i],m[i],n[i]])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
print(est.params)
'''




growth=[2.4,2.89 ,.85 ,4.21 ,3.02 ,3.62 ,1.08 ,-.39 ,3.86 ,2.27 ,.38 ,1.04 ,2.36 ,1.72 ,.1 ,.95 ]
vote=[44.6 ,57.76 ,49.91 ,61.34 ,49.60 ,61.79 ,48.95 ,44.70 ,59.17 ,53.94 ,46.55 ,54.74 ,50.27 ,51.24 ,46.32 ,52.00 ]
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
x=growth
y=vote
plt.scatter(x,y)
x=sm.add_constant(x)
est=sm.OLS(y,x).fit()
#print(est.params)
plt.plot(np.array(x),est.params[0]+est.params[1]*np.array(x),label='ols')
sci=est.conf_int(alpha=0.05)
rci=est.get_robustcov_results(cov_type='HC0').conf_int(alpha=0.05)
plt.plot(np.array(x),sci[0][0]+sci[1][0]*np.array(x),label='sci_low')
plt.plot(np.array(x),sci[0][1]+sci[1][1]*np.array(x),label='sci_top')
plt.plot(np.array(x),rci[0][0]+rci[1][0]*np.array(x),label='rci_low')
plt.plot(np.array(x),rci[0][1]+rci[1][1]*np.array(x),label='rci_top')
plt.xlabel('growth')  
plt.ylabel('vote')  
plt.legend()
plt.show()

