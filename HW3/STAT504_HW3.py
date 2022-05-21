import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.api as sm
import random as rd

dt=pd.read_csv("C:/Users/niuzc/Desktop/qog_jan16.csv",index_col=0)


'''
x1=dt[['epi_watsup']]
y1=dt[['wdi_mortinftot']]
plt.scatter(x1,y1)
plt.xlabel('epi_watsup')  
plt.ylabel('wdi_mortinftot')  
plt.show()
'''


'''
dt=pd.read_csv("C:/Users/niuzc/Desktop/qog_jan16.csv",index_col=0)
est=ols("wdi_mortinftot ~ epi_watsup", data=dt).fit()
print(est.params)

x=np.array(dt['epi_watsup'])
y=np.array(dt['wdi_mortinftot'])
plt.scatter(x,y)
plt.plot(x,-0.601256*x+59.583229,color='r')
plt.xlabel('epi_watsup')  
plt.ylabel('wdi_mortinftot')  
plt.show()
'''

'''
x=dt['epi_watsup'].values.tolist()
y=dt['wdi_mortinftot'].values.tolist()
b=10000
n=len(x)
alpha=[]
beta=[]
for i in range(0,b):
    dumx=[]
    dumy=[]
    for j in range(0,n):
        index=rd.randint(0,n-1)
        dumx.append(x[index])
        dumy.append(y[index])
    dumx = sm.add_constant(dumx)
    est=sm.OLS(dumy,dumx).fit()
    alpha.append(est.params[0])
    beta.append(est.params[1])
amean=np.mean(alpha)
bmean=np.mean(beta)
asd=np.std(alpha,ddof=1)
bsd=np.std(beta,ddof=1)
Ca=[amean-1.96*asd,amean+1.96*asd]
Cb=[bmean-1.96*bsd,bmean+1.96*bsd]
print(Ca)
print(Cb)
'''

'''
x=dt['epi_watsup'].values.tolist()
y=dt['wdi_mortinftot'].values.tolist()
z=dt['wdi_accelectr'].values.tolist()
fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.scatter(x, z, y)
ax.set_xlabel('X')
ax.set_ylabel('Z')
ax.set_zlabel('Y')

X=[]
for i in range(0,len(x)):
    X.append([x[i],z[i]])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
print(est.params)
a=est.params[0]
b=est.params[1]
c=est.params[2]
xx=np.linspace(min(x),max(x))
zz=np.linspace(min(z),max(z))
XX, ZZ = np.meshgrid(xx,zz)
YY=a+b*XX+c*ZZ
ax.plot_surface(XX,ZZ,YY) 
plt.show()
'''

'''
x=dt['epi_watsup'].values.tolist()
y=dt['wdi_mortinftot'].values.tolist()
z=dt['wdi_accelectr'].values.tolist()
z1 = sm.add_constant(z)
est1=sm.OLS(x,z1).fit()
x_z=list(np.array(x)-est1.params[0]-est1.params[1]*np.array(z))
est2=sm.OLS(y,z1).fit()
y_z=list(np.array(y)-est2.params[0]-est2.params[1]*np.array(z))

plt.scatter(x_z,y_z)
plt.xlabel('x_z')  
plt.ylabel('y_z')  

x_z1=sm.add_constant(x_z)
est3=sm.OLS(y_z,x_z1).fit()
print(est3.params)
plt.plot(x_z,est3.params[0]+est3.params[1]*np.array(x_z),color='r')

plt.show()
'''

'''
x=dt['epi_watsup'].values.tolist()
y=dt['wdi_mortinftot'].values.tolist()
z=dt['wdi_accelectr'].values.tolist()
x1=sm.add_constant(x)
est=sm.OLS(z,x1).fit()
print(est.params)
'''

#2.2---------------------------------------------

'''
x=np.array(dt['epi_watsup'])
y=dt['wdi_mortinftot'].values.tolist()
x_2=x**2
X=[]
for i in range(0,len(x)):
    X.append([x[i],x_2[i]])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
print(est.params)
a=est.params[0]
b=est.params[1]
c=est.params[2]
plt.scatter(x,y)
x_ols=np.array(range(0,101))
y_ols=a+b*x_ols+c*x_ols**2
plt.plot(x_ols,y_ols,color='r')
plt.xlabel('epi_watsup')  
plt.ylabel('wdi_mortinftot')  
plt.show()
'''

'''
x=dt['epi_watsup'].values.tolist()
xmean=np.mean(x)
print(xmean)
'''

'''
x=dt['epi_watsup'].values.tolist()
y=dt['wdi_mortinftot'].values.tolist()
B=10000
n=len(x)
a=[]
b=[]
c=[]
Ex=[]
for i in range(0,B):
    dumx=[]
    dumy=[]
    for j in range(0,n):
        index=rd.randint(0,n-1)
        dumx.append(x[index])
        dumy.append(y[index])
    Ex.append(np.mean(dumx))
    dumx_2=list(np.array(dumx)**2) 
    X=[]
    for k in range(0,len(dumx)):
        X.append([dumx[k],dumx_2[k]])
    X=sm.add_constant(X)
    est=sm.OLS(dumy,X).fit()
    a.append(est.params[0])
    b.append(est.params[1])
    c.append(est.params[2])

APD=np.array(b)+2*np.array(c)*np.array(Ex)
meanAPD=np.mean(APD)
sdAPD=np.std(APD,ddof=1)
C=[meanAPD-1.96*sdAPD,meanAPD+1.96*sdAPD]
print(C)
'''

'''
x=np.array(dt['epi_watsup'])
y=dt['wdi_mortinftot'].values.tolist()
x_2=x**2
X=[]
for i in range(0,len(x)):
    X.append([x[i],x_2[i]])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
a=est.params[0]
b=est.params[1]
c=est.params[2]
ADPsum=((b*(x+0.0001)+c*(x+0.0001)**2)-(b*(x-0.0001)+c*(x-0.0001)**2))/0.0002
ADP=np.mean(ADPsum)
print(ADP)
'''

'''
x=np.array(dt['epi_watsup'])
y=np.array(dt['wdi_mortinftot'])
z=np.array(dt['wdi_accelectr'])
fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.scatter(x, z, y)
ax.set_xlabel('X')
ax.set_ylabel('Z')
ax.set_zlabel('Y')

X=[]
for i in range(0,len(x)):
    X.append([x[i],x[i]**2,z[i],z[i]**2])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
print(est.params)
a=est.params[0]
b=est.params[1]
c=est.params[2]
d=est.params[3]
e=est.params[4]
xx=np.linspace(min(x),max(x))
zz=np.linspace(min(z),max(z))
XX, ZZ = np.meshgrid(xx,zz)
YY=a+b*XX+c*XX**2+d*ZZ+e*ZZ**2
ax.plot_surface(XX,ZZ,YY,color='y') 
plt.show()
'''

'''
x=np.array(dt['epi_watsup'])
y=np.array(dt['wdi_mortinftot'])
z=np.array(dt['wdi_accelectr'])
X=[]
for i in range(0,len(x)):
    X.append([x[i],x[i]**2,z[i],z[i]**2])
X=sm.add_constant(X)
est=sm.OLS(y,X).fit()
a=est.params[0]
b=est.params[1]
c=est.params[2]
d=est.params[3]
e=est.params[4]
APDsum=((b*(x+0.0001)+c*(x+0.0001)**2+d*z+e*z**2)-(b*(x-0.0001)+c*(x-0.0001)**2+d*z+e*z**2))/0.0002
APD=np.mean(APDsum)
print(APD)
'''


x=np.array(dt['epi_watsup'])
y=np.array(dt['wdi_mortinftot'])
z=np.array(dt['wdi_accelectr'])
B=10000
n=len(x)
APD=[]
for i in range(0,B):
    dumx=[]
    dumz=[]
    dumy=[]
    for j in range(0,n):
        index=rd.randint(0,n-1)
        dumx.append(x[index])
        dumz.append(z[index])
        dumy.append(y[index])
    dumx_2=list(np.array(dumx)**2)
    dumz_2=list(np.array(dumz)**2)
    dumX=[]
    for i in range(0,len(dumx)):
        dumX.append([dumx[i],dumx_2[i],dumz[i],dumz_2[i]])
    dumX=sm.add_constant(dumX)
    est=sm.OLS(dumy,dumX).fit()
    a,b,c,d,e=est.params
    dx,dz,dy=np.array(dumx),np.array(dumz),np.array(dumy)
    dumAPDsum=((b*(dx+0.0001)+c*(dx+0.0001)**2+d*dz+e*dz**2)-(b*(dx-0.0001)+c*(dx-0.0001)**2+d*dz+e*dz**2))/0.0002
    dumAPD=np.mean(dumAPDsum)
    APD.append(dumAPD)

meanAPD=np.mean(APD)
sdAPD=np.std(APD,ddof=1)
C=[meanAPD-1.96*sdAPD,meanAPD+1.96*sdAPD]
print(C)



