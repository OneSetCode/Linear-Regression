import numpy as np
import matplotlib.pyplot as plt
from numpy.core.fromnumeric import size
x=np.random.normal(-1,0.2,10000)
e=np.random.normal(0,1,10000)
y1=x**2+e
y2=x**2
plt.scatter(x,y1,label='Y',s=0.5)
plt.scatter(x,y2,label='BLP',s=0.5)
plt.xlabel('x')  
plt.ylabel('y')  
plt.legend()  
plt.show() 
