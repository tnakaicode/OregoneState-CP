""" From "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau and MJ Paez
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia 2020. 
    Please respect copyright & acknowledge our work."""

# LensGravity.py    Deflection of light by sun wi Matplotlib

import numpy as np
import matplotlib.pyplot as plt
y = np.zeros((2),float);     ph = np.zeros((181),float)
yy = np.zeros((181),float);  xx = np.zeros((181),float)
rx = np.zeros((181),float);  ry = np.zeros((181),float)
Gsun = 4477.1                     # Sum mass x G (m)  
GM = 28.*Gsun                     # Sun mass
y[0] = 1.e-6; y[1] = 1e-6         # Initial condition for u=1/r

def f(t,y):                             # RHS, can modify
    rhs = np.zeros((2),float)
    rhs[0] = y[1]
    rhs[1] = 3*GM*(y[0]**2)-y[0]
    return rhs

def rk4Algor(t, h, N, y, f):    # Do not modify
    k1 = np.zeros(N); k2=np.zeros(N); k3=np.zeros(N); k4=np.zeros(N);
    k1 = h*f(t,y)
    k2 = h*f(t+h/2.,y+k1/2.)
    k3 = h*f(t+h/2.,y+k2/2.)
    k4 = h*f(t+h,y+k3)
    y = y+(k1+2*(k2+k3)+k4)/6.
    return y

f(0,y)                      # Initial conditions
dphi = np.pi/180.           # 180 values of angle phi
i = 0                        
for phi in np.arange(0,np.pi+dphi,dphi):
    ph[i] = phi
    y = rk4Algor(phi,dphi,2,y,f)            # Call rk4
    xx[i] = np.cos(phi)/y[0]/1000000        # Scale for graph
    yy[i] = np.sin(phi)/y[0]/1000000
    i = i+1
m = (yy[180] - yy[165])/(xx[180]-xx[165])   # Slope of straight line
b = yy[180] - m*xx[180]                     # Intercept of line
j = 0

for phi  in np.arange(0,np.pi+dphi,dphi):
    ry[j] = m*xx[j] + b            # Eqn straight line
    j=j+1
plt.figure(figsize=(12,6))
plt.plot(xx,yy)                  # Straight line light tajectroy
plt.plot(xx,-yy)                 # Symmetric for negative y
plt.plot(0,0,'ro')               # Mass at origin
plt.text(0.02,0.02, 'Sun')
plt.plot(0.98,0,'bo')            # Source
plt.plot(0.98,1.91,'go')         # Position source seen by O
plt.plot(0.98,-1.91,'go')         
plt.text(1,0,'S observer')
plt.text(-1.00, 0.20,'Source')
plt.text(1.02, 1.91,"S' observer")
plt.text(1.02,-2,"S'' observer")
plt.plot([0],[3.])               # Invisible point
plt.plot([0],[-3.])              # Invisible point at -y
plt.plot(xx,ry)                  # Upper straight line
plt.plot(xx,-ry)                 # Lower straight line
plt.xlabel('x')
plt.ylabel('y')
plt.show()
