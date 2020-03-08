""" From "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau and MJ Paez
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 2020. 
    Please respect copyright & acknowledge our work."""
 
# RelvPot.py: Relativistic potential plotting

import matplotlib.pyplot as plt
import numpy as np

dx = 0.1
yy = np.zeros((210))
xx = np.zeros((210))
yy1= np.zeros((210))
i = 0
ell = 4.3
G = 1.0
for x in np.arange(2.7,40,0.2):
    Vef = -G/x+ell**2/(2.*x**2)-G*ell**2/x**3
    xx[i] = x
    yy[i] = Vef
    yy1[i] = -1/x+ell**2/(2.*x**2)
    i += 1
f1  =plt.figure()
plt.axes()
plt.plot(xx[:180],yy[:180])
plt.plot(xx[22:180],yy1[22:180])
plt.xlabel("r/M")
plt.ylabel("Eff.Potential")
plt.title("Relativistic and Newton Potential")
plt.text(9,0.02,"Newtonian")
plt.plot([11,22],[-0.028,-0.028])
plt.plot(3.8,0.041,marker="o")
plt.plot(13.8,-0.031,marker="o")
plt.show()
