""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# AdvecLax.py:      Solve advection eqnt via Lax-Wendroff scheme
# du/dt+ c*d(u**2/2)/dx=0;   u(x,t=0)=exp(-300(x-0.12)**2)
# initial condition: u(x,0)=exp(-300(x-0.12)**2) gaussian shape

import numpy as np
import matplotlib.pyplot as plt
from numpy import *

N = 100
c = 1.
dx = 1. / N
beta = 0.8   # beta = c*dt/dx
dt = beta * dx / c
T_final = 0.5
n = int(T_final / dt)
u0 = []
uf = []
xx = []
u = np.zeros((N + 1), float)
plt.figure(0)


def plotIniExac():       # Plot initial and exact solution
    for i in range(0, N):
        x = i * dx
        xx.append(x)
        u0.append(np.exp(-300. * (x - 0.12)**2))  # Gaussian initial data
        # initfn.plot(pos = (0.01*i, u0[i]) )  # plot initial function
        # Exact in cyan
        uf.append(np.exp(- 300. * (x - 0.12 - c * T_final)**2))
    plt.plot(xx, u0, 'b')   # in blue
    plt.plot(xx, uf, 'g')   # in orange


plotIniExac()


def numerical():               # Lax Wendroff solution
    for j in range(0, n + 1):  # loop for time
        for i in range(0, N - 2):  # loop for x
            u[i + 1] = (1. - beta * beta) * u0[i + 1] - (0.5 * beta) * \
                (1. - beta) * u0[i + 2] + 0.5 * beta * (1. + beta) * u0[i]
            u[0] = 0.
            u[N - 1] = 0.
            u0[i] = u[i]
    plt.plot(xx, u[:-1], 'r')  # in red


numerical()
plt.title('Initial in blue, final in green, numerical in red')
plt.xlabel('x')
plt.show()
