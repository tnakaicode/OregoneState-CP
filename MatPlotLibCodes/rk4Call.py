""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# rk4Call.py: 4th-O Runge-Kutta that calls rk4Algor
#             Here for ODE y" = -100y-2y'+ 100 sin(3t)

from rk4Algor import rk4Algor
from numpy import *
import numpy as np
import matplotlib.pyplot as plt

Tstart = 0.
Tend = 10.
Nsteps = 100  # Initialization
tt = []
yy = []
yv = []
y = zeros((2), float)
y[0] = 3.
y[1] = -5.          # Initial position & velocity
t = Tstart
h = (Tend - Tstart) / Nsteps


def f(t, y):                           # Force (RHS) function
    fvector = zeros((2), float)
    fvector[0] = y[1]
    fvector[1] = -100. * y[0] - 2. * y[1] + 10. * sin(3. * t)
    return fvector


while (t < Tend):
    tt.append(t)                           # Time loop
    if ((t + h) > Tend):
        h = Tend - t     # Last step
    y = rk4Algor(t, h, 2, y, f)
    yy.append(y[0])
    yv.append(y[1])
    t = t + h
fig = plt.figure()
plt.subplot(111)
plt.plot(tt, yy, 'r')
plt.title('Position versus')
plt.xlabel('t')
plt.ylabel('y')
fig1 = plt.figure()
plt.subplot(111)
plt.plot(tt, yv)
plt.title('Velocity versus time')
plt.xlabel('t')
plt.ylabel('y')
plt.show
