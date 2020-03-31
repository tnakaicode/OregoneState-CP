""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# rk4Call.py: 4th-O Runge-Kutta that calls rk4Algor
#             Here for ODE y" = -100y-2y'+ 100 sin(3t)

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d
from rk4Algor import rk4Algor


def f(t=0, y=[0, 1]):
    # Force (RHS) function
    fvector = np.zeros((2), float)
    fvector[0] = y[1]
    fvector[1] = -100. * y[0] - 2. * y[1] + 10. * np.sin(3. * t)
    return fvector


if __name__ == '__main__':
    # Initialization
    Tstart = 0.
    Tend = 10.
    Nsteps = 500
    tt = []
    yy = []
    yv = []

    # Initial position & velocity
    y = np.zeros(2)
    y[0] = 3.
    y[1] = -5.
    t = Tstart
    h = (Tend - Tstart) / Nsteps

    while (t < Tend):
        tt.append(t)
        # Time loop
        if ((t + h) > Tend):
            h = Tend - t
            # Last step
        y = rk4Algor(t, h, 2, y, f)
        yy.append(y[0])
        yv.append(y[1])
        t = t + h

    obj = plot2d(aspect="auto")
    obj.axs.plot(tt, yy, 'r')
    obj.axs.set_title('Position versus')
    obj.axs.set_xlabel('t')
    obj.axs.set_ylabel('y')
    obj.SavePng(obj.tempname + "-Position.png")

    obj.new_fig(aspect="auto")
    obj.axs.plot(tt, yv)
    obj.axs.set_title('Velocity versus time')
    obj.axs.set_xlabel('t')
    obj.axs.set_ylabel('y')
    obj.SavePng(obj.tempname + "-Velocity.png")
