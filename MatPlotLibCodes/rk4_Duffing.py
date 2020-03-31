""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# rk4Duffing.py solve ODE for Duffing Osc via rk4 & Matplotlib

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d
from rk4Algor import rk4Algor


def f(t, y):
    rhs = np.zeros((2))
    rhs[0] = y[1]
    rhs[1] = -2 * g * y[1] - a * y[0] - b * y[0]**3 + A * np.cos(w * t)
    return rhs


if __name__ == '__main__':
    tt = []
    yy = []
    vy = []
    a = 0.5
    b = -0.5
    g = 0.02
    A, w, h = 0.0008, 1., 0.01
    y = np.zeros((2), float)
    y[0] = 0.09
    y[1] = 0.

    f(0, y)
    # Time Loop
    for t in np.arange(0, 40, h):
        # Call rk4
        y = rk4Algor(t, h, 2, y, f)
        tt.append(t)
        # x(t)
        yy.append(y[0])
        vy.append(y[1])

    obj = plot2d(aspect="auto")
    ax1 = obj.add_axs(2, 2, 1, aspect="auto")
    ax1.plot(tt[1000:], yy[1000:])
    # 1000 avoids transients
    # x(t)
    ax1.set_title('Duffing Oscillator x(t)')
    ax1.set_xlabel('t')
    ax1.set_ylabel('x(t)')

    ax2 = obj.add_axs(2, 2, 4, aspect="equal")
    ax2.plot(yy[1000:], vy[1000:])
    ax2.set_title('Phase Space Orbits, Duffing Oscillator')
    ax2.set_xlabel('x(t)')
    ax2.set_ylabel('v(t)')
    obj.SavePng()
