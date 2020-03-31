""" From "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau and MJ Paez
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 2020. 
    Please respect copyright & acknowledge our work."""

# RelOrbits.py: Reltiv orbits in gravitational pot (needs rk4)

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d
from rk4Algor import rk4Algor

dh = 0.04
dt = dh
ell = 4.3
G = 1.0
N = 2
E = 0.040139
phi = np.zeros((944), float)
rr = np.zeros((944), float)
y = np.zeros((2), float)
y[0] = 0.052
y[1] = np.sqrt(2 * E / ell**2 + 2 * G * y[0] / ell **
               2 - G * y[0]**2 + 2 * G * y[0]**3)


def f(t, y):
    rhs = np.zeros(2)
    rhs[0] = y[1]
    rhs[1] = -y[0] + G / ell**2 + 3 * G * y[0]**2
    return rhs


f(0, y)
i = 0
for fi in np.arange(0, 5.8 * np.pi, dt):
    y = rk4Algor(fi, dt, N, y, f)
    rr[i] = (1 / y[0]) * np.sin(fi)  # Notice 1/r (=u)
    phi[i] = (1 / y[0]) * np.cos(fi)
    i = i + 1

obj = plot2d()
obj.axs.plot(phi[:-1], rr[:-1])
obj.axs.set_xlabel("r/M")
obj.SavePng()
