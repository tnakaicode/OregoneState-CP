""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# CircPolarztn.py: Maxwell eqs. circular polariz via FDTD

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os
from vpython import *  

sys.path.append(os.path.join('../'))
from base import plot2d

phy, pyx = 0.0, 0.0
max = 201
c = 0.01                 # Courant stable if c < 0.1
Ex = np.zeros((max + 2, 2), float)
Ey = np.zeros((max + 2, 2), float)
Hy = np.zeros((max + 2, 2), float)
Hx = np.zeros((max + 2, 2), float)
arrowcol = color.white
Earrows = []
Harrows = []
for i in range(0, max, 10):
    Earrows.append(arrow(pos=vector(0, i - 100, 0), axis=vector(0, 0, 0),
                         color=arrowcol))
    Harrows.append(arrow(pos=vector(0, i - 100, 0), axis=vector(0, 0, 0),
                         color=color.yellow))


def plotfields(Ex, Ey, Hx, Hy):
    for n, arr in enumerate(Earrows):
        arr.axis = (35 * Ey[10 * n, 1], 0, 35 * Ex[10 * n, 1])
    for n, arr in enumerate(Harrows):
        arr.axis = (35 * Hy[10 * n, 1], 0, 35 * Hx[10 * n, 1])


def inifields():
    # Initial E & H
    phx = 0.5 * np.pi
    phy = 0.0
    z = np.arange(0, max)
    Ex[:-2, 0] = np.cos(-2 * np.pi * z / 200 + phx)
    Ey[:-2, 0] = np.cos(-2 * np.pi * z / 200 + phy)
    Hx[:-2, 0] = np.cos(-2 * np.pi * z / 200 + phy + np.pi)
    Hy[:-2, 0] = np.cos(-2 * np.pi * z / 200 + phx)


def newfields():
    while True:
        # Time stepping
        Ex[1:max - 1, 1] = Ex[1:max - 1, 0] + \
            c * (Hy[:max - 2, 0] - Hy[2:max, 0])
        Ey[1:max - 1, 1] = Ey[1:max - 1, 0] + \
            c * (Hx[2:max, 0] - Hx[:max - 2, 0])
        Hx[1:max - 1, 1] = Hx[1:max - 1, 0] + \
            c * (Ey[2:max, 0] - Ey[:max - 2, 0])
        Hy[1:max - 1, 1] = Hy[1:max - 1, 0] + \
            c * (Ex[:max - 2, 0] - Ex[2:max, 0])
        Ex[0, 1] = Ex[0, 0] + c * (Hy[200 - 1, 0] - Hy[1, 0])

        # Periodic
        Ex[200, 1] = Ex[200, 0] + c * (Hy[200 - 1, 0] - Hy[1, 0])
        Ey[0, 1] = Ey[0, 0] + c * (Hx[1, 0] - Hx[200 - 1, 0])
        Ey[200, 1] = Ey[200, 0] + c * (Hx[1, 0] - Hx[200 - 1, 0])
        Hx[0, 1] = Hx[0, 0] + c * (Ey[1, 0] - Ey[200 - 1, 0])
        Hx[200, 1] = Hx[200, 0] + c * (Ey[1, 0] - Ey[200 - 1, 0])
        Hy[0, 1] = Hy[0, 0] + c * (Ex[200 - 1, 0] - Ex[1, 0])
        Hy[200, 1] = Hy[200, 0] + c * (Ex[200 - 1, 0] - Ex[1, 0])
        plotfields(Ex, Ey, Hx, Hy)

        Ex[:max, 0] = Ex[:max, 1]
        Ey[:max, 0] = Ey[:max, 1]
        Hx[:max, 0] = Hx[:max, 1]
        Hy[:max, 0] = Hy[:max, 1]


inifields()
newfields()
