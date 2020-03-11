""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# BugsVis.py The Logistic map wi V isual package

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d

m_min = 1.0
m_max = 4.0
step = 0.005
lasty = int(1000 * 0.5)
# Eliminates some points
count = 0
# Plot every 2 iterations

px, py = [], []
for m in np.arange(m_min, m_max, step):
    y = 0.5
    for i in range(1, 201, 1):
        y = m * y * (1 - y)
        # Avoid transients

    for i in range(201, 402, 1):
        y = m * y * (1 - y)

    for i in range(201, 402, 1):
        # Avoid transients
        oldy = int(1000 * y)
        y = m * y * (1 - y)
        inty = int(1000 * y)
        if inty != lasty and count % 2 == 0:
            px.append(m)
            py.append(y)
        lasty = inty
        count += 1

obj = plot2d(aspect="auto")
obj.axs.scatter(px, py, s=0.5)
#obj.axs.plot(px, py)
obj.SavePng()
