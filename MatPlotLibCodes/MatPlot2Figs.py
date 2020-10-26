""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# MatPlot2figs.py: plot of 2 subplots on 1 fig, 2 separate figs

import numpy as np
import matplotlib.pyplot as plt

Xmin = -5.0
Xmax = 5.0
Npoints = 500
DelX = (Xmax - Xmin) / Npoints                             # Delta x
x1 = np.arange(Xmin, Xmax, DelX)                        # x1 range
x2 = np.arange(Xmin, Xmax, DelX / 20)           # Different x2 range
y1 = -np.sin(x1) * np.cos(x1 * x1)                          # Function 1
y2 = np.exp(-x2 / 4.) * np.sin(x2)                         # Function 2
print("\n Now plotting, look for Figures 1 & 2 on desktop")

plt.figure(1)         # Fig 1
plt.subplot(2, 1, 1)                     # 1st subplot in first figure
plt.plot(x1, y1, 'r', lw=2)
plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('$-\sin(x)*\cos(x^2)$')
plt.grid(True)                                          # Form grid
plt.subplot(2, 1, 2)                     # 2nd subplot in first figure
plt.plot(x2, y2, '-', lw=2)
plt.xlabel('x')                                       # Axes labels
plt.ylabel('f(x)')
plt.title('exp(-x/4)*sin(x)')

plt.figure(2)  # Fig 2
plt.subplot(2, 1, 1)                       # 1st subplot in 2nd figure
plt.plot(x1, y1 * y1, 'r', lw=2)
plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('$\sin^2(x)*\cos^2(x^2)$')
plt.subplot(2, 1, 2)                       # 2nd subplot in 2nd figure
plt.plot(x2, y2 * y2, '-', lw=2)
plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('$\exp(-x/2)*\sin^2(x)$')
plt.grid(True)
plt.show()
