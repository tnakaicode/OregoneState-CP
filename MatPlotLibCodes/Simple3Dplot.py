""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# Simple3Dplot.py: matplotlib 3D plot, rotate & scale with mouse

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

print("Please be patient while I do importing & plotting")
delta = 0.1
x = np.arange(-3., 3., delta)
y = np.arange(-3., 3., delta)
X, Y = np.meshgrid(x, y)
Z = np.sin(X) * np.cos(Y)                       # Surface height

fig = plt.figure()                               # Create figure
axs = Axes3D(fig)                                  # Plots axes
axs.plot_surface(X, Y, Z)                            # Surface
axs.plot_wireframe(X, Y, Z, color='r')        # Add wireframe
axs.set_xlabel('X')
axs.set_ylabel('Y')
axs.set_zlabel('Z')
plt.show()                                        # Output figure
