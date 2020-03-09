# -*- coding: utf-8 -*-
""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2020. 
    Please respect copyright & acknowledge our work."""

# Beam.py: solves Navier-Stokes equation for flow around beam

import numpy as np
import matplotlib.pylab as plt
from mpl_toolkits.mplot3d import Axes3D

print("Working, wait for the figure after 100 iterations")

Nxmax = 70
Nymax = 20
IL = 10
H = 8
T = 8
h = 1.
# Stream
u = np.zeros((Nxmax + 1, Nymax + 1), float)
# Vorticity
w = np.zeros((Nxmax + 1, Nymax + 1), float)
V0 = 1.0
omega = 0.1
nu = 1.
iter = 0
R = V0 * h / nu


def borders():
    # Init stream
    # Init vorticity
    for i in range(0, Nxmax + 1):
        for j in range(0, Nymax + 1):
            w[i, j] = 0.
            u[i, j] = j * V0

    # Fluid surface
    for i in range(0, Nxmax + 1):
        u[i, Nymax] = u[i, Nymax - 1] + V0 * h
        w[i, Nymax - 1] = 0.

    # Inlet
    for j in range(0, Nymax + 1):
        u[1, j] = u[0, j]
        w[0, j] = 0.

    # Centerline
    for i in range(0, Nxmax + 1):
        if i <= IL and i >= IL + T:
            u[i, 0] = 0.
            w[i, 0] = 0.

    # Outlet
    for j in range(1, Nymax):
        w[Nxmax, j] = w[Nxmax - 1, j]
        u[Nxmax, j] = u[Nxmax - 1, j]


def beam():
    # BC for beam

    # Sides
    for j in range(0, H + 1):
        # Front
        # Back
        w[IL, j] = - 2 * u[IL - 1, j] / (h * h)
        w[IL + T, j] = - 2 * u[IL + T + 1, j] / (h * h)

    for i in range(IL, IL + T + 1):
        w[i, H - 1] = - 2 * u[i, H] / (h * h)

    for i in range(IL, IL + T + 1):
        for j in range(0, H + 1):
            # Front
            u[IL, j] = 0.

            # Back
            u[IL + T, j] = 0.

            # Top
            u[i, H] = 0


def relax():
    # Relax stream
    # Reset
    beam()

    # Relax stream
    for i in range(1, Nxmax):
        for j in range(1, Nymax):
            r1 = omega * ((u[i + 1, j] + u[i - 1, j] + u[i, j + 1] + u[i, j - 1]
                           + h * h * w[i, j]) / 4 - u[i, j])
            u[i, j] += r1

    # Relax vorticity
    for i in range(1, Nxmax):
        for j in range(1, Nymax):
            a1 = w[i + 1, j] + w[i - 1, j] + w[i, j + 1] + w[i, j - 1]
            a2 = (u[i, j + 1] - u[i, j - 1]) * (w[i + 1, j] - w[i - 1, j])
            a3 = (u[i + 1, j] - u[i - 1, j]) * (w[i, j + 1] - w[i, j - 1])
            r2 = omega * ((a1 - (R / 4.) * (a2 - a3)) / 4. - w[i, j])
            w[i, j] += r2


borders()
while (iter <= 100):
    iter += 1
    if iter % 10 == 0:
        print(iter)
    relax()
for i in range(0, Nxmax + 1):
    for j in range(0, Nymax + 1):
        u[i, j] = u[i, j] / V0 / h
x = range(0, Nxmax - 1)
y = range(0, Nymax - 1)
X, Y = np.meshgrid(x, y)


def functz(u):
    # Stream flow
    z = u[X, Y]
    return z


Z = functz(u)
fig = plt.figure()
axs = Axes3D(fig)
axs.plot_wireframe(X, Y, Z, color='r')
axs.set_xlabel('X')
axs.set_ylabel('Y')
axs.set_zlabel('Stream Function')
plt.show()
