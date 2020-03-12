""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HOmovMat: Matplot animated soltion t-dependent Sch Eqt for HO

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os
from matplotlib.animation import FuncAnimation
from optparse import OptionParser

sys.path.append(os.path.join('../'))
from base import plot2d, create_tempnum, create_tempdir

dx = 0.04
dx2 = dx * dx
k0 = 5.5 * np.pi
dt = dx2 / 20.0
xmax = 6.0
beta = dt / dx2
nmax = 301

xs = np.arange(-xmax, xmax + dx / 2, dx)
V = np.zeros((nmax), float)
V = 15.0 * xs**2
R = np.zeros((nmax, 2), float)
I = np.zeros((nmax, 2), float)
R[:, 0] = np.exp(-0.5 * (xs / 0.5)**2) * np.cos(k0 * xs)       # Re Psi
I[:, 0] = np.exp(-0.5 * (xs / 0.5)**2) * np.sin(k0 * xs)       # Im Pse

fig = plt.figure()
ax = fig.add_subplot(111, autoscale_on=False,
                     xlim=(-xmax, xmax), ylim=(0, 1.5))
ax.grid()
plt.title("Harmonic Oscillator")
line, = ax.plot(xs, R[:, 0]**2 + I[:, 0]**2, lw=2)


def animate(dum):
    R[1:-1, 1] = R[1:-1, 0] - beta * \
        (I[2:, 0] + I[:-2, 0] - 2 * I[1:-1, 0]) + dt * V[1:-1] * I[1:-1, 0]
    line.set_data(xs, R[:, 1] * R[:, 0] + I[:, 0]**2)
    I[1:-1, 1] = I[1:-1, 0] + beta * (R[2:, 1] + R[:-2, 1] - 2 * R[1:-1, 1])
    - dt * V[1:-1] * R[1:-1, 1]
    R[:, 0] = R[:, 1]
    I[:, 0] = I[:, 1]
    return line,


ani = FuncAnimation(fig, animate, frames=100, interval=1)
tmpdir = create_tempdir()
gifname = create_tempnum("HOmovMatplot.gif", tmpdir, ".gif")
ani.save(gifname, writer="pillow")
plt.show()
