""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# FDTD.py  FDTD Maxwell's equations in 1-D wi Visual

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

Xm = 201
Ym = 100
Zm = 100
ts = 2
beta = 0.01
Ex = np.zeros((Xm, ts), float)
Hy = np.zeros((Xm, ts), float)  # Arrays

fig, axs = plt.subplots()
axs.grid()
eplot, = axs.plot(np.arange(Xm), np.arange(Xm))
hplot, = axs.plot(np.arange(Xm), np.arange(Xm))


def PlotFields(Ex, Hy):
    z = np.arange(Xm)
    eplot.set_data(2 * z - Xm, 800 * Ex[z, 0])
    hplot.set_data(2 * z - Xm, 800 * Hy[z, 0])


def animate(t):
    print(t)
    Ex[1:Xm - 1, 1] = Ex[1:Xm - 1, 0] + beta * (Hy[0:Xm - 2, 0] - Hy[2:Xm, 0])
    Hy[1:Xm - 1, 1] = Hy[1:Xm - 1, 0] + beta * (Ex[0:Xm - 2, 0] - Ex[2:Xm, 0])
    Ex[0, 1] = Ex[0, 0] + beta * (Hy[Xm - 2, 0] - Hy[1, 0])
    Ex[Xm - 1, 1] = Ex[Xm - 1, 0] + beta * (Hy[Xm - 2, 0] - Hy[1, 0])
    Hy[0, 1] = Hy[0, 0] + beta * (Ex[Xm - 2, 0] - Ex[1, 0])
    Hy[Xm - 1, 1] = Hy[Xm - 1, 0] + beta * (Ex[Xm - 2, 0] - Ex[1, 0])
    PlotFields(Ex, Hy)
    # New -> old
    Ex[:Xm, 0] = Ex[:Xm, 1]
    Hy[:Xm, 0] = Hy[:Xm, 1]
    return


def init():
    z = np.arange(Xm)
    # Initial field
    Ex[:, 0] = 0.1 * np.sin(2 * np.pi * z / 100.0)
    Hy[:, 0] = 0.1 * np.sin(2 * np.pi * z / 100.0)
    PlotFields(Ex, Hy)
    return


ani = FuncAnimation(fig, animate, frames=50,
                    init_func=init, interval=20, blit=False)
ani.save("./FDTD.gif", writer='pillow')
plt.show()
