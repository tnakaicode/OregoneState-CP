""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# FDTD.py  FDTD Maxwell's equations in 1-D wi Visual

from vpython import *
import numpy as np

Xm = 201
Ym = 100
Zm = 100
ts = 2
beta = 0.01
Ex = np.zeros((Xm, ts), float)
Hy = np.zeros((Xm, ts), float)  # Arrays
scene = graph(x=0, y=0, width=800, height=500,
              title='E: cyan, H: red. Periodic BC', forward=(-0.6, -0.5, -1))
Eplot = curve(x=list(range(0, Xm)), color=color.cyan,
              radius=1.5, display=scene)
Hplot = curve(x=list(range(0, Xm)), color=color.red,
              radius=1.5, display=scene)
# vplane = curve(pos=[(-Xm, Ym), (Xm, Ym), (Xm, -Ym), (-Xm, -Ym),
#                    (-Xm, Ym)], color=color.cyan)
vplane = curve(color=color.cyan)
#zaxis = curve(pos=[(-Xm, 0), (Xm, 0)], color=color.magenta)
zaxis = curve(color=color.magenta)
hplane = curve(pos=[(-Xm, 0, Zm), (Xm, 0, Zm), (Xm, 0, -Zm), (-Xm, 0, -Zm),
                    (-Xm, 0, Zm)], color=color.magenta)
#ball1 = sphere(pos=vector(Xm + 30, 0, 0), color=color.black, radius=2)
ball1 = sphere(color=color.black, radius=2)
#ExLabel1 = label(text='Ex', pos=vector(-Xm - 10, 50), box=0)
ExLabel1 = label(text='Ex', box=0)
#ExLabel2 = label(text='Ex', pos=vector(Xm + 10, 50), box=0)
ExLabel2 = label(text='Ex', box=0)
HyLabel = label(text='Hy', box=0)
#HyLabel = label(text='Hy', pos=vector(-Xm - 10, 0, 50), box=0)
#zLabel = label(text='Z', pos=vector(Xm + 10, 0), box=0)
zLabel = label(text='Z', box=0)


def PlotFields():
    z = arange(Xm)
    Eplot.x = 2 * z - Xm                    # World to screen coords
    Eplot.y = 800 * Ex[z, 0]
    Hplot.x = 2 * z - Xm
    Hplot.z = 800 * Hy[z, 0]


z = arange(Xm)
Ex[:, 0] = 0.1 * sin(2 * pi * z / 100.0)                 # Initial field
Hy[:, 0] = 0.1 * sin(2 * pi * z / 100.0)
PlotFields()

while True:
    rate(600)
    Ex[1:Xm - 1, 1] = Ex[1:Xm - 1, 0] + beta * (Hy[0:Xm - 2, 0] - Hy[2:Xm, 0])
    Hy[1:Xm - 1, 1] = Hy[1:Xm - 1, 0] + beta * (Ex[0:Xm - 2, 0] - Ex[2:Xm, 0])
    Ex[0, 1] = Ex[0, 0] + beta * (Hy[Xm - 2, 0] - Hy[1, 0])   # BC
    Ex[Xm - 1, 1] = Ex[Xm - 1, 0] + beta * (Hy[Xm - 2, 0] - Hy[1, 0])
    Hy[0, 1] = Hy[0, 0] + beta * (Ex[Xm - 2, 0] - Ex[1, 0])    # BC
    Hy[Xm - 1, 1] = Hy[Xm - 1, 0] + beta * (Ex[Xm - 2, 0] - Ex[1, 0])
    PlotFields()
    Ex[:Xm, 0] = Ex[:Xm, 1]                              # New -> old
    Hy[:Xm, 0] = Hy[:Xm, 1]
