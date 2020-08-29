""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# DielectVis.py: Visual-Animated FDTD E&B dielectric fields

from vpython import *
import numpy as np
Xmax = 401
Ymax = 100
Zmax = 100
scene = graph(x=0, y=0, width=800, height=500,
              title='Hy (cyan), Ex (yellow), Dielectric (gray)', forward=(0.0, -0.3, -0.7))
Hplot = curve(x=list(range(0, Xmax)), color=color.cyan, radius=1.5,
              display=scene)
Eplot = curve(x=list(range(0, Xmax)), color=color.yellow, radius=1.5,
              display=scene)
#vplane = curve(pos=[(-Xmax, Ymax), (Xmax, Ymax), (Xmax, -Ymax),(-Xmax, -Ymax), (-Xmax, Ymax)], color=color.cyan)
vplane = curve(pos=[(-Xmax, 0, Ymax), (Xmax, 0, Ymax)], color=color.cyan)
zaxis = curve(pos=[(-Xmax, 0, 0), (Xmax, 0, 0)], color=color.magenta)
# hplane = curve(pos=[(-Xmax, 0, Zmax), (Xmax, 0, Zmax), (Xmax, 0, -Zmax),
#                    (-Xmax, 0, -Zmax), (-Xmax, 0, Zmax)], color=color.magenta)
hplane = curve(pos=[(-Xmax, 0, Zmax), (Xmax, 0, Zmax)], color=color.magenta)
sep = box(width=180, height=200, length=400,
          pos=vector(200, 0, 0), opacity=0.5)
eps = 4
dd = 0.5
Xmax = 401
Ex = np.zeros((Xmax), float)
Hy = np.zeros((Xmax), float)  # Declare
beta = np.zeros((Xmax), float)
z = arange(201)
Ex[:201] = 0.5 * np.sin(2 * pi * z / 100)
Hy[:201] = 0.5 * np.sin(2 * pi * z / 100)
for i in range(0, 401):
    if i < 201:
        beta[i] = dd                           # Free space
    else:
        beta[i] = dd / eps                             # Dielectric
Hylabel1 = label(text='Ex', pos=vector(-Xmax - 10, 120, 0), box=0)
Exlabel = label(text='Hy', pos=vector(-Xmax - 10, 0, 50), box=0)
zlabel = label(text='Z', pos=vector(Xmax + 10, 0, 0), box=0)       # Shifts fig
polfield = arrow(display=scene)
polfield2 = arrow(display=scene)


def plotfields():
    k = arange(Xmax)
    Hplot.x = 2 * k - Xmax                   # World to screen coords
    Hplot.z = 150 * Hy[k]
    Eplot.x = 2 * k - Xmax
    Eplot.y = 150 * Ex[k]


plotfields()
for i in range(0, 400):                          # Time evolution
    rate(50)
    for k in range(1, Xmax - 1):                # Field propagation
        Ex[k] = Ex[k] + beta[k] * (Hy[k - 1] - Hy[k])
        Hy[k] = Hy[k] + dd * (Ex[k] - Ex[k + 1])
    plotfields()
