""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# 3QMdisks.py: Wavepacket scattering from 3 disks wi MatPlot

import numpy as np
import matplotlib.pylab as plt
import sys
from mpl_toolkits.mplot3d import Axes3D

r = 10
N = 101
x1 = 51  # 51 = 90.*sqrt 3/2-30
dx = 0.1
dx2 = dx * dx
k0 = 20.
k1 = 0.
dt = 0.002
fc = dt / dx2

# Declare arrays
V = np.zeros((N, N), float)
Rho = np.zeros((N, N), float)
RePsi = np.zeros((N, N), float)
ImPsi = np.zeros((N, N), float)
ix = np.arange(0, 101)
iy = np.arange(0, 101)


def Pot1Disk(xa, ya):
    # Potential single disk
    for i in range(ya - r, ya + r + 1):
        for j in range(xa - r, xa + r + 1):
            if np.sqrt((i - ya)**2 + (j - xa)**2) <= r:
                V[i, j] = 10


def Pot3Disks():
    # Potential three disk
    Pot1Disk(25, 45)
    Pot1Disk(75, 45)
    Pot1Disk(50, 80)


def Psi_0(Xo, Yo):
    # Initial Psi
    for i in np.arange(0, N):
        for j in np.arange(0, N):
            Gaussian = np.exp(-0.03 * (i - Yo)**2 - 0.03 * (j - Xo)**2)
            RePsi[i, j] = Gaussian * np.cos(k0 * i + k1 * j)
            ImPsi[i, j] = Gaussian * np.sin(k0 * i + k1 * j)
            Rho[i, j] = RePsi[i, j]**2 + ImPsi[i, j]**2 + 0.01


if __name__ == '__main__':
    # Psi and Rho initial
    # Initial Psi
    Xo = 60
    Yo = 25
    Psi_0(Xo, Yo)
    Pot3Disks()

    num = 201
    for t in range(0, num):
        sys.stdout.write("\r{:d} / {:d}".format(t + 1, num))
        sys.stdout.flush()

        ImPsi[1:-1, 1:-1] = ImPsi[1:-1, 1:-1] + fc * (RePsi[2:, 1:-1]
                                                      + RePsi[:-2, 1:-1] - 4 * RePsi[1:-1, 1:-1] + RePsi[1:-1, 2:]
                                                      + RePsi[1:-1, :-2]) + V[1:-1, 1:-1] * dt * RePsi[1:-1, 1:-1]
        RePsi[1:-1, 1:-1] = RePsi[1:-1, 1:-1] - fc * (ImPsi[2:, 1:-1]
                                                      + ImPsi[:-2, 1:-1] - 4 * ImPsi[1:-1, 1:-1] + ImPsi[1:-1, 2:]
                                                      + ImPsi[1:-1, :-2]) + V[1:-1, 1:-1] * dt * ImPsi[1:-1, 1:-1]

        # Compute Rho
        # Hard Disk, psi = 0
        for i in range(1, N - 1):
            for j in range(1, N - 1):
                if V[i, j] != 0:
                    RePsi[i, j] = 0
                    ImPsi[i, j] = 0
                Rho[i, j] = 0.1 * (RePsi[i, j]**2
                                   + ImPsi[i, j]**2) + 0.0002 * V[i, j]

        if t in [0, 50, 100, 110, 125, 150, 200]:
            X, Y = np.meshgrid(ix, iy)
            Z = Rho[X, Y]

            fig = plt.figure()
            axs = Axes3D(fig)
            axs.set_xlabel('y')
            axs.set_ylabel('x')
            axs.set_zlabel('Rho(x,y)')
            axs.plot_wireframe(X, Y, Z, color='g')
            fig.savefig("3QMdisks_3D_{:03d}.png".format(t))

            fig, axs = plt.subplots()
            axs.set_aspect("equal")
            axs.set_xlabel('y')
            axs.set_ylabel('x')
            axs.grid()
            axs.contourf(X, Y, Z, cmap="jet")
            fig.savefig("3QMdisks_2D_{:03d}.png".format(t))
            plt.close()
