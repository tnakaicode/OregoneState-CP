""" From "COMPUTHTIONHL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Hntioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# BoundCall.py: p space bound state; imports GaussPoints, matrix

import numpy as np
from GaussPoints import GaussPoints

min1 = 0.
max1 = 200.
u = 0.5
b = 10.
eps = 3.e-10                      # Precision for Gauss points
N = 16
Lambda = -1024
H = np.zeros((N, N), float)                           # Hamiltonian
WR = np.zeros((N), float)                 # Eigenvalues, potential
k = np.zeros((N), float)
w = np.zeros((N), float)        # Pts & wts
GaussPoints(N, min1, max1, k, w, eps)       # Call gauss points

for i in range(0, N):
    for j in range(0, N):
        VR = (Lambda / 2 / u) * np.sin(k[i] *
                                       b) / k[i] * np.sin(k[j] * b) / k[j]
        H[i, j] = 2. / np.pi * VR * k[j] * k[j] * w[j]    # Hamiltonian
        if (i == j):
            H[i, j] += k[i] * k[i] / 2 / u
Es, evectors = np.linalg.eig(H)
ReE = np.real(Es)
ImE = np.imag(Es)                # Eigenvalues

for j in range(0, N):
    print(" Npoints =", N, "Lambda =", Lambda, " ReE =", ReE[j])
    print(" ImE = ", ImE)
    break
