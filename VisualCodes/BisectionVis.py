""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# BisectionVis.py: Visual pack, 0 of f(x) via Bisection algorithm

import numpy as np
import matplotlib.pyplot as plt
import math
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d


obj = plot2d()


def f(x):
    # Your function here
    return 2 * math.cos(x) - 2 * x


def Bisection(Xminus, Xplus, Nmax, eps):
    print("it\tx\tf(x)")
    px, py = [], []
    for it in range(0, Nmax):
        x = (Xplus + Xminus) / 2.

        txt = "\r"
        txt += "{:d}\t{:.3f}\t{:.3f}\t".format(it, x, f(x))
        sys.stdout.write(txt)
        sys.stdout.flush()

        if (f(Xplus) * f(x) > 0.):
            # Change x+ to x
            Xplus = x
        else:
            # Change x- to x
            Xminus = x

        # Converged?
        if (abs(f(x)) < eps):
            print("\n Root found with precision eps = ", eps)
            break

        if it == Nmax - 1:
            print("\n No root after N iterations\n")

        px.append(x)
        py.append(f(x))
    obj.axs.plot(px, py)

    print(x)


eps = 1e-3
Nmax = 100
a = 0.0
b = 7.0

Bisection(0.0, 10.0, 100, eps)
Bisection(-5.0, 7.0, 100, 1e-5)
Bisection(-10.0, .0, 100, 1e-5)

obj.Show()
