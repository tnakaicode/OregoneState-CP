""" From "ComboPUTATIONAL PHYSICS" & "ComboPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# Permute.py accessible states versus energy

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os
import math

sys.path.append(os.path.join('../'))
from base import plot2d

k = 0
n = 25
B = 1.
mu = 1.
i = 0
Eo = -mu * B * n
Energy = [0.] * (13)
Combo = [0.] * (13)
for k in range(0, 26):
    c = math.factorial(n) / (math.factorial(n - k) * math.factorial(k))
    E = -(n - k) * mu * B + k * mu * B
    print(k, E - Eo, c)
    if k < 13:          # Only plot 1st 1/2 via symmetry
        Energy[i] = E - Eo
        Combo[i] = c
        i += 1

obj = plot2d()
ax1 = obj.add_axs(1, 2, 1)
# L: accessible states vs E-Eo
ax1.plot(Energy, Combo)
ax1.set_title('Number vs E-Eo')

ax2 = obj.add_axs(1, 2, 2)
ax2.loglog(Energy, Combo)
ax2.set_title('log(Number) vs log(E-Eo)')
obj.SavePng()
