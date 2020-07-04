""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# DecaySoundVis.py: Visual, spontaneous decay simulation

import numpy as np
import matplotlib.pyplot as plt
import random
import winsound
import sys
import time
import os

sys.path.append(os.path.join('../'))
from base import plot2d

lambda1 = 0.005                               # Decay constant
max = 80.
time_max = 500
seed = 68111
number = nloop = max                           # Initial value
graph1 = gdisplay(title='Spontaneous Decay', xtitle='Time',
                  ytitle='Number')
decayfunc = gcurve(color=color.green)

for time in np.arange(0, time_max + 1):              # Time loop
    for atom in np.arange(1, number + 1):          # Decay loop
        decay = random.random()
        if (decay < lambda1):
            nloop = nloop - 1                     # A decay
            winsound.Beep(600, 100)              # Sound beep
    number = nloop
    decayfunc.plot(pos=vector(time, number))
    rate(30)
