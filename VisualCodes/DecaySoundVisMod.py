""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# DecaySoundVis.py: Visual, spontaneous decay simulation

from vpython import *
#from vpython.graph import *
import random
import winsound as ws

lambda1 = 0.005                               # Decay constant
max = 100.
time_max = 500
seed = 68111
number = nloop = max                           # Initial value
graph1 = graph(title='Spontaneous Decay', xtitle='Time',
                  ytitle='Number')
decayfunc = gcurve(color=color.green)

for time in arange(0, time_max + 1):              # Time loop
    for atom in arange(1, number + 1):          # Decay loop
        decay = random.random()
        if (decay < lambda1):
            nloop = nloop - 1                     # A decay
            soundfile = "C:/Windows/Media/Windows Startup.wav"
            ws.PlaySound(soundfile, ws.SND_FILENAME | ws.SND_ASYNC)
    number = nloop
    decayfunc.plot(pos=vector(time, number))
    rate(30)
