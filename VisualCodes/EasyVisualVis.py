""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# EasyVisualVis.py: Visual, simple graph object using Visual

from vpython import *                  # Import Visual

Plot1 = gcurve(color=color.white)         # gcurve method
for x in arange(0., 8.1, 0.1):              # x range
    Plot1.plot(pos=vector(x, 5. * cos(2. * x) * exp(-0.4 * x)))
graph1 = graph(width=600, height=450,
               title='Visual 2-D Plot', xtitle='x', ytitle='f(x)',
               foreground=color.black, background=color.white)
Plot2 = gdots(color=color.black)          # Dots
for x in arange(-5., +5, 0.1):
    Plot2.plot(pos=vector(x, cos(x)))
