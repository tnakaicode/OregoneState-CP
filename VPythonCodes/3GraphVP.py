""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# 3GraphVP.py: Vpython package, 3 plots, with bars, dots & curve

from vpython import *

string = "blue: sin^2(x), black= cos^2(x), cyan: sin(x)*cos(x)"
graph1 = graph(title=string, xtitle='x', ytitle='y',
               background=color.white, foreground=color.black)
y1 = gcurve(color=color.blue)     # curve
y2 = gvbars(color=color.black)    # vertical bars
y3 = gdots(color=color.cyan)      # dots
for x in arange(-5, 5, 0.1):        # arange for  plots
    y1.plot(pos=vector(x, sin(x)**2))
    y2.plot(pos=vector(x, cos(x) * cos(x) / 3.))
    y3.plot(pos=vector(x, sin(x) * cos(x)))
