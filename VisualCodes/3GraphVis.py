""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""
  
# 3GraphVis.py: Visual package, 3 plots, with bars, dots & curve

from vpython import *

string = "blue: sin^2(x), white: cos^2(x), red: sin(x)*cos(x)"
graph1 = graph(title=string, xtitle='x', ytitle='y')
y1 = gcurve(color=color.yellow)          # curve
y2 = gvbars(color=color.white)             # vertical bars
y3 = gdots(color=color.red)              # dots

for x in arange(-5, 5, 0.1):                # arange for floats
    y1.plot( pos=(x, sin(x)*sin(x)) )
    y2.plot( pos=(x, cos(x)*cos(x)/3.) )
    y3.plot( pos=(x, sin(x)*cos(x)) )
