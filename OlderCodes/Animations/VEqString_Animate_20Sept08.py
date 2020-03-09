""" From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation
"""
# EqString.py:          Animated leapfrog solution of wave equation (Sec 18.2.2)


from vpython import *

# Set up curve
g = display(width = 600, height = 300, title = 'Vibrating string')
vibst = curve(x = range(0, 100), color = color.yellow)
ball1 = sphere(pos = (100, 0), color = color.red, radius = 2)
ball2 = sphere(pos = ( - 100, 0), color = color.red, radius = 2)
ball1.pos
ball2.pos
vibst.radius = 1.0

# Parameters
rho = 0.01                                                      # string density
ten = 40.                                                       # string tension
c = math.sqrt(ten/rho)                                      # Propagation speed
c1 = c                                                           # CFL criterium
ratio =  c*c/(c1*c1)

# Initialization
xi = zeros( (101, 3), Float)                     # 101 x's & 3 t's (maybe float)
for i in range(0, 81):
    xi[i, 0] = 0.00125*i;                                   # Initial conditions

for i in range (81, 101):                               # first part of string
    xi[i, 0] = 0.1 - 0.005*(i - 80)                         # second part of string

for  i in range(0, 100):                                          # First t step
    vibst.x[i] = 2.0*i - 100.0         # assign & scale x:  0<i<100  - >  - 100<i<100
    vibst.y[i] = 300.*xi[i, 0]                  # assing & scale y: xi to 300*xi
vibst.pos                                                          # draw string

# Later time steps
for i in range(1, 100):                                         # use  algorithm
   xi[i, 1] = xi[i, 0] + 0.5*ratio*(xi[i + 1, 0] + xi[i - 1, 0] - 2*xi[i, 0])
while 1:                                    # continue plotting until user quits
    rate(50)                                # delays plotting, (bigger = slower)
    for i in range(1, 100):              
        xi[i, 2] = 2.*xi[i, 1] - xi[i, 0]  +  ratio*(xi[i + 1, 1]  +  xi[i - 1, 1]  -  2*xi[i, 1])     
    for i in range(1, 100):
         vibst.x[i] = 2.*i - 100.0                            # scaledx - positions
         vibst.y[i] = 300.*xi[i, 2]                         # scaled y - positions
    vibst.pos                                                     # plot string
    for i in range(0, 101):
        xi[i, 0] = xi[i, 1]                                      # recycle array
        xi[i, 1] = xi[i, 2]                      
print "finished"
