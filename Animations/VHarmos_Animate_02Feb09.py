""" From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation
"""
# VHarmos.py: Animaated t - dep Schro eq, Gaussian wavepacket, harm oscill V

from vpython import *

# Parameters
xmax = 300
dx = 0.04
dx2 = dx*dx
k0 = 5.5*math.pi
dt = dx2/4.0

# initialization
xp =  - 6
psr = zeros( (xmax + 1, 2), Float)                            # real wave function
psi = zeros( (xmax + 1, 2), Float)                        # imaginary wave function
p2 = zeros( (xmax + 1), Float)                             # probability (plotted)
v =  zeros( (xmax + 1), Float)                                        # potential

# set up curve
g = display(width = 500, height = 250, 
          title = 'Wave function in harmonic oscillator potential')
wvf =  curve(x = range(0, 300 + 1), color = color.yellow)                  
for i in range(0, xmax):
   xp2 = xp*xp
   psr[i, 0] = math.exp( - 0.5*xp2/0.25) * math.cos(k0*xp);   # Re wave funct Psi
   psi[i, 0] = math.exp( - 0.5*xp2/0.25) * math.sin(k0*xp);               # Im Psi
   v[i] = 15.0*xp2                                                   # Potential
   xp   += dx
   count = 0
while 1:                                             # time propagation, endless
   for i in range(1, xmax - 1):                                          # Re Psi
       psr[i, 1] = psr[i, 0] - dt*(psi[i + 1, 0] + psi[i - 1, 0]    
                    - 2.*psi[i, 0])/(dx2) + dt*v[i]*psi[i, 0]
       p2[i] = psr[i, 0]*psr[i, 1] + psi[i, 0]*psi[i, 0]
   if count%10 == 0:  # Add points to plot
       j = 0
       for i in range(1, xmax - 1, 3):         
            wvf.x[j] = 2*i - xmax
            wvf.y[j] = 130*p2[i]
            j = j + 1
       wvf.radius = 4.0 
       wvf.pos
   for i in range(1, xmax - 1):
        psi[i, 1] = psi[i, 0] + dt*(psr[i + 1, 1] + psr[i - 1, 1] - 2.*psr[i, 1])/(dx2) - dt*v[i]*psr[i, 1]
   for  i in range(0, xmax):
       psi[i, 0] = psi[i, 1]
       psr[i, 0] = psr[i][1] 
   count  =  count + 1
