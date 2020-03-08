""" From "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau and MJ Paez
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 2020. 
    Please respect copyright & acknowledge our work."""
 
# Wormhole.py: Symbolic evaluation of wormhole derivative

from sympy import *
L, x, M, rho, a, r,  lp= symbols('L x M rho a r lp')
x = (2*L-a)/(pi*M)
r = rho+M*(x*atan(x) -log(1+x*x)/2)
drdL = diff(r,L)
print ('drdL(raw) = ', drdL)
drdL = simplify(drdL)
print  (' And finally! dr/dL (simplified)=', drdL)
