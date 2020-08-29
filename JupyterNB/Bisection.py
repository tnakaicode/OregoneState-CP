#!/usr/bin/env python
# coding: utf-8

# In[1]:


""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu (deceased), Univ Bucharest, 2020. 
    Please respect copyright & acknowledge our work."""

# Bisection.py: zero of f(x) via Bisection algorithm within [a,b]

from vpython import *
eps = 1e-3;  Nmax = 100;  a = 0.0; b = 7.0     # Precision, [a,b]
                          


# In[3]:


def f(x): return 2*cos(x) - x           # Your function here
    
def Bisection(Xminus, Xplus, Nmax, eps):          # Do not change    
   for it in range(0, Nmax):
       x = (Xplus +  Xminus)/2.                      
       print(" it =", it, " x = ", x, " f(x) =", f(x))
       if (f(Xplus)*f(x) > 0.): Xplus = x       # Change x+ to x
       else: Xminus =  x                        # Change x- to x
       if (abs(f(x) ) < eps):                   # Converged?
          print("\n Root found with precision eps = ", eps)
          break
       if it == Nmax-1: print ("\n No root after N iterations\n")
   return x

root = Bisection(a, b, Nmax, eps)
print(" Root =", root)


# In[ ]:




