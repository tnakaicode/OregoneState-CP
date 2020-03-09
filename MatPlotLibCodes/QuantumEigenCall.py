""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# QuantumEigenCall.py: Finds E & psi via rk4 + bisection

import numpy as np
import matplotlib.pyplot as plt
from rk4Algor import rk4Algor

# m/(hbar*c)**2 = 940MeV/(197.33MeV-fm)**2 = 0.4829
eps = 1e-1
Nsteps = 501
h = 0.04
Nmax = 100  # Params
E = -17.
Emax = 1.1 * E
Emin = E / 1.1


def f(x, y):                                 # RHS for ODE
    global E
    F = np.zeros((2), float)
    F[0] = y[1]
    F[1] = -(0.4829) * (E - V(x)) * y[0]
    return F


def V(x):                                        # Potential
    if (abs(x) < 10.):
        return (-16.0)
    else:
        return (0.)


def diff(h, E):                           # Change in log deriv
    #global E
    y = np.zeros((2), float)
    i_match = Nsteps // 3                     # Matching radius
    nL = i_match + 1
    # print('nL',nL)
    y[0] = 1.E-15                          # Initial left wf
    y[1] = y[0] * np.sqrt(-E * 0.4829)
    for ix in range(0, nL + 1):
        x = h * (ix - Nsteps / 2)
        y = rk4Algor(x, h, 2, y, f)
    left = y[1] / y[0]                       # Log  derivative
    y[0] = 1.E-15  # Slope for even; reverse if odd
    y[1] = -y[0] * np.sqrt(-E * 0.4829)           # Initialize R wf
    for ix in range(Nsteps, nL + 1, -1):
        x = h * (ix + 1 - Nsteps / 2)
        y = rk4Algor(x, -h, 2, y, f)
    right = y[1] / y[0]                       # Log derivative
    return((left - right) / (left + right))


def plot(h):                   # Repeat integrations for plot
    global xL, xR, Rwf, Lwf
    x = 0.                              # Matching radius
    Lwf = []                              # Left wave function
    Rwf = []                             # Right wave function
    xR = []                                   # x for right wf
    xL = []
    Nsteps = 1501                         # Integration steps
    y = np.zeros((2), float)
    yL = np.zeros((2, 505), float)
    i_match = 500                           # Matching radius
    nL = i_match + 1
    print('nL', nL)
    y[0] = 1.E-40                           # Initial left wf
    y[1] = -np.sqrt(-E * 0.4829) * y[0]
    for ix in range(0, nL + 1):
        yL[0][ix] = y[0]
        yL[1][ix] = y[1]
        x = h * (ix - Nsteps / 2)
        y = rk4Algor(x, h, 2, y, f)
    y[0] = -1.E-15          # - slope: even; reverse for odd
    y[1] = -np.sqrt(-E * 0.4829) * y[0]
    for ix in range(Nsteps - 1, nL + 2, -1):        # Right WF
        x = h * (ix + 1 - Nsteps / 2)          # Integrate in
        y = rk4Algor(x, -h, 2, y, f)
        xR.append(x)
        Rwf.append(y[0])
    x = x - h
    normL = y[0] / yL[0][nL]
    for ix in range(0, nL + 1):   # Normalize L wf & derivative
        x = h * (ix - Nsteps / 2 + 1)
        y[0] = yL[0][ix] * normL
        y[1] = yL[1][ix] * normL
        xL.append(x)
        Lwf.append(y[0])        # Factor for scale


fig = plt.figure()
ax = fig.add_subplot(111)
ax.grid()  # j +=1

for count in range(0, Nmax):              # Main program
    E = (Emax + Emin) / 2.                # Bisec E range
    Diff = diff(h, E)
    Etemp = E
    E = Emax
    diffMax = diff(h, E)
    E = Etemp
    if (diffMax * Diff > 0):
        Emax = E    # Bisection algor
    else:
        Emin = E
    print("Iteration, E =", count, E)
    if (abs(Diff) < eps):
        break
    if count > 3:
        fig.clear()               # Erase previous figure
        plot(h)
        plt.plot(xL, Lwf)
        plt.plot(xR, Rwf)
        plt.text(3, -200, 'Energy= %10.4f' % (E), fontsize=14)
        plt.pause(0.8)  # Pause to delay figures

    plt.xlabel('x')
    plt.ylabel('$\psi(x) $', fontsize=18)
    plt.title('R & L Wavefunctions Matched at x = 0')

print("Final eigenvalue E =", E)
print("Iterations = ", count, ", max = ", Nmax)
plt.show()
