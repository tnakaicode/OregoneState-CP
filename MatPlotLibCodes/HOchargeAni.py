""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# HOchargeMat.py charge in HO plus E field wi Matplotlib

import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os
from matplotlib.animation import FuncAnimation
from optparse import OptionParser

sys.path.append(os.path.join('../'))
from base import plot2d, create_tempnum


class HOAnimate(plot2d):

    def __init__(self, aspect='auto'):
        plot2d.__init__(self, aspect=aspect)

        self.dx = 0.06
        self.k0 = 5.5 * np.pi
        self.dt = self.dx**2 / 8.
        self.xs = np.arange(-6.0, 6.0 + self.dx / 2, self.dx)

        # Re Psi
        # Im Psi
        self.ps = np.exp(-0.5 * (self.xs / 0.5)**2)
        self.psr = self.ps * np.cos(self.k0 * self.xs)
        self.psi = self.ps * np.sin(self.k0 * self.xs)

        # E field
        # V HO + E
        self.E = 70
        self.v = 25.0 * self.xs**2 - self.E * self.xs

        self.dtdx2 = self.dt / self.dx**2
        self.dtv = self.dt * self.v[1:-1]

        pt = np.linspace(0, 100, 10)
        self.axs.set_title("Charged Harmonic Oscillator in E Field")
        self.ini()
        self.ani = FuncAnimation(
            self.fig, self.run, pt, blit=True, init_func=self.ini)

    def SaveGif(self, gifname=None):
        if gifname == None:
            gifname = create_tempnum(self.rootname, self.tmpdir, ".gif")
        self.ani.save(gifname, writer="pillow")

    def ini(self):
        self.line, = self.axs.plot(self.xs, self.psr**2 + self.psi**2, lw=2)
        self.line1, = self.axs.plot(self.xs, self.psr**2, lw=2)
        self.line2, = self.axs.plot(self.xs, self.psi**2, lw=2)
        return self.line,

    def run(self, t=0):
        self.psr[1:-1] += -self.dtdx2 * \
            (self.psi[2:] - 2 * self.psi[1:-1] + self.psi[:-2]) \
            + self.dtv * self.psi[1:-1]
        self.psi[1:-1] += +self.dtdx2 * \
            (self.psr[2:] - 2 * self.psr[1:-1] + self.psr[:-2]) \
            - self.dtv * self.psr[1:-1]
        self.line.set_data(self.xs, self.psr**2 + self.psi**2)
        self.line1.set_data(self.xs, self.psr**2)
        self.line2.set_data(self.xs, self.psi**2)
        return self.line,


if __name__ == '__main__':
    argvs = sys.argv
    parser = OptionParser()
    parser.add_option("--dir", dest="dir", default=None)
    opt, argc = parser.parse_args(argvs)
    print(opt, argc)

    obj = HOAnimate()
    obj.SaveGif()
    plt.show()
    # plt.close()
