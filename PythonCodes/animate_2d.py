"""
base from animete_decay.py
"""
import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import os
from matplotlib.animation import FuncAnimation
from optparse import OptionParser

sys.path.append(os.path.join('../'))
from base import plot2d, create_tempnum


class Animate2D (plot2d):
    def __init__(self):
        plot2d.__init__(self, aspect="auto")
        self.axs.set_ylim(-1.1, 1.1)
        self.axs.set_xlim(0, 10)

        self.init()
        self.ani = FuncAnimation(self.fig, self.run, frames=self.data_gen,
                                 blit=False, interval=0.1, repeat=True, init_func=self.init)

    def data_gen(self, t=0):
        cnt = 0
        while cnt < 1000:
            cnt += 1
            t += 0.05
            sys.stdout.write("\r{:d} / {:d}".format(cnt, 1000))
            sys.stdout.flush()
            yield t, np.sin(2 * np.pi * t) * np.exp(-t / 10.)

    def run(self, data):
        # update the data
        t, y = data
        self.xdata.append(t)
        self.ydata.append(y)
        self.zdata.append(y / 2)
        self.line0.set_data(self.xdata, self.ydata)
        self.line1.set_data(self.xdata, self.zdata)
        return

    def init(self):
        self.xdata = []
        self.ydata = []
        self.zdata = []
        self.line0, = self.axs.plot(self.xdata, self.ydata, lw=2)
        self.line1, = self.axs.plot(self.xdata, self.zdata, lw=2)
        return

    def SaveGif(self, gifname=None):
        if gifname == None:
            gifname = create_tempnum(self.rootname, self.tmpdir, ".gif")
        self.ani.save(gifname, writer="pillow")

    def SaveGif_Serial(self, gifname=None):
        if gifname == None:
            gifname = create_tempnum(self.rootname, self.tmpdir, ".gif")
        else:
            rootname, ext_name = os.path.splitext(gifname)
            gifname = create_tempnum(rootname, "", ".gif")
            print(gifname)
        self.ani.save(gifname, writer="pillow")


if __name__ == '__main__':
    argvs = sys.argv
    parser = OptionParser()
    parser.add_option("--dir", dest="dir", default=None)
    opt, argc = parser.parse_args(argvs)
    print(opt, argc)

    obj = Animate2D()
    # plt.show()
    obj.SaveGif_Serial("./temp_20200312000/animate_2d.gif")
    obj.SaveGif()
    obj.SaveGif("./animate_2d.gif")
    plt.close()
