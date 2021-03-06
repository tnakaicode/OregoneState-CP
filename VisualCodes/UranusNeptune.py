""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# UranusNeptune.py: Orbits of Neptune & Uranus

from vpython.graph import *
scene = display(width=600,height=600,  
	title =  'White Neptune & Black Uranus', range=40)
sun = sphere(pos=vector(0,0,0), radius=2, color=color.yellow)
escenau = gdisplay(x=600,width=400,height=400, 
	title='Pertubation of Uranus Angular Position')
graphu = gcurve(color=color.cyan)

escenan = gdisplay(x=800,y=400,width=400,height=400)
graphn = gcurve(color=color.white)
rfactor = 1.8e-9
G = 4*pi*pi         # in units T in years, R AU, Msun=1
mu = 4.366244e-5    # mass Uranus in solar masses
M = 1.0             # mass Sun
mn = 5.151389e-5    # Neptune mass in solar masses
du = 19.1914        # distance Uranus Sun in AU
dn = 30.0611        # distance Neptune sun in AU
Tur = 84.0110       # Uranus Orbital Period yr
Tnp = 164.7901      # Neptune Orbital Period yr
omeur = 2*pi/Tur    # Uranus angular velocity (2pi/T)
omennp = 2*pi/Tnp   # Neptune angular velocity
omreal = omeur
urvel = 2*pi*du/Tur    # Uranus orbital velocity UA/yr
npvel = 2*pi*dn/Tnp    # Neptune orbital velocity UA/yr
# 1 Uranus at lon 2 gr 16 min sep 1821
radur = (205.64)*pi/180.  # to radians in 1690 -wrt x-axis
urx = du*cos(radur)       # init x- pos ur. in 1690
ury = du*sin(radur)       # init y-pos ur in 1690
urvelx = urvel*sin(radur)
urvely = -urvel*cos(radur)
# 1690 Neptune at long. 
radnp = (288.38)*pi/180. # 1690 rad Neptune wrt x-axis
Uranus = sphere(pos=vector(urx,ury,0), radius=0.5,color=(.88,1,1), 
	make_trail=True)
urpert = sphere(pos=vector(urx,ury,0), radius=0.5,color=(.88,1,1), 
	make_trail=True)
fnu = arrow(pos=Uranus.pos,color=color.orange,axis=vector(0,4,0))
npx = dn*cos(radnp)       #init coord x Neptune 1690
npy = dn*sin(radnp)       #           y
npvelx = npvel*sin(radnp)
npvely = -npvel*cos(radnp)
Neptune = sphere(pos=vector(npx,npy,0), radius=0.4,color=color.cyan, 
	make_trail=True)
fun = arrow(pos=Neptune.pos,color=color.orange,axis=vector(0,-4,0))
nppert = sphere(pos=vector(npx,npy,0), radius=0.4, color=color.white, 
	make_trail=True)
velour = vector(urvelx,urvely,0)   #initial vector velocity Uranus
velnp = vector(npvelx,npvely,0)    #initial vector velocity Neptune
dt = 0.5                    # time increment in terrestrial year
r = vector(urx,ury,0)           # initial position Uranus wrt Sun
rnp = vector(npx,npy,0)         # initial position Neptune wrt Sun
veltot = velour
veltotnp = velnp
rtot = r
rtotnp = rnp

def ftotal(r,rnp,i):   # i==1 Uranus  i==2 Neptune
    Fus =  -G*M*mu*r/(du**3)  # Force sun over URANUS
    Fns = -G*M*mn*rnp/(dn**3) # Force Sun over NEPTUNE
    dnu = mag(rnp-r)          # distance Neptune-Uranus
    Fnu = -G*mu*mn*(rnp-r)/(dnu**3) # force N on U
    Fun = -Fnu                    # force Uranus on Neptune
    Ftotur = Fus+Fnu                # total force on U (sun + N)
    Ftotnp = Fns+Fun                # On Neptune F sun +F urn
    if i==1: return Ftotur
    else:    return Ftotnp
    
def rkn(r,veltot,rnp,m,i):    # on Neptune
    k1v = ftotal(r,rnp,i)/m
    k1r = veltot
    k2v = ftotal(r,rnp+0.5*k1r*dt,i)/m
    k2r = veltot+0.5*k2v*dt
    k3v = ftotal(r,rnp+0.5*k2r*dt,i)/m
    k3r = veltot+0.5*k3v*dt
    k4v = ftotal(r,rnp+k3r*dt,i)/m
    k4r = veltot+k4v*dt
    veltot = veltot+(k1v+2*k2v+2*k3v+k4v)*dt/6.0
    rnp = rnp+(k1r+2*k2r+2*k3r+k4r)*dt/6.0
    return r,veltot

def rk(r,veltot,rnp,m,i):   # on Uranus
    k1v = ftotal(r,rnp,i)/m
    k1r = veltot
    k2v = ftotal(r+0.5*k1r*dt,rnp,i)/m
    k2r = veltot+0.5*k2v*dt
    k3v = ftotal(r+0.5*k2r*dt,rnp,i)/m
    k3r = veltot+0.5*k3v*dt
    k4v = ftotal(r+k3r*dt,rnp,i)/m
    k4r = veltot+k4v*dt
    veltot = veltot+(k1v+2*k2v+2*k3v+k4v)*dt/6.0
    r = r+(k1r+2*k2r+2*k3r+k4r)*dt/6.0
    return r,veltot

for i in arange(0,320):# estaba 1240
    rate(10)
    rnewu,velnewu = rk(r,velour,rnp,mu,1) # Uranus
    rnewn,velnewn = rkn(rnp,velnp,r,mn,2) # Neptune
    r = rnewu                      # Uranus position
    velour = velnewu               # Uranus velocity
    du = mag(r)
    omeur = mag(velour)/du       # Angular velocity of Uranus
    degr = 205.64*pi/180- omeur*i*dt # Angular position Uranus
    rnp = rnewn                   # Neptune pos
    velnp = velnewn                # Neptune pos
    dn = mag(rnp)
    omenp = mag(velnp)/dn
    radnp = radnp - dt*omenp           # Radians Neptune
    npx = dn*cos(radnp) 
    npy = dn*sin(radnp)
    rnp = vector(npx,npy,0)       # Neptune position   
    deltaomgs = -omeur+omreal
    graphu.plot(pos=vector(i,deltaomgs*180/pi*3600))
    urpert.pos = r
    fnu.pos = urpert.pos            # position of arrow on Uranus
    dnu = mag(rnp-r)                    # distance Neptune-Uranus
    fnu.axis = 75*norm(rnp-r)/dnu   # axes  the arrow over Uranus
    Neptune.pos = rnp                       # radiovector Neptune
    fun.pos = Neptune.pos
    fun.axis = -fnu.axis            # arrow on Neptune
