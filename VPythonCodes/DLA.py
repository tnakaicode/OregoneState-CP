  """ From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
   by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2018. 
    Please respect copyright & acknowledge our work."""

# DLA.py:   Diffusion Limited aggregation 

from vpython import *
import  numpy as np, random
Maxx=500;    Maxy=500             #canvas width, height
escene = canvas(width=Maxx, height=Maxy,
	title='Diffusion Limited Aggregation', range=40)
escene.center=vector(0,0,15)                

def gauss_ran():
    r1 = random.random()
    r2 = random.random()
    fac = sqrt(-2*log(r1))
    mem = int(fac*20000*cos(2*pi*r2))
    return mem
    
rad = 40.;   step = 0;  trav = 0;   size = 60;   max = 500                           
grid =np.zeros((size,size))# Particle locations, 1=occupied 
ring(pos=vector(0,0,0),axis=vector(0,0,1),radius=rad,
	             thickness=0.5,color=color.cyan)
grid[30,30] = 1                 # Particle in center
sphere(pos=vector(4*30/3-40,-4*30/3+40,0),
	      radius=.8,color=color.green)  
ball=sphere(radius=0.8)             # Moving ball
                    
while True:                  # Generates new ball  
    hit = 0                              # No hit 
    angle = 2. *pi * random.random()  
    x =rad * cos(angle)         
    y =rad * sin(angle)         
    dist = abs(gauss_ran() )        # Length of walk 
    # print(dist)     # Uncomment to see start point
    # sphere(pos=vector(x,y),color=color.magenta) 
    trav = 0
    ballcolor=(color.yellow)
    while( hit==0 and x<40 and x>-40 and y<40 
    	      and y>-40 and trav < abs(dist)):
        if(random.random() <0.5): step=1    
        else: step =- 1;
        xg = int(0.7*x+30)      # transform coord for indexes
        yg = int(-0.7*y+30+0.5) # xg=m*x*b, 30=0*m+b, 58=m*40+b
        if ((grid[xg+1,yg] + grid[xg-1,yg]
        	+ grid[xg,yg+1] + grid[xg,yg-1]) >= 1):
          hit = 1                     # Ball hits fixed ball
          grid[xg,yg] = 1            # Position now occupied
          sphere(pos=vector(x,y,0),radius=0.8,color=color.yellow)
        else:
            if (random.random() < 0.5): x += step  # Move right
            else: y += step             # Prob = 1/2 to move up
            xp = 80*x/56.0-40                  
            yp = -80*y/56.+40                          
            ball.pos = vector(xp,yp,0)      
            rate(10000)                     # Change ball speed
        trav = trav + 1     #increments distance, < dist 
