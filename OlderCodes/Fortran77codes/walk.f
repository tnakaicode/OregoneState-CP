c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                                                        
c    		
c  walk.f: random walk simulation                               
c	        If your compiler complains about drand48, seed48,         
c         replace drand48 with rand(seed) and remove call to seed48	                                             
c         Data output as sqrt(steps), distance                      
c
   	  Program walk
   	    Implicit none
      	Real*8 drand48, theta, x, y 
          Integer i, j, max, seed
c                                         set parameters (# of steps)
	      max = 10000
	      seed = 11168
c                                           open file, seed generator
	      Open(6, file = 'walk.dat', Status = 'Unknown')
	      Call seed48(seed)
c                                                      take max steps
          Do 20 i = 1, max
            x = x + (drand48()-0.5)*2. 
            y = y + (drand48()-0.5)*2.  
						Write (6, *) x, y
 20       Continue
 	      Close(6)
	      Stop 'data saved in walk.dat'
	    End
