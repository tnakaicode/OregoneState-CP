c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c    			   						
c  pond.f: Calculate pi using Monte-Carlo integration (throw stones)
c          If your compiler complains about drand48, seed48,
c          replace drand48 with rand(seed) and remove seed48 call                                         
 
	    Program pond
			
	      Implicit none 
c          drand48 function, max number of stones, seed for generator
	      Real*8 area, x, y, drand48
	      Integer i, max, pi, seed
				
	      max = 2000
	      seed = 68111 
c                        open file, set initial value, seed generator
	      Open(6, File = 'pond.dat', Status = 'Unknown')
	      pi = 0
	      Call seed48(seed) 
	      Do 10 i = 1, max
	        x = drand48()*2-1
	        y = drand48()*2-1
            If ((x*x + y*y) .LE. 1) Then
	          pi = pi+1
	        Endif
	        area = 4. * pi/Real(i)
	        Write(6, *) i, area
 10 	  Continue
 	      Close(6)
 	      Stop 'data saved in pond.dat'
 	    End
