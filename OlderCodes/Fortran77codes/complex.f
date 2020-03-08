c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation   
c								                                             
c  complex.f: Dealing with complex numbers on a comput        
c 
      Program complex 
			
        Implicit none 
        Complex*16 z, zsqrt, zlog
       	Real*8 i, pi, phi, x, y, zatan, zatan2
       	pi = 3.1415926535897932385E0
       	Write (*,10) 'phi', 'x', 'y', 'sqrt', 'log', 'atan', 'atan2'
       	Write (*, *) ' '
c                                                      loop for angle
 	      Do 100 i = 0, 2.6, 0.1
          phi = i * pi
c                                 calculate carthesian representation
          x = cos(phi)
          y = sin(phi)
          z = cmplx(x, y)
          zsqrt = sqrt(z)
          zlog = log(z)
          zatan = atan(y/x)
          zatan2 = atan2(y, x)
          Write (*, 20) i, '*pi', x, y, zsqrt, 'i', zlog, 
					                                 'i', zatan, zatan2
 100   	Continue
 10    	Format (a4, 2a9, a14, a18, a14, a10)
 20    	Format (f3.1, a3, 3f9.4, f8.4, a1, f9.4, f8.4,a1, 2f9.4)
	      Stop 'complex'
      End
