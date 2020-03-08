c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                               
c    			 
c  integ.f: Integrate exp(-x) using trapezoid, Simpson & Gauss rules
c           gauss.f must be included 
c           derivation from the exact output as x y1 y2           

      Program integrate
			
      	Implicit none
      	Real*8 trapez, simpson, quad, r1, r2, r3
      	Real*8 theo, vmin, vmax
      	Integer i
c                               theoretical result, integration range
      	theo = 0.632120558829
      	vmin = 0.
      	vmax = 1.
      	Open(6, File = 'integ.dat', Status = 'Unknown')
c                 calc integral using both methods for steps = 3..501
        Do 50 i = 3, 501 , 2
          r1 = trapez(i, vmin, vmax)
          r1 = abs(r1-theo)
          r2 = simpson(i, vmin, vmax)
          r2 = abs(r2-theo)
          r3 = quad(i, vmin, vmax)
          r3 = abs(r3-theo)
          write(6, *) i, r1, r2, r3      
 50     Continue
 	      Close(6)
        Stop 'data saved in integ.dat'
      End
c                                               function to integrate
      Function f(x)
        Implicit none
      	Real*8 f, x
        f = exp(-x)
       	Return
      End
c                                                      trapezoid rule
      Function trapez(i, min, max)
      	Implicit none
      	Integer i, n				
      	Real*8 f, interval, min, max, trapez, x
      	trapez = 0		 
      	interval = ((max-min) / (i-1))
c                                                       sum midpoints
      	Do 21 n = 2, (i-1)          	
          x = interval * (n-1)
          trapez = trapez + f(x)*interval
 21   	Continue 
c                                                       add endpoints
      	trapez = trapez+0.5*(f(min)+f(max))*interval
        Return
      End
c                                                      Simpsons rule
      Function simpson(i, min, max)
      	Implicit none
      	Integer i, n				
      	Real*8 f, interval, min, max, simpson, x
      	simpson = 0		 
      	interval = ((max-min) / (i-1))
c                                                 loop for odd points
      	Do 31 n = 2, (i-1), 2          	
          x = interval * (n-1)
          simpson = simpson + 4*f(x)
 31   	Continue 
c                                                loop for even points
      	Do 32 n = 3, (i-1), 2          	
          x = interval * (n-1)
          simpson = simpson + 2*f(x)
 32   	Continue  
c                                                       add endpoints
      	simpson = simpson+f(min)+f(max)
      	simpson = simpson*interval/3
        Return
      End
c                                                    Gauss quadrature
      Function quad(i, min, max)
      	Implicit none
      	Real*8 w(1000), x(1000)
      	Real*8 f, min, max, quad
      	Integer i, job, n
      	quad = 0
      	job = 0
      	call gauss(i, job, min, max, x, w)
      	Do 41 n = 1, i
          quad = quad+f(x(n))*w(n)
 41   	Continue
      	Return 
      	End
