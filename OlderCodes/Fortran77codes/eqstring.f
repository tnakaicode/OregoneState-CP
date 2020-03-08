c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                           
c    			                                                             
c  eqstring.f: Solution of  wave equation using time stepping         
c  comment: Output data is saved in 3D grid format used by gnuplot 

      Program string
			
        Implicit None
      	Real*8 x(101,3)
      	Integer i, k, max
				
      	max = 100
      	Open(9,file = 'eqstring.dat',status = 'Unknown')   
c                                                   initialize values
	      Do 10 i = 1,80
           x(i,1) = 0.00125*i
 10   	Continue
        Do 20 i = 81,101
          x(i,1) = 0.1-0.005*(i-81)
 20  	  Continue
c                                                 the first time step
	      Do 30 i = 2,100
          x(i,2) = x(i,1)+0.5*(x(i+1,1)+x(i-1,1)-2.0*x(i,1))
 30   	Continue
c                                                     all other times
        Do 40 k = 1,max
          Do 50 i = 2,100
            x(i,3) = 2.0*x(i,2)-x(i,1)+(x(i+1,2)+x(i-1,2)-2.0*x(i,2))
 50       Continue
          Do 60 i = 1,101
c                                                          new -> old
            x(i,1) = x(i,2)
            x(i,2) = x(i,3)
 60       Continue
c                                               output every 10 steps
          If ( Mod(k,10) .EQ. 0 ) then
            Do 70 i = 1,101
              Write(9,11) x(i,3)
 70	    Continue
            Write(9,11)
          EndIf
 40   	Continue
 11    Format (e12.6)
 	Close(9)
      	Stop 'data saved in eqstring.dat'
      	End

