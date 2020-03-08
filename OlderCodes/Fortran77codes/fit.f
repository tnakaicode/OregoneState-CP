c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation    
c
c  fit.f: Least square fit to decay spectrum 

      Program fit
			
        Implicit none
        Integer i, j
        Real*8 s, sx, sy, sxx, sxy, delta, inter, slope
        Real*8 x(12), y(12), d(12)
c                                             input value y, x values
        Data y /32, 17, 21, 7, 88, 6, 5, 2, 2, 80.1, 48, 1/
        Do 10 i = 1, 12
          x(i) = i*10-5
 10     Continue
        Do 11 i = 1, 12
          d(i) = 1.0
 11     Continue
c                                                      calculate sums
        Do 30 i = 1, 12
          s = s   +         1 / (d(i)*d(i))
          sx = sx  +      x(i) / (d(i)*d(i))
          sy = sy  +      y(i) / (d(i)*d(i))
          sxx = sxx + x(i)*x(i) / (d(i)*d(i)) 
          sxy = sxy + x(i)*y(i) / (d(i)*d(i))
 30     Continue
c                                              calculate coefficients
        delta = s*sxx-sx*sx
        slope = (s*sxy-sx*sy) / delta		
        inter = (sxx*sy-sx*sxy) / delta
        Write(*,*) 'intercept = ', inter
        Write(*,*) 'slope = ', slope
        Write(*,*) 'correlation = ', -sx/sqrt(sxx*s)		
        Stop 'fit'
        End
