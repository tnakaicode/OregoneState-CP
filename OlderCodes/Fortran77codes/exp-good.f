c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                           
c	                                                                 
c  exp-good.f:  good algorithm for calculating exponential
c               related programs: exp-bad.f

      Program expgood
			
 	      Implicit none
c                             limit for accuracy, max in x, step in x
       	Real*8 element, min, max, step, sum, x
	      Integer i, j, n
				
	      min = 1E-10
	      max = 10.0
	      step = 0.1
	      Open(6, File = 'exp-good.dat', Status = 'Unknown')
c                                                           execution
	      Do 10 x = 0, max, step
	        sum = 1
	        element = 1
	        Do 20 n = 1, 10000
	          element = element*(-x)/n
	          sum = sum + element
	    if ((abs(element/sum) .lt. min) .AND. (sum .ne. 0)) then 
	            Write (6,*) x, sum
c                       N.B. since no "while" in f77, need use "goto"
	            GoTo 10
	          Endif       
 20	      Continue
 10 	  Continue
        Close(6)
        Stop 'data saved in exp-good.dat'
      End
       
