c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                         
c                                                                      
c  exp-bad.f: bad algorithm for calculating e^-x as a finite sum               
c             related programs: exp-good.f					      

      Program expbad
			
  	    Implicit none
c          min: accuracy, step in x, max in x, up numer, down denomin
       	Real*8 down, min, max, step, sum, up, x
	      Integer i, n
				
	      min = 1E-10
	      max = 10.
	      step = 0.1
	      Open(6, File = 'exp-bad.dat', Status = 'Unknown')
c                                                           execution
	      Do 10 x = 0, max, step
	        sum = 1
          Do 20 n = 1, 10000
	          up = 1 
	          down = 1
	          Do 30 i = 1, n
	            up = -up*x
	            down = down*i
 30	        Continue
            sum = sum + up/down
            If ((abs(up/down/sum) .lt. min) .AND. (sum .ne. 0)) then
	            Write (6,*) x, sum
c                  N.B. since no "while" statement in f77 need "goto"
	            GoTo 10
	          Endif       
 20	      Continue
 10 	  Continue
        Close(6)
        Stop 'data saved in exp-bad.dat'
      End
