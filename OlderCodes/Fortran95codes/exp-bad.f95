!	 exp-bad.f: A bad algorithm for calculating exponential									 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
Program expbad

  Implicit none
         ! min =  accuracy, x step, max in x, up numer, down denomin.
  Real*8 :: down, min, max, step, sum, up, x
	Integer :: i, j
	
	min  = 1E - 10
	max  = 10.
	step = 0.1
	open(6, File = 'exp-bad.dat', Status = 'Unknown')
	                                                        ! summation
  Do x = 0, max, step                                           
    sum = 1
    i = 0
	  down = 1 
    up = 1
		                                      ! while loop may never stop
    Do while((sum == 0).or.(abs((up/down)/sum)  >  min))   
	    i = i + 1
      down = 1 
      up = 1
        Do  j = 1, i
	        up   = - up*x
	        down = down*j
        End Do
        sum = sum + up/down
    End Do
    write (6, *) x, sum
  End Do
  close(6)
  Stop 'data saved in exp-bad.dat'
End Program expbad
