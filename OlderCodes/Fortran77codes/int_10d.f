c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c  
c  int_10d.f: 10-D integration using Monte-Carlo            
c            If your compiler complains about drand48, seed48   
c            replace drand48 with rand(seed) and remove seed48 call
 
      Program int10d
			
      	Implicit none 
      	Real*8 drand48, x, y
      	Integer i, j, n
				
      	Open(6, File = 'int_10d.dat', Status = 'Unknown')
      	Call seed48(68111)
c                  Outer loops determines number of trials = accuracy
        Do 10 i = 1, 65536
          x = 0
c                         Add 10 random numbers, square, add and save
          Do 20 j = 1, 10
            x = x+drand48()
              
 20       Continue
          y = y+x*x
          if (mod(i, 2**n) .eq. 0) then
            n = n+1
            Write (6, *) i, y/i
          endif 
 10   	Continue
 	      Close(6)
      	Stop 'data saved in int_10d.dat'
      End
