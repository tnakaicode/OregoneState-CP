c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                                
c   
c	    random.f: simple random number generator, not for serious work
c  
      Program random
			
      	Implicit none
       	Integer i, number, old, seed, x, y
c                                  set seed, number generated numbers
       	seed = 11
       	number = 1000
       	Open(6, file = 'random.dat', Status = 'Unknown')
        old = seed
c                                                           execution
      	Do 10 i = 1, number
          x = Mod((57*old+1), 256)
          y = Mod((57*x+1), 256)
          Write (6, *) x, y
          old = y 
 10     Continue
        Close(6)
        Stop 'data saved in random.dat'
      End
