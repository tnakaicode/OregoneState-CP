c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                                
c     
c  over.f: determine overflow and underflow limits  

      Program overflow
			
        Implicit none 
        Integer I, N
        Real*8 under, over
c                                              number of iterations N
        N = 1024
c                                                  set initial values
        under = 1
        over = 1
c                                          calculate, print to screen
        Do 15, I = 1, N
          under = under / 2
          over = over * 2
          Write (*, *) I, over, under
 15     Continue   
        Stop 'over'
      End
