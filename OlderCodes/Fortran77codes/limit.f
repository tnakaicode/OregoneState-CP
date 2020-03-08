c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                       
c 
c  limit.f: determines the machine precision               
c  
      Program limit
			
        Implicit none
        Integer I, N
        Real*8 eps, one
c                                              number of iterations N
        N = 60
c                                                  set initial values
        eps = 1
        one = 1
c                                     add eps to one and print result
        Do 15, I = 1, N
          eps = eps / 2
          one = 1 + eps
          Write (*, *) I, one, eps
 15     Continue   
        Stop 'limit'
      End
