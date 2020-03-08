c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                             
c 
c  decay.f: Spontaneous radioactive decay simulation                
c           If compiler complains about drand48, seed48, replace
c           drand48 with rand(seed) and remove the seed48 call 	
c
      Program decay
			
      	Implicit none
      	Real*8 r, drand48, lambda
      	Integer i, j, h, nleft, nloop, start, seed
				
c Set parameters (decay rate, initial no of atoms, seed), plant seed
      	lambda = 0.01
      	start = 1000 
      	seed = 11168  
      	h = 1
      	nloop = start
      	nleft = start
      	call seed48(seed)
      	Open(6, File = 'decay.dat')
c                                      loop over times and over atoms
      	Do 20 j = 1, 10000
          Do 10 i = 1, nleft
             r = drand48()
             
             IF (r .LE. lambda) Then
               nloop = nloop -1
             EndIF
 10       Continue
c                                                      atom loop ends
          nleft = nloop
          Write (6, *) h, ' ', Real(nleft)/start
          h = h + 1
          If (nleft .eq. 0) Goto 30
 20   	Continue
 30     Close(6) 
      	Stop 'data saved in decay.dat'
      End
