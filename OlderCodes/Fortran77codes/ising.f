c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                             
c
c  ising.f: Ising model of magnetic dipole string  
c          If your compiler complains about drand48, seed48 
c          replace drand48 with rand(seed) and remove seed48 call 
c          Plot without connecting datapoints with lines

	    Program Ising
			
	      Implicit none
	      Integer max
	      Parameter(max = 100)
	      Integer element, i, spins(max), seed, t 
	      Real*8 drand48, energy, kt, new, j, old
c             define number temperature, exchange energy, random seed
	      Parameter(kt = 100, j = -1, seed = 68111)
c                                          open files, seed generator

	      Open(8, file = 'spin-up.dat', Status = 'Unknown')
	      Open(9, file = 'spin-do.dat', Status = 'Unknown')
	      Call seed48(seed)
c                           generate a uniform configuration of spins
	      Do 10 i = 1,max
          spins(i) = 1                     
 10     Continue
c                                                   step through time
	      Do 20 t = 1, 500
	        old = energy(spins, j, max)
	        element = drand48()*max+1
             
c                                                           flip spin
	        spins(element) = spins(element)*(-1)
	        new = energy(spins, j, max)
c                                                Metropolis algorithm
          If (new.GT.old .AND. exp((-new+old)/kt) .LT.drand48()) Then
            
             spins(element) = spins(element)*(-1)
          Endif
	        Do 30 i = 1,max
	        If (spins(i) .EQ. 1) Then
	          Write(8,*) t, i
	        Endif
	        If (spins(i) .EQ. (-1)) Then
	          Write(9,*) t, i
	        Endif
 30 	    Continue	 
 20     Continue
 	      Close(8)
 	      Close(9)
        Stop 'data saved in spin-up.dat, spin-do.dat'
 	    End 

	     Function energy(array, j, max)
	       Implicit none
	       Integer array(max), i, max
	       Real*8 energy, j
	       energy = 0
	       Do 22 i = 1,(max-1)
	         energy = energy+array(i)*array(i+1)
 22	     Continue
	       Return
	    End
