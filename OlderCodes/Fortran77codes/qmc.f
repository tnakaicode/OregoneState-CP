c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                               
c    			 
c  qmc.f: Feynman path integral for ground state wave function 
c         If your compiler complains about drand48, srand48, 
c           uncomment the define statements further down.  

     	Program qmc
			
	      Implicit none
        Integer i, j, max, element, prop(100)
      Real*8 change, drand48, energy, newE, oldE, out, path(100)
				
	      max = 250000
	      Open(9, file = 'qmc.dat', Status = 'Unknown')
	      call seed48(68111)       
c                                initial path and initial probability
	      Do 10 j = 1, 100
	        path(j) = 0.
	        prop(j) = 0 
 10 	  Continue
c                                         find energy of initial path
	      oldE = energy(path, 100)
	      Do 20 i = 1, max
c                                             pick one random element
	        element = drand48()*100+1
c                                  change by random value -0.9..0.9
  	        change = ((drand48()-0.5)*2)
            path(element) = path(element)+change
c                                                 find the new energy
	        newE = energy(path, 100)
c                                                    Metrop algorithm
        If ((newE.GT.oldE) .AND. (exp(-newE+oldE).LT.drand48())) Then
         path(element) = path(element)-change
          Endif
c                                                add up probabilities
	      Do 30 j = 1, 100
	        element = path(j)*10+50	  	  	   
	        prop(element) = prop(element)+1
 30 	  Continue
 	      oldE = newE
 20 	  Continue
c                                                         file output
	      Do 40 j = 1, 100
	        out = prop(j)
	        Write(9, *) j-50, out/max
 40 	  Continue
 	      Close(9)
 	      Stop 'data saved in qmc.dat'
 	    End
c                                          calculate energy of system
	    Function energy(array, max)
	      Implicit none
	      Integer i, max
	      Real*8 energy, array(max)
	      energy = 0
	      Do 50 i = 1, (max-1)
	        energy = energy + (array(i+1)-array(i))**2 + array(i)**2
 50	    Continue
	      Return
	    End
 	  
