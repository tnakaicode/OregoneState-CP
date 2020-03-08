c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                             
c                                                              
c  eqheat.f: Solution of heat equation using with finite differences                                                 
c            Output data is saved in 3D grid format used by gnuplot 

	    Program heat
			
        Implicit None       
      	Double Precision cons, ro, sph, thk, u(101,2)
      	Integer i, k, max
				
      	Open(9,file = 'eqheat.dat',status = 'Unknown')
c            specific heat, thermal conductivity and density for iron
       	sph = 0.113
	      thk = 0.12
	      ro = 7.8
        cons = thk/(sph*ro)
c                                                number of iterations
    	  max = 30000
c                                    At t = 0 all points are at 100 C
      	Do 10 i = 1,100
          u(i,1) = 100.0
 10   	Continue
c                                                      endpoints zero
      	Do 20 i = 1,2
          u(1,i) = 0.0
          u(101,i) = 0.0
 20     Continue
c 			                                    start solution, time loop
      	Do 100 k = 1,max
c                               loop over space, endpoints stay fixed
          Do 30 i = 2,100
            u(i,2) = u(i,1) + cons*(u(i+1,1) + u(i-1,1)-2*u(i,1))
 30       Continue
c                                        output every 1000 time steps
          If ( (Mod(k,1000) .eq. 0)  .or.  (k .eq. 1) ) Then
            Do 40 i = 1,101,2
              Write(9,22)u(i,2)
 40         Continue
            Write (9,22)
          EndIf
c                                                          new -> old
          Do 50 i = 2,100
            u(i,1) = u(i,2)
 50       Continue
 100  	Continue
 22   	Format (f10.6)
 	      Close(9)
      	Stop 'data saved in eqheat.dat'
      End
