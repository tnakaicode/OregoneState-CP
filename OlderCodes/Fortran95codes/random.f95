!	 random.f90: A very simple random number generator - not suitable			 
!	 for serious work																					 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!			 
				program random
				implicit none
! declarations
				integer ::i, number, old, seed, x, y
! set parameters (seed for generator, number of generated numbers)
				seed = 11
				number = 1000
! open output file, seed number generator
				open(6, FILE='random.dat', Status='Unknown')
				old = seed
! execution 
				do	i = 1, number
					 x = modulo((57*old+1), 256)
					 y = modulo((57*x+1), 256)
					 write (6,*) x, y
					 old=y 
				end do
!
			close(6)
			stop 'data saved in random.dat'
			end program random
