!	 qmc.f90:		 Feynman path integral (quantum Monte Carlo) for	
!	 ground state wave function																					 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!				
		program qmc
	Implicit none
				integer ::i, j, max, element, prop(100)
				real*8 ::change, random, energy, newE, oldE, out, path(100)
	max	 = 250000
	open(9, FILE='qmc.dat', Status='Unknown')
				 
! initial path and initial probability 
	do	j=1,100
		path(j)=0.0
		prop(j)=0 
		end do
! find energy of initial path
	oldE = energy(path, 100)
do	i=1,max
! pick one random element
		element = random()*100+1
! change it by an random value -0.9..0.9
			change	= ((random()-0.5)*2)
			path(element)=path(element)+change
! find the new energy
		newE=energy(path, 100)
! reject change if new energy is greater and the Boltzmann factor
! is less than another random number 
					if ((newE>oldE) .AND. (exp(-newE+oldE)<random())) then
						 path(element)=path(element)-change
					endif
! add up probabilities
		do	j=1,100
			element=path(j)*10+50						 
			prop(element)=prop(element)+1
			end do
		oldE = newE
 end do
! write output data to file
	do	j=1,100
		out=prop(j)
		write(9,*) j-50, out/max
		end do
	close(9)
	stop 'data saved in qmc.dat'
	end program qmc
!
! function calculates energy of the system
	function energy(array, max)
	implicit none
	integer ::i, max
	real*8 ::energy, array(max)
	energy=0
	do	i=1,(max-1)
		 energy=energy + (array(i+1)-array(i))**2 + array(i)**2
		end do
	return
	end
		
