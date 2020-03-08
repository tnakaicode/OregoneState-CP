! Newton_fd.f90:Newton-Raphson root finder with self-taking derivative (forward diff)
!	 (crude, but if it works, it works well) 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
program NewtonRHL_fd
implicit none
integer ::it, imax = 10 !Maximum number of iterations permitted
real *8 ::x = 2.0, dx = 1e-2, eps = 1e-6, df,F !x guess, must be close to root

! 
 !			 

do	it = 1,imax
		 
		 
		!Compute function value
		 df = ( F(x+dx) - F(x) )/dx			 ! forward difference derivative
		 dx	 =	-F(x)/df
		 x =	x+dx													 !New approximation
		 write(*,*) it, x, F(x)
		if ( abs(F(x)) <= eps	 )then !Check for convergence
		write(*,*) eps
		stop
		endif
		end do
end
function F(x)			! Will find zero of this function
implicit none
real *8 ::x,F			
F= 2*cos(x) - x
end

