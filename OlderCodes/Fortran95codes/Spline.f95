! spline.f90: Cubic Spline fit to data, based on " Numerical Recipes in C " 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
program spline
implicit none
!	 input array x[n], y[n] represents tabulation function y(x)
! with x0 < x1 ... < x(n-1). n = # of tabulated points
!output yout for given xout (here xout via loop at end)
!yp1 and ypn: 1st derivatives at endpoints, evaluated internally
!y2[n]	is array of second derivatives
!(setting yp1 or	ypn >0.99e30 produces natural spline)
real*8 :: xout, yout,h,b,a, Nfit,p,qn,sig,un,yp1,ypn,x(9),y(9),y2(9),u(9)
integer :: klo,khi,k,n,i,np 
!save data in Spline.dat 
open(9, FILE='Spline.dat', Status='Unknown')
!input data	 
open(10, FILE='Input.dat', Status='Unknown')
!you may enter your own data here!
data x / 0., 1.2, 2.5, 3.7, 5., 6.2, 7.5, 8.7, 9.9/ 
data y / 0., 0.93, 0.6, -0.53, -0.96, -0.08, 0.94, 0.66, -0.46 /
n=9
np = 15
! save input data in Input.dat file
do i=1,n
write(10,*) x(i), y(i)
end do
Nfit=3000;
! enter the desired number of points to fit

yp1	 =	(y(2)-y(1))/(x(2)-x(1))- (y(2)-y(2))/(x(3)-x(2))+ (y(3)-y(1))/(x(3)-x(1))	 ! 1st deriv at initial point
		 
		
ypn = (y(n-1)-y(n-2))/(x(n-1)-x(n-2))- (y(n-2)-y(n-3))/(x(n-2)-x(n-3))+(y(n-1)-y(n-3))/(x(n-1)-x(n-3))
 ! 1st deriv at end point
		
		 
if (yp1 > 0.99e30) then 
	y2(1)=0.0
	u(1)=0.0
else 
		y2(1)	 =	(-0.5)
		u(1) = (3.0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
endif				 
do i=2,n-1	 ! decomposition loop; y2, u are temps
		
		sig = (x(i)-x(i-1))/(x(i+1)-x(i-1));
		p = sig*y2(i-1)+2.0
		y2(i) = (sig-1.0)/p
		u(i) = (y(i+1)-y(i))/(x(i+1)-x(i)) - (y(i)-y(i-1))/(x(i)-x(i-1))
		u(i) = (6.0*u(i)/(x(i+1)-x(i-1))-sig*u(i-1))/p;
end do	
if (ypn > 0.99e30) then 
	qn =0.0
	un = 0.0 !test for natural
else											 ! else evaluate second derivative
			qn = 0.5
			un = ((3.0)/(x(n-1)-x(n-2)))*(ypn-(y(n-1)-y(n-2))/(x(n-1)-x(n-2)))
			y2(n-1) = (un-qn*u(n-2))/(qn*y2(n-2)+1.0)
endif
	do k = n-2, 1,-1
	 y2(k) = y2(k)*y2(k+1)+u(k)		 ! back substitution
	end do			 
 ! splint (initialization) ends

 ! Parameters determined, Begin *spline* fit
do i=1,Nfit !loop over xout values
		
		xout = x(1) + (x(n)-x(1))*(i)/(Nfit)
		
		klo = 0								 !Bisection algor to find place in table
		khi = n-1							 ! klo, khi bracket xout value 
		do while (khi-klo > 1) 
				k = (khi+klo)/2.0
				if (x(k) > xout) then
				khi = k 
				else 
				klo = k			
				endif	 
		end do			
		h = x(khi)-x(klo)
		if (x(k) > xout) then 
		khi = k
		else 
		klo = k
		endif	 
		h = x(khi)-x(klo)
		a = (x(khi)-xout)/h
		b = (xout-x(klo))/h
		yout = (a*y(klo)+b*y(khi)+((a*a*a-a)*y2(klo)+(b*b*b-b)*y2(khi))*(h*h)/6.0)
		! write data using gnuplot 2D format 
		write (9,*) xout,yout	 
		
end do
stop 'data stored in Spline.dat' 
end program spline
