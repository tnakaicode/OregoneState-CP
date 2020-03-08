! integrate.f90: Integrate exp(-x) using trapezoid, Simpson and Gauss rules 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
				program integrate
				implicit none
! declarations 
				Real*8 ::trapez, simpson, quad, r1, r2, r3
				Real*8 ::theo, vmin, vmax
				Integer ::i
!
! theoretical result, integration range
				theo = 0.632120558829
				vmin=0.0
				vmax=1.0
				open(6, File='integ.dat', Status='Unknown')
! calculate integral using both methods for steps = 3..501
				do	i=3, 501 , 2
					r1=trapez(i, vmin, vmax)
					r1=abs(r1-theo)
					r2=simpson(i,vmin, vmax)
					r2=abs(r2-theo)
					r3=quad(i,vmin, vmax)
					r3=abs(r3-theo)
					write(6,*) i, r1, r2, r3			
			 end do
	close(6)
				stop 'data saved in integ.dat'
				end program integrate
!
! the function we want to integrate
				function f(x)
				implicit none
				Real*8 ::f, x
					f=exp(-x)
				return
				end
!
! trapezoid rule
				function trapez(i, min, max)
				implicit none
				integer ::i, n				
				Real*8 ::f, interval, min, max, trapez, x
				trapez=0		 
				interval = ((max-min) / (i-1))
! sum the midpoints
				do	n=2, (i-1)						
					x = interval * (n-1)
					trapez = trapez + f(x)*interval
				end do 
! add the endpoints	 
				trapez = trapez+0.5*(f(min)+f(max))*interval
				return
				end
!
! Simpson's rule
				function simpson(i, min, max)
				implicit none
				integer ::i, n				
				Real*8 ::f, interval, min, max, simpson, x
				simpson=0		 
				interval = ((max-min) / (i-1))
! loop for odd points
				do	n=2, (i-1), 2						
					x = interval * (n-1)
					simpson = simpson + 4*f(x)
				end do 
! loop for even points
				do	n=3, (i-1), 2						
					x = interval * (n-1)
					simpson = simpson + 2*f(x)
				end do	
! add the endpoints	 
				simpson = simpson+f(min)+f(max)
				simpson=simpson*interval/3
				return
				end
!
! Gauss' rule
				function quad(i, min, max)
				implicit none
				real*8 ::w(1000), x(1000)
				real*8 ::f, min, max, quad
				integer ::i, job, n
				quad=0
				job=0
				call gauss(i, job, min, max, x, w)
				do	n=1, i
					quad=quad+f(x(n))*w(n)
				end do
				return 
				end

!gauss.f90: Points and weights for Gaussian quadrature								 
!		rescale rescales the gauss-legendre grid points and weights
!
!		npts		 number of points
!		job = 0	 rescalling uniformly between (a,b)
!					1	 for integral (0,b) with 50% points inside (0, ab/(a+b))
!					2	 for integral (a,inf) with 50% inside (a,b+2a)
!		x, w		 output grid points and weights.
!
			subroutine gauss(npts,job,a,b,x,w) 
			integer ::npts,job,m,i,j 
			real*8 ::x(npts),w(npts),a,b,xi
			real*8 ::t,t1,pp,p1,p2,p3,aj
			real*8 ::eps,pi,zero,two,one,half,quarter
			parameter (pi = 3.14159265358979323846264338328, eps = 3.0E-14)
			parameter (zero=0.0d0,one=1.0d0,two=2.0d0)
			parameter (half=0.5d0,quarter=0.25d0)


			m=(npts+1)/2
			do	i=1,m
				 t=cos(pi*(i-quarter)/(npts+half))
 10		continue
				 p1=one
				 p2=zero
				 aj=zero
				 do	 j=1,npts
						p3=p2
						p2=p1
						aj=aj+one
						p1=((two*aj-one)*t*p2-(aj-one)*p3)/aj
				 end do
				 pp=npts*(t*p1-p2)/(t*t-one)
				 t1=t
				 t=t1-p1/pp
!
				 if(abs(t-t1)>eps) goto 10
!
				 x(i)=-t
				 x(npts+1-i)=t
				 w(i)=two/((one-t*t)*pp*pp)
				 w(npts+1-i)=w(i)
			 end do
!
! rescale the grid points 
	 select case(job)		
					case (0)
!			scale to (a,b) uniformly
				 do	 i=1,npts
						x(i)=x(i)*(b-a)/two+(b+a)/two
						w(i)=w(i)*(b-a)/two
				 end do
					 
					 case(1) 
! scale to (0,b) with 50% points inside (0,ab/(a+b))
				 do	 i=1,npts
						xi=x(i)
						x(i)=a*b*(one+xi)/(b+a-(b-a)*xi)
						w(i)=w(i)*two*a*b*b/((b+a-(b-a)*xi)*(b+a-(b-a)*xi))
				 end do
				 
						case(2) 
! scale to (a,inf) with 50% points inside (a,b+2a)
				 do	 i=1,npts
						xi=x(i)
						x(i)=(b*xi+b+a+a)/(one-xi)
						w(i)=w(i)*two*(a+b)/((one-xi)*(one-xi))
				 end do
					 
			end select
!
			return
			end

