c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                          
c                                                
c  bound.f:  bound states in momentum space of delta shell potential
c      uses LAPACK SGEEV, 16 grid points, l = 0, lambda variable                      
c      NB: results are NOT stable to continuing increase in Npts!                               

      Program bound
			
	        Integer n, Size, job, info, i, j
	        Real b
	        Parameter (Size = 100, pi = 3.141592, b = 10.0)
	        Real lambda, scale
	        Double Precision Pot
	        Complex x(Size),V(Size, Size)
	        Real H(Size,Size), Work(0:Size,0:Size), k(Size), w(Size)
c	                                           Enter potential strength
	        Write(*,*) 'enter lambda'
	        Read(*,*) lambda
	        Write(*,*) 'enter scaling factor'
	        Read(*,*) scale
	        Write(*,*) 'number of points'
	        Read(*,*) n
c
c	      set up Gauss points & weights on the interval [0,inf]
c	      scale is the "midpoint" of the integration on the infinite
c
	        Call Gauss(n,2,0.0,scale,k,w)
c                                                     Set up V and H
	        DO i = 1,n
		        DO j = 1,n
			        Pot = -1.0*lambda*SIN(b*k(i))*SIN(b*k(j))
			        Pot = Pot/(k(i)*k(j))
			        H(i,j) = w(j)*k(j)*k(j)*2.0*Pot/pi
			        If (i .EQ. j) Then
				        H(i, j) = H(i, j)+k(i)*k(i)
			        Endif
		      End Do
	       End Do
	       job = 1
	       Call sgeev(h,  Size, n, x, V,Size, Work, job, info)
	       Write(*,*) info
	      Do i = 1, n
		      Write(*, *) x(i)
	      End Do
	    End
