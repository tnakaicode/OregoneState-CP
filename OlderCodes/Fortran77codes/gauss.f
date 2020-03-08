c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation   
c
c  gauss.f: Points and weights for Gaussian quadrature  
c           error message if subroutine called without a main
c           this file must reside in same directory as integ.c 
c     npts = number of points
c     job  = 0  rescalling uniformly between (a, b)
c      1  for integral (0, b) with 50% points inside (0, ab/(a+b))
c      2  for integral (a, inf) with 50% inside (a, b+2a)
c     x, w     output integration points and weights.
c
      subroutine gauss(npts, job, a, b, x, w) 
			
        integer npts, job, m, i, j 
        real*8 x(npts), w(npts), a, b, xi
        real*8 t, t1, pp, p1, p2, p3, aj
        real*8 eps, pi, zero, two, one, half, quarter
        parameter (pi =3.14159265358979323846264338328, eps = 3.E-14)
        parameter (zero = 0.d0, one = 1.d0, two = 2.d0)
        parameter (half = 0.5d0, quarter = 0.25d0)
				
        m = (npts+1)/2
        do 1020 i = 1, m
          t = cos(pi*(i-quarter)/(npts+half))
 1000   continue
        p1 = one
        p2 = zero
        aj = zero
        do 1010 j = 1, npts
          p3 = p2
          p2 = p1
          aj = aj+one
          p1 = ((two*aj-one)*t*p2-(aj-one)*p3)/aj
 1010   continue
        pp = npts*(t*p1-p2)/(t*t-one)
        t1 = t
        t = t1-p1/pp
        if ( abs(t-t1) .gt. eps ) goto 1000
        x(i) = -t
        x(npts+1-i) = t
        w(i) = two/((one-t*t)*pp*pp)
        w(npts+1-i) = w(i)
 1020   continue
c                 rescales the gauss-legendre grid points and weights
        if (job .eq. 0) then
c                                           scale to (a, b) uniformly
          do 1030 i = 1, npts
            x(i) = x(i)*(b-a)/two+(b+a)/two
            w(i) = w(i)*(b-a)/two
 1030     continue
          elseif ( job .eq. 1 ) then
c                scale to (0, b) with 50% points inside (0, ab/(a+b))
            do 1040 i = 1, npts
             xi = x(i)
              x(i) = a*b*(one+xi)/(b+a-(b-a)*xi)
              w(i) = w(i)*two*a*b*b/((b+a-(b-a)*xi)*(b+a-(b-a)*xi))
 1040       continue
          elseif ( job .eq. 2 ) then
c                  scale to (a, inf) with 50% points inside (a, b+2a)
          do 1050 i = 1, npts
            xi = x(i)
            x(i) = (b*xi+b+a+a)/(one-xi)
            w(i) = w(i)*two*(a+b)/((one-xi)*(one-xi))
 1050     continue
          else
            pause 'Wrong value of job'
        endif
        Return
        end
