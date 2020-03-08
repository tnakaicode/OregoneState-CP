c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c    	
c  twodsol.f:  Solves the sine-Gordon equation for a 2D soliton 
c
      Program twodsol
			
        Implicit None
        Double Precision u(201, 201, 3)
        Integer nint
				
        Open(9, file = 'twodsol.dat', status = 'new')
        Write(*, *)'   Enter an integer from 1 to 100'
        Write(*, *)' this number is proportional to time'
        Write(*, *)'   time = 0 is for the integer = 1'
        Read(*, *)nint
        Write(*, *)'working with input = ', nint
c                                                      initialization
        Call initial(u)
c                               solution at time proportional to nint
        Call solution(u, nint)
        Stop
      End
      
      Subroutine initial(u)
			
c                          initializes the constants and 2-D soliton
        Implicit None
        Double Precision u(201,201,3), dx, dy, dt, xx, yy, dts, time
        Integer i, j
        Common /values/dx, dy, dt, time, dts    
c                                 initial condition for all xy values
        dx = 14.d0/200.d0
        dy = dx
        dt = dx/dsqrt(2.d0)
        dts = (dt/dx)**2
        yy = -7.d0
        time = 0.d0
        Do 10 i = 1, 201
          xx = -7.d0
          Do 20 j = 1, 201
            u(i, j, 1) = 4.d0*Datan(3.d0-sqrt(xx*xx+yy*yy))
            xx = xx+dx
 20       Continue
          yy = yy+dy
 10     Continue
        Return 
      End

      Subroutine solution(u, nint)
			
c       solves SGE for initial conditions in routine initial.    
        Implicit None  
      Double Precision u(201,201,3), dx, dy, dt, time, a2,zz, dts, a1
        Integer l, m, mm, k, j, i, nint
        Common /values/dx, dy, dt, time, dts
c                              these values passed by routine initial
        time = time+dt
c          2nd iteration using d phi/dt = 0 at t = 0 (G(x, y, 0) = 0)
c                               and d U/dx = 0 at -x0, x0, -y0 and y0
        Do 80 l = 2, 200
          Do 90 m = 2, 200
            a2 = u(m+1, l, 1)+u(m-1, l, 1)+u(m, l+1, 1)+u(m, l-1, 1)
            u(m, l, 2) = 0.5*(dts*a2-dt*dt*Dsin(0.25d0*a2))
 90       Continue
 80     Continue
c                                         borders in second iteration
        Do 130 mm = 2, 200
          u(mm, 1, 2) = u(mm, 2, 2)
          u(mm, 201, 2) = u(mm, 200, 2)
          u(1, mm, 2) = u(2, mm, 2)
          u(201, mm, 2) = u(200, mm, 2)
 130    Continue
c                                               still undefined terms
        u(1, 1, 2) = u(2, 1, 2)
        u(201, 1, 2) = u(200, 1, 2)
        u(1, 201, 2) = u(2, 201, 2)
        u(201, 201, 2) = u(200, 201, 2) 
        Do 100 k = 1, nint
          Do 60 l = 2, 200
            Do 70 m = 2, 200
              a1 = u(m+1, l, 2)+u(m-1, l, 2)+u(m, l+1, 2)+u(m, l-1,2)
              u(m, l, 3) = -u(m, l, 1)+dts*a1-dt*dt*Dsin(0.25d0*a1)
              u(m, 1, 3) = u(m, 2, 3)
              u(m, 201, 3) = u(m, 200, 3) 
 70         Continue
 60       Continue
          Do 140 mm = 2, 200
            u(mm, 1, 3) = u(mm, 2, 3)
            u(mm, 201, 3) = u(mm, 200, 3)
            u(1, mm, 3) = u(2, mm, 3)
            u(201, mm, 3) = u(200, mm, 3)
 140      Continue
          u(1, 1, 3) = u(2, 1, 3)
          u(201, 1, 3) = u(200, 1, 3)
          u(1, 201, 3) = u(2, 201, 3)
          u(201, 201, 3) = u(200, 201, 3)
c                                                         new i-> old
          Do 110 l = 1, 201
            Do 120 m = 1, 201
              u(l, m, 1) = u(l, m, 2)
              u(l, m, 2) = u(l, m, 3)
 120        Continue
 110      Continue
c                                output at times proportional to nint
          IF (k .eq. nint) Then
          Do 30 i = 1, 201, 5
            Do 40 j = 1, 201, 5
              zz = Dsin(u(i, j, 3)/2.d0)
              Write(9, *)zz
 40         Continue
c     the three xxx are to separate spatial rows for 3D plotting,
c     they must be replaced by blank lines (not carriage Returns) 
            Write(9, *)'xxx'
 30       Continue
        EndIF
        time = time+dt
 100    Continue
        Return
      End
