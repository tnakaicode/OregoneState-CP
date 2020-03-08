c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                            
c    			
c  comment: Output data is saved in 3D grid format used by gnuplot

      Program soliton
			
        Implicit None
      	Real*8 ds, dt, max, mu, eps, u(131, 3)
      	Parameter(ds = 0.4, dt = 0.1, max = 2000,mu = 0.1, eps = 0.2)
c                            delta t, delta x, time steps, mu and eps
	      Real*8 a1, a2, a3, fac, time
	      Integer i, j, k
				
      	Open (9, file = 'soliton.dat', status = 'Unknown')
c                                                   Initial condition
      	Do 10 i = 1, 131
          u(i, 1) = 0.5*(1.-tanh(0.2*ds*(i-1)-5.))
 10   	Continue
c                                                       the endpoints
	      u(1, 2) = 1.
	      u(1, 3) = 1.
	      u(131, 2) = 0.
	      u(131, 3) = 0.
	      fac = mu*dt/(ds**3.)
	      time = dt
c                                                      the first step
	      Do 20 i = 2, 130
	        a1 = eps*dt*(u(i+1, 1)+u(i, 1)+u(i-1, 1))/(ds*6.d0)
          If ((i .gt. 2).and.(i.le.129)) Then
            a2 = u(i+2, 1)+2.*u(i-1, 1)-2.*u(i+1, 1)-u(i-2, 1)
          Endif
          If ((i .eq. 2) .or. (i .eq. 130)) Then 
            a2 = u(i-1, 1)-u(i+1, 1)
          Endif
          a3 = u(i+1, 1)-u(i-1, 1)
          u(i, 2) = u(i, 1)- a1*a3-fac*a2/3.d0
 20	      Continue
c                                                         other times
 	        Do 30 j = 1, max
          Do 40 i = 2, 130
            a1 = eps*dt*(u(i+1, 2)+u(i, 2)+u(i-1, 2))/(3.d0*ds)
            If ((i .gt. 2).and.(i.le.129)) Then
              a2 = u(i+2, 2)+2.d0*u(i-1, 2)-2.d0*u(i+1, 2)-u(i-2, 2)
            Endif
            If ((i .eq. 2) .or. (i .eq. 130)) Then 
              a2 = u(i-1, 2)-u(i+1, 2)
            Endif 
            a3 = u(i+1, 2)-u(i-1, 2)
            u(i, 3) = u(i, 1)- a1*a3-2.d0*fac*a2/3.d0
            u(1, 3) = 1.d0
 40       Continue
c                                                          new -> old
          Do 50 k = 1, 131
            u(k, 1) = u(k, 2)
            u(k, 2) = u(k, 3)
 50       Continue
c                                              output every 200 steps
          If (Mod(j, 200) .eq. 0) Then
            Do 60 k = 1, 131
              Write(9, 22)u(k, 3)
 60         Continue  
            Write(9, 22)
          EndIf
          time = time + dt
 30     Continue
 22 	  Format(f10.6)
 	      Close(9)         
      	Stop 'data saved in soliton.dat'
      End
