c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c    
c  spline.f: uses the SLATEC routines DBINT4 & DBVALU to interpolate
c	           a set of x-y values using cubic splines               
c            you need the SLATEC library compiled as libslatec.a   

	    Program spline
			
	      Implicit none
	      Integer NDATA
	      Parameter(NDATA = 5)
	      Real*8 BCOEF(NDATA), X(NDATA), Y(NDATA), T(NDATA+4)
	      Real*8 W(5*(NDATA+2)), W2(3*4)
       	Real*8 DBVALU, FBCL, FBCR, IN, VAL
	      Integer I, IBCL, IBCR, IDERIV, INBV, K, N, KNTOPT
				
        Open(6, File = 'spline.dat', Status = 'Unknown')
c                                                        input values
	      X(1) = 1.
	      X(2) = 2.
	      X(3) = 3.
	      X(4) = 4.
	      X(5) = 5.
	      Y(1) = 2.
	      Y(2) = 4.1
	      Y(3) = 3.
	      Y(4) = 5.
	      Y(5) = 2.
c       natural splines, set second derivatives at end points to zero
 	      IBCL = 2
 	      IBCR = 2
 	      FBCL = 0.
 	      FBCR = 0.
 	      KNTOPT = 1
c                                        find the spline coefficients
 	      Call DBINT4 (X, Y, NDATA, IBCL, IBCR, FBCL, FBCR, KNTOPT, 
     +		              T, BCOEF, N, K, W)
c                                                      initialization
  	    INBV = 1
 	      IN = 1.
 	      IDERIV = 0
	      Do 10 I = 1, 101
 	        VAL = DBVALU (T, BCOEF, N, K, IDERIV, IN, INBV, W2)
 	        Write (6, *) IN, VAL
 	        IN = IN+0.04
 10	    Continue
 	      Close(6)
 	      Stop 'data saved in spline.dat'
 	    End
