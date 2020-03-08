!	 scatt.f90:	 scattering phase shift in momentum space from delta shell 
!						 potential, LU decomposition with partial pivoting.				 
!						uses gauss.f,		LUfactor, LUSolve (included)							
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
Program scatt

  Integer :: n, Size, i, j, Row, Column
	Double Precision :: b, Pot
	Parameter (Size = 300, pi = 3.1415926535897932384626, b = 10.0)
	Double Precision :: lambda, scale, ko, Temp
	Double Precision :: F(Size, Size), k(Size), w(Size),D(Size),r(Size)
	Double Precision :: V(Size),L(Size,Size),U(Size,Size), P(Size,Size)
	Integer :: PivotInfo(Size)
	
	                                  ! Enter potential strength lambda
  Write(*, *) 'enter lambda'              
	Read(*, *) lambda
	Write(*, *) 'enter scaling factor'
	Read(*, *) scale
	Write(*, *) 'enter ko'
	Read(*, *) ko
	Write(*, *) 'enter grid size'
 	Read(*, *) n
                     ! Set up Gaussian integration points and weights
               ! on interval [0, inf] with the mid - point at 'scale'
                                 ! Set  last element in k array to ko
  call gauss(n, 2, 0d0, scale, k, w)
                                                  ! 	Set up D matrix
  Do i = 1, n
	  D(i) = 2.0d0/pi*w(i)*k(i)*k(i)/(k(i)*k(i) - ko*ko)
	End Do
	D(n + 1) = 0.0
	Do j = 1, n
	  D(n + 1) = D(n + 1) + w(j)*ko*ko/(k(j)*k(j) - ko*ko)
	End Do
	D(n + 1) = D(n + 1)*( - 2.0d0/pi)
       ! 	Set up F matrix and V vector 
	Do i = 1, n
		Do j = 1, n
			Pot = - b*b*lambda*SIN(b*k(i))*SIN(b*k(j))
			Pot = Pot/(k(i)*b*k(j)*b)
			F(i, j) = Pot*D(j)
			If (i == j) then
				F(i, j) = F(i, j) + 1.0d0
			Endif
		End Do
		V(i) = Pot
	End Do
 ! 	   LU factorization.  Put LU factors of F in corresponding matrix
 ! 	           (not efficient but easy ). Store partial pivoting info
 ! 
	call LUfactor(F, n, Size, L, U, PivotInfo)
                                                   ! 	Pivot and solve
                                         ! 	 Set P to identity matrix
  	Do Row = 1, n + 1
		Do Column = 1, n + 1
		  P(Row, Column) = 0
		  If (Row .EQ. Column) P(Row, Column) = 1
		End Do
	End Do
                              ! Interchange rows to get true P matrix
	Do Row = 1,  n
		Do Column = 1,  n
		  Temp = P(Row, Column)
		  P(Row, Column) = P(PivotInfo(Row), Column)
		  P(PivotInfo(Row), Column) = Temp
		End Do
	End Do
	call LUSolve(V, L, U, n, Size, PivotInfo, r)
	                                                   ! output results
  write(*, *) ko*ko, DATAN( - r(n)*ko)                     
End Program scatt

! 	                LU factorization, partial pivoting of A in Ax = b
subroutine LUfactor(A, n, Size, L, U, PivotInfo)
  Integer :: n, Column, CurrentPivotRow, CurrentRow, SwapCol, Row 
	Integer :: ElimCol, Size
	Double Precision :: A(Size, Size), L(Size, Size), U(Size, Size)
	Integer :: PivotInfo(Size) 
	Double Precision :: CurrentPivotValue, Swap
	Do Column = 1, n - 1
		CurrentPivotRow = Column
		CurrentPivotValue = A(CurrentPivotRow, Column)
                                    ! Determine row for largest pivot
		Do CurrentRow = Column + 1, n         
		  If ( DABS(A(CurrentRow, Column)) .GT. CurrentPivotValue ) Then
		  	CurrentPivotValue = DABS(A(CurrentRow, Column))
		  	CurrentPivotRow = CurrentRow
			Endif
		End Do
		PivotInfo(Column) = CurrentPivotRow
		                            ! Swap rows so largest value at pivot
    Do SwapCol = Column, n            
			Swap = A(Column, SwapCol)
			A(Column, SwapCol) = A(PivotInfo(Column), SwapCol)
			A(PivotInfo(Column), Swapcol) = Swap
		End Do
	!
  ! Gauss Elimin, upper triangular A, unpivoted lower triangular L
	!
    Do Row = Column + 1, n
		  L(Row, Column) = A(Row, Column)/A(Column, Column)
		  Do ElimCol = Column + 1, n
		    A(Row, ElimCol) = A(Row, ElimCol) &
				                         - L(Row, Column)*A(Column, ElimCol)
			End Do
	  End Do
	End Do
	                             ! Ensure bottom right not pivoted to 0
	PivotInfo(n) = n                    
	Do Row = 2, n - 1  
	                                                  ! Now pivot the L
		DO Column = 1, Row - 1
			Swap = L(Row, Column)
			L(Row, Column) = L(PivotInfo(Row), Column)
			L(PivotInfo(Row), Column) = Swap
		End Do
	End Do
	                                                 ! Clean up L and U
  Do Column = 1, n                                        
		Do Row =  1, Column 
			U(Row, Column) = A(Row, Column)
			L(Row, Column) = 0
			IF (Row .EQ. Column) L(Row, Column) = 1
		End Do
		Do Row = Column + 1, n 
			U(Row, Column) = 0
		End Do
	End Do
	Return
End 

! Part of an LU decomposition + partial pivoting to solve Ax = b 
Subroutine LUSolve(b, L, U, n, Size, PivotInfo, x)
  Integer :: n, Size, Row, Column
	Double Precision :: b(Size), x(Size)
	Integer :: PivotInfo(Size)
	Double Precision :: L(Size, Size), U(Size, Size)
	Double Precision :: Temp
	Do Row = 1, n                  ! Interchange rows of b for pivoting
		Temp = b(Row)
		b(Row) = b(PivotInfo(Row))
		b(PivotInfo(Row)) = Temp
	End Do
               ! 	 Solve Ly = b, where y = Ux, by forward elimination
	Do Row = 2, n
		DO Column = 1, Row - 1
			b(Row) = b(Row) - L(Row, Column)*b(Column)
		End Do
		b(Row) = b(Row)/L(Row, Row)
	End Do
                                ! 	Solve Ux = y by back substitution
	x(n) = b(n)/U(n, n)
	Do Row = n - 1, 1, - 1
		x(Row) = b(Row)
		Do Column = Row + 1, n
			x(Row) = x(Row) - U(Row, Column)*x(Column)
		End Do
		x(Row) = x(Row)/U(Row, Row)
	End Do
	Return
End

	
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

