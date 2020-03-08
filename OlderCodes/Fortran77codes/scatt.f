c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation   
c
c  scatt.f:  scattering phase shift in momentum space from delta shell 
c            potential, LU decomposition with partial pivoting.        
c            uses gauss.f, LUfactor, LUSolve (included)              
c
      Program scatt
			
        Integer n, Size, i, j, Row, Column
        Double Precision b, Pot
        Parameter (Size = 300, pi = 3.1415926535897932384626, b = 10.)
        Double Precision lambda, scale, ko, Temp, F(Size, Size)
        Double Precision  k(Size), w(Size), D(Size), r(Size), V(Size)
        Double Precision L(Size, Size), U(Size,Size), P(Size,Size)
        Integer PivotInfo(Size)
c                                     Input potential strength lambda
        Write(*, *) 'enter lambda'
        Read(*, *) lambda
        Write(*, *) 'enter scaling factor'
        Read(*, *) scale
        Write(*, *) 'enter ko'
        Read(*, *) ko
        Write(*, *) 'enter grid size'
        Read(*, *) n
c Set up Gaussian pts &wts, intrvl [0, inf], mid-point at 'scale'.
c                                  Set  last element in k array to ko
        Call gauss(n, 2, 0d0, scale, k, w)
        Do i = 1, n
          D(i) = 2.d0/pi*w(i)*k(i)*k(i)/(k(i)*k(i)-ko*ko)
        End Do
        D(n+1) = 0.
        Do j = 1, n
          D(n+1) = D(n+1)+w(j)*ko*ko/(k(j)*k(j)-ko*ko)
        End Do
        D(n+1) = D(n+1)*(-2.d0/pi)
c                                        Set up F matrix and V vector 
        Do i = 1, n+1
          Do j = 1, n+1
            Pot = -b*b*lambda*SIN(b*k(i))*SIN(b*k(j))
            Pot = Pot/(k(i)*b*k(j)*b)
            F(i, j) = Pot*D(j)
            IF (i .EQ. j) Then
            F(i, j) = F(i, j)+1.d0
            Endif
          End Do
          V(i) = Pot
        End Do
c
c      LU factorization. LU factors of F, store partial pivoting info
        call LUfactor(F, n+1, Size, L, U, PivotInfo)
c                          Pivot and solve, set P to identity matrix
        Do Row = 1, n+1
          Do Column = 1, n+1
            P(Row, Column) = 0
            If (Row .EQ. Column) P(Row, Column) = 1
          End Do
        End Do
c                               Interchange rows to get true P matrix
        Do Row = 1, n+1
          Do Column = 1, n+1
            Temp = P(Row, Column)
            P(Row, Column) = P(PivotInfo(Row), Column)
            P(PivotInfo(Row), Column) = Temp
          End Do
        End Do
        Call LUSolve(V, L, U, n+1, Size, PivotInfo, r)
c                                                      Output results
         Write(*, *) ko*ko, Datan(-r(n+1)*ko)
      End
      
c  LU factorization, partial pivoting of A in prep for solving Ax = b

      Subroutine LUfactor(A, n, Size, L, U, PivotInfo)
        Integer n, Column, CurrentPivotRow, CurrentRow, SwapCol, Row 
        Integer ElimCol, Size
        Double Precision A(Size, Size), L(Size, Size), U(Size, Size)
        Integer PivotInfo(Size) 
        Double Precision CurrentPivotValue, Swap
        Do Column = 1, n-1
          CurrentPivotRow = Column
          CurrentPivotValue = A(CurrentPivotRow, Column)
c                                     Determine row for largest pivot
        Do CurrentRow = Column+1, n                            
        If (DABS(A(CurrentRow, Column)) .GT. CurrentPivotValue) Then
            CurrentPivotValue = DABS(A(CurrentRow, Column))
            CurrentPivotRow = CurrentRow
          Endif
        End Do
        PivotInfo(Column) = CurrentPivotRow
c                                 Swap rows so largest value at pivot
        Do SwapCol = Column, n
        Swap = A(Column, SwapCol)
        A(Column, SwapCol) = A(PivotInfo(Column), SwapCol)
        A(PivotInfo(Column), Swapcol) = Swap
        End Do
c                                                Gaussian Elimination
c            Get upper triangular A and un-pivoted lower triangular L
        Do Row = Column+1, n
          L(Row, Column) = A(Row, Column)/A(Column, Column)
            Do ElimCol = Column+1, n
              A(Row,ElimCol) = A(Row,ElimCol) 
							                  - L(Row,Column) * A(Column,ElimCol)
            End Do
          End Do
        End Do
c               Make sure bottom right value doesnot get pivoted to 0
        PivotInfo(n) = n
c                                                     Now pivot the L
        Do Row = 2, n-1
          Do Column = 1, Row-1
            Swap = L(Row, Column)
            L(Row, Column) = L(PivotInfo(Row), Column)
            L(PivotInfo(Row), Column) = Swap
          End Do
        End Do
c                                                    clean up L and U
        Do Column = 1, n
          Do Row = 1, Column 
            U(Row, Column) = A(Row, Column)
            L(Row, Column) = 0
            IF (Row .EQ. Column) L(Row, Column) = 1
          End Do
        Do Row = Column+1, n 
          U(Row, Column) = 0
        End Do
        End Do
        Return
      End
      
      Subroutine LUSolve(b, L, U, n, Size, PivotInfo, x)
        Integer n, Size, Row, Column
        Double Precision b(Size), x(Size)
        Integer PivotInfo(Size)
        Double Precision L(Size, Size), U(Size, Size)
        Double Precision Temp
c                                  Interchange rows of b for pivoting
        Do Row = 1, n
          Temp = b(Row)
          b(Row) = b(PivotInfo(Row))
          b(PivotInfo(Row)) = Temp
        End Do
c  Solve Ly = b by forward elimination, since L diagonal, y(1) = b(1)
        Do Row = 2, n
          Do Column = 1, Row-1
            b(Row) = b(Row)-L(Row, Column)*b(Column)
          End Do
          b(Row) = b(Row)/L(Row, Row)
          End Do
c                                   solve Ux = y by back substitution
        x(n) = b(n)/U(n, n)
        Do Row = n-1, 1, -1
          x(Row) = b(Row)
          Do Column = Row+1, n
            x(Row) = x(Row)-U(Row, Column)*x(Column)
          End Do
        x(Row) = x(Row)/U(Row, Row)
        End Do
        Return
      End
  
