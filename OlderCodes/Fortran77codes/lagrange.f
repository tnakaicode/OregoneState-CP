c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                
c		                                               
c  lagrange.f: Langrange interpolation of cross table           
c
      Program lagrange
			
      	Implicit none
      	Real*8 inter, x, xin(9), yin(9)
      	Integer i, end
				
      	end = 9
      	Open(6, File = 'lagrange.dat', Status = 'Unknown')
c                                                          Input data
      	Data xin /0, 25, 50, 75, 100, 125, 150, 175, 200/
      	Data yin /810.6, 16, 45, 83.5, 52.8, 199., 10.8, 98.25, 48.7/
c                                                      Calculate f(x)
      	Do 20 i = 0, 1000
          x = i*0.2 
          Write (6, *) x, inter(xin, yin, end, x)
 20     Continue
 	      Close(6)
      	Stop 'data saved in lagrange.dat'
      End

c                                Evaluate interpolation function at x
      Function inter(xin, yin, end, x)
      	Implicit none
      	Integer i, j, end
      	Real*8 inter, lambda(10), xin(10), yin(10), x
      	inter = 0
      	Do 200 i = 1, end
          lambda(i) = 1
          Do 300 j = 1, end
            If (i .ne. j) Then
              lambda(i) = lambda(i) *((x - xin(j))/(xin(i) - xin(j)))
            EndIf
 300      Continue
          inter = inter + (yin(i) * lambda(i))
 200   	Continue
      	Return
      End
