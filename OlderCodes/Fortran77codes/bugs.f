c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation   
c    
c  bugs.f: Bifurcation diagram for logistic map   
c           plot without conneting datapoints with lines  

      Program bugs 
			
        Implicit none
	      Real*8 m_min, m_max, m, step, y
	      Integer x
	      m_min = 1.0
	      m_max = 4.0
	      step = 0.01
	      Open(6, File = 'bugs.dat', Status = 'Unknown')
c                   Loop for m values, arbitrary starting value for y
     	  Do 10 m = m_min, (m_max-step), step
          y = 0.5
c                                       Wait until transients die out
	        Do 20 x = 0, 200
	          y = m * y * (1 - y)
 20	      Continue
c                                                   Record 200 points
	        Do 30 x = 201, 401
	          y = m*y * (1 - y)
	          Write (6, 50) m, y
 30	      Continue 
 10     Continue	
 50     Format (f5.3, f10.6)
 	      Close(6)
        Stop 'data saved in bugs.dat' 
      End	
 
