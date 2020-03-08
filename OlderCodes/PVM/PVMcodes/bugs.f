/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
  PROGRAM bugbif
C
C This program recreates
C the non-linear population dynamics
C of the logistics map.  A seed value of
C a population (0<x<1) is given
C and a sequence of subsequent populations
C are generated with the equation
C xnew=mu*x*(1-x).  It has been 
C modified from the original program bug
C to cycle over a range of growth parameters
C and to output the long term solutions--
C several stable points for small mu and
C a chaotic distribution for mu>4. 
C 
  DOUBLE PRECISION x,mu
  INTEGER step,resolution
  OPEN(1, FILE='logmap')
C
C First set the seed.
C
  x=0.5
  WRITE(*,*) 'Enter a resolution, an integer 1-1000'
  WRITE(*,*) 'a larger number gives better resolution'
  READ(*,*) resolution
C
C Now step thru mu outputing the
C populations for a number of cycles
C after the initial transients have been
C passed. In this code we do 200 steps
C to move thru the transients and then
C take the next 200 steps as our data.
C
  DO step=1*resolution,4*resolution,1
    mu=(DBLE(step)/DBLE(resolution))
    DO i=1,200
      x=mu*x*(1.0-x)
    END DO
    DO i=1,200
      x=mu*x*(1.0-x)
10      FORMAT(F7.3,F7.3)     
      WRITE(1,10) mu,x
    END DO
  END DO
  END
