/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
  program logwrkr
C   Log-worker program
C This is a worker program which calculates a portion
C  of the logistic map.  The fraction of the map calculated
C  and the resolution are determined by the master program 
C  which starts this program and allocates work to it.
C
C Sean Fox 4/27/95
C
  implicit none
  include 'fpvm3.h'

  integer*4 wrkrtomstr, mstrtowrkr
  parameter( wrkrtomstr=2, mstrtowrkr=1)
  integer*4  rc, wrkr_tid, mstr_tid, msg_buff
  integer*4 jobsize, resolution, i, step, startstep
  character*7 worker_index
  character*1 chartemp
  real*8 x,mu,temp
C
C First the worker enrolls itself and finds it's
C parent's TID.  
C
      call pvmfmytid( wrkr_tid )
      call pvmfparent( mstr_tid )
C
C Next we open a file with the suffix of the worker tid.
C This ensures a unique name for each worker's output file
C but requires a bit of a 
C cludge to transform the wrkr_tid into a useable
C chararcter file suffix.  There is probably a prettier way
C to do this.

      worker_index=' '
  do i=1,6
   temp=(DBLE(wrkr_tid)/(10.0d0**i)-AINT(DBLE(wrkr_tid)/(10.d0**i)))
   chartemp= CHAR(INT(AINT(10.d0*temp+0.1d0)+48))
   worker_index=chartemp//worker_index
  end do
  open(1,FILE='logfile.' // worker_index, STATUS='NEW')
C
C Now listen for work messages from the master.  Unpack the information
C when we get it.
C
  call pvmfrecv( mstr_tid, mstrtowrkr, msg_buff )
  call pvmfunpack(INTEGER4, startstep,1,1, rc)
  call pvmfunpack(INTEGER4, jobsize,1,1,rc)
  call pvmfunpack(INTEGER4,resolution,1,1,rc)
C
C If startstep is non-zero then we do the 
C iterative logistics map calculation.
C 
  DO WHILE (startstep .NE. 0)
C
C We step through mu, at each value we do 200 steps to
C work through the transients, and then output the next 200
C steps.  startstep/resolution is the starting value of mu for this
C particular job. Mu <4 will blow up so we have to be carefull
C to trap this possibility. 
C     
       x=0.5d0
       DO step=startstep,startstep+jobsize
          mu=DBLE(step)/DBLE(resolution)
          IF(mu .GE. 4.0d0) THEN
            mu=4.0d0
          ENDIF
    DO i=1,200
                        x=mu*x*(1.0d0-x)
                END DO
                DO i=1,200
                        x=mu*x*(1.0-x)
10                      FORMAT(F7.3,F7.3)                       
                        WRITE(1,10) mu,x
                END DO
             END DO
C
C Write back our tid to the master program to signal we are done.
C
           call pvmfinitsend(PVMDEFAULT,msg_buff)
     call pvmfpack(INTEGER4, wrkr_tid ,1,1, rc)
           call pvmfsend(mstr_tid,wrkrtomstr,rc)
C
C Listen for the next message
C
    call pvmfrecv( -1, mstrtowrkr, msg_buff )
    call pvmfunpack(INTEGER4, startstep,1,1, rc)
    call pvmfunpack(INTEGER4, jobsize,1,1,rc)
    call pvmfunpack(INTEGER4,resolution,1,1,rc)
  END DO
C
C If we've recieved a 0 value for startstep from the master
C program then we can shut down.
C
  call pvmfexit(rc)
  stop
  end
