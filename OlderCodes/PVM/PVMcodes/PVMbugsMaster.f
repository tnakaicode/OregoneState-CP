/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
 program logmaster
C
C Log-Master Program
C This is the master control code which breaks the
C calculation of the logistics map over a  
C variety of workers. A population calculation is
C done using 400 iterations of the recursive relation
C x=mu*x*(1-x) with a starting value of x=0.5.
C mu is varied between 1 and 4 with a step size of
C 1/resolution, where the variable resolution is set
C by the user.  A job, consisting of a range of steps
C in mu space is sent to each worker.  Each job consists
C of "jobsize" steps in mu space.  The master programs allocates
C another job to each worker when the worker finishes with the
C previous job.  By using small jobs sizes we achieve efficient dynamic
C allocation where the fastest machine does the most work and little
C time is wasted waiting for slow workers to finish.  
C
  implicit none
  include 'fpvm3.h'

  integer*4 wrkrtomstr, mstrtowrkr
  parameter( wrkrtomstr=2, mstrtowrkr=1)
  integer*4  rc, wrkr_tid(20), mstr_tid, msg_buff
  integer*4 jobsize,i,startstep,resolution,n_workers
  integer*4 workerdone
  open(1,FILE='input',STATUS='OLD')
C
C First enroll itself and redirect worker
C standard out.
C
        call pvmfmytid( mstr_tid )
        call pvmfcatchout( 1, rc )
C
C Now get information about division of work 
C and the resolution of the calcuations.
C 
C
  WRITE(*,*) 'Enter resolution 1-1000'
  READ(1,*) resolution
  WRITE(*,*) 'Enter number of workers'
  READ(1,*) n_workers
  WRITE(*,*) 'Enter job size 1-resolution'
  READ(1,*) jobsize
C
C Now start up the workers
C
  call pvmfspawn('logwrkr',PVMDEFAULT,'*',n_workers ,wrkr_tid,rc)
C
C Now send off the first round of work.
C The variable startstep keeps track of the step in mu space
C at which the next job should be started.
C
  startstep=1*resolution
  DO i=1,n_workers
         call pvmfinitsend(PVMDEFAULT,msg_buff)
   call pvmfpack(INTEGER4, startstep ,1,1, rc)
   call pvmfpack(INTEGER4, jobsize ,1,1, rc)
   call pvmfpack(INTEGER4, resolution ,1,1, rc)
         call pvmfsend(wrkr_tid(i),mstrtowrkr,rc)
   startstep=startstep+jobsize+1
        END DO
C
C Now we listen for jobs that are done.  When a worker
C finishes we send off more work until all the work
C has been allocated. 
C
  DO WHILE (DBLE(startstep)/DBLE(resolution) .LT. 4.0d0)
   call pvmfrecv(-1,wrkrtomstr, msg_buff)
   call pvmfunpack(INTEGER4, workerdone,1,1,rc) 
   call pvmfinitsend(PVMDEFAULT,msg_buff)
   call pvmfpack(INTEGER4, startstep,1,1,rc)
   call pvmfpack(INTEGER4, jobsize,1,1,rc)
   call pvmfpack(INTEGER4, resolution,1,1,rc)
   call pvmfsend(workerdone,mstrtowrkr,rc)
   startstep=startstep+jobsize+1  
  END DO
C
C Now all the work has been sent out. When workers signal they
C have finished we send back 0's to indicate they can 
C terminate themselves.
C
  DO i=1,n_workers
   call pvmfrecv(-1,wrkrtomstr, msg_buff)
         call pvmfunpack(INTEGER4, workerdone,1,1,rc)
         call pvmfinitsend(PVMDEFAULT,msg_buff)
         call pvmfpack(INTEGER4, 0,1,1,rc)
         call pvmfpack(INTEGER4, 0,1,1,rc)
         call pvmfpack(INTEGER4, 0,1,1,rc)
         call pvmfsend(workerdone,mstrtowrkr,rc)
  END DO
C
C Now all the workers have been told to terminate so
C we can terminate ourself.
  call pvmfexit(rc)
  stop
  end

