/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* parallel Monte Carlo integration, Quadrant 1 Slave*/
#include <stdio.h>
#include <math.h>
#include <pvm3.h>
#define steps 50000
#define xmin 0.0
#define xmax 0.5
#define ymin 0.0
#define ymax 0.5
double f(double x)          /* border function  */
{ return(-sqrt(-x*x+x)+0.5); }
main() {
int i, ptid, bufid, dum, type, send[1];
double x, y;
srand48(pvm_mytid());    /* seed  random generator */ 
ptid = pvm_parent();
do
{  send[0]=0;
   for (i=1; i<=steps; i++)
   {  x= drand48()*(xmax+xmin);         /* random points in the */
      y= drand48()*(ymax+ymin);         /* quadrant */
      if (f(x)<=y) send[0]++;           /* point is inside the figure */
   }     
   pvm_initsend(PvmDataDefault);        /* send result back to master */
   pvm_pkint(send, 1, 1); 
   pvm_send(ptid, 1);
   bufid=pvm_recv(ptid, -1);            /* any message from master */
   pvm_bufinfo(bufid, &dum, &type, &dum);   /* more work ? */
}while(type);

pvm_exit;                   /* type=0 means we are done */ 
}
