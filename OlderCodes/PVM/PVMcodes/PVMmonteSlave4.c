/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* parallel Monte Carlo area calculation */
/* slave program for fourth quadrant */

#include <stdio.h>
#include <math.h>
#include <pvm3.h>
#define steps 50000
#define xmin -0.5
#define xmax 0.0
#define ymin 0.0
#define ymax 0.5

double f(double x)      /* the function for the */
{         /* border in this quadrant */
   return(sqrt(0.25-x*x));
}

main()
{
int i, hit=0, ptid, bufid, dum, send[1], type;
double x, y;

srand48(pvm_mytid());     /* seed the number generator */
          /* not a good way of doing it */
ptid = pvm_parent();


do
{
   send[0]=0;
   for (i=1; i<=steps; i++)
   {
      x= drand48()*(xmax+xmin);   /* random points in the */
      y= drand48()*(ymax+ymin);   /* quadrant */
      if (f(x)<=y) send[0]++;         /* point is inside the figure */
   }     
   pvm_initsend(PvmDataDefault);    /* send result back to master */
   pvm_pkint(send, 1, 1);
   pvm_send(ptid, 4);
   bufid=pvm_recv(ptid, -1);      /* any message from master */
   pvm_bufinfo(bufid, &dum, &type, &dum); /* more work ? */
}while(type);

pvm_exit;           /* type=0 means we are done */ 
}
