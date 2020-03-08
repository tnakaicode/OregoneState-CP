/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* master program for bifurcation diagram of logistic map*/
#include <stdio.h>
#include <pvm3.h>
#define min 1               /* minimum for m */
#define max 4               /* maximum for m */
#define step 0.1            /* m range for slave */ 
#define nstep 100.0         /* number of steps for slave */
#define skip 200            /* # results to skip */
#define count 300           /* # results to save */
main() {
struct pvmhostinfo *hostp;
int bufid, check, dum, i, nhost, narch, ptid, stid;
char name[64];
double buf[5], m; 
ptid = pvm_mytid();         /* get PVM ID number */
pvm_config( &nhost, &narch, &hostp );   /* configure virtual machine */
gethostname(name, 64);
printf("The master process runs on %s \n", name);
printf("I found the following hosts in your virtual machine\n");
for (i = 0; i < nhost; i++)
{ printf("\t%s\n", hostp[i].hi_name); }
printf("\nStarting slaves\n");
for (i=0; i<nhost; i++)         /* start slaves on all hosts */
{  check=pvm_spawn("mapslave", 0,PvmTaskHost,hostp[i].hi_name, 1, &stid);
   if (!check) 
   {  printf("Couldn't start process on %s\n", hostp[i].hi_name);
      nhost--;   
      } }
pvm_setopt(PvmRoute, PvmRouteDirect);
buf[2]=nstep;               /* parameters for slaves */
buf[3]=skip;                 
buf[4]=count;                
for(m=min; m<=max; m+=step)     /* m parameter for slaves */
{  printf("%f\n", m);               /* some feedback */
   bufid=pvm_recv(-1, 2);               /* slave is ready */
   pvm_bufinfo(bufid, &dum, &dum, &stid);   /* which machine? */
   buf[0]=m;                    /* min and max m */
   if((m+step)<max) buf[1]=m+step;
   else buf[1]=max;
   pvm_initsend(PvmDataDefault);        /* send parameters */
   pvm_pkdouble(buf, 5, 1);         /* to slave */
   pvm_send(stid, 1);               
}
for (i=0; i<nhost; i++)             /* wait for final results */
{  bufid=pvm_recv(-1, 2);           /* wait for message */
   pvm_bufinfo(bufid, &dum, &dum, &stid);   /* which machine */
   pvm_initsend(PvmDataDefault);        /* tell slave to shut down */
   pvm_send(stid, 0);
}
pvm_exit;
} 