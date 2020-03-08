/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* slave program for bifurcation plot of logistics map */
#include <stdio.h>
#include <pvm3.h>
FILE *output;                   /* internal file name */
main() {
double m, sent[5], new, old;
int bufid, dum, ptid, type, x, xskip, xcount;
char name[30], tmp[10], tmp2[10];
ptid = pvm_parent();
pvm_setopt(PvmRoute, PvmRouteDirect);  /* tell master we're ready */ 
pvm_initsend(PvmDataDefault);
pvm_send(ptid, 2);
gethostname(tmp2, 10);
do                            /* wait for news from master */
{  bufid=pvm_recv(ptid, -1);            /* any message from master */
   pvm_bufinfo(bufid, &dum, &type, &dum);   /* kind of message? */
   if (type)                             /* more work arrived */
   {  pvm_upkdouble(sent, 5, 1);
      xskip=sent[3];                /* skip transients */
      xcount=sent[4];               /* # points to record */
      strcpy(name, tmp2);           /* create unique file name */
      sprintf(tmp, "%f", sent[1]);
      strcat(name, tmp);    
      strcat(name, ".dat");
      output=fopen(name, "w");
      for (m=sent[0]; m<=sent[1]; m+=(sent[1]-sent[0])/sent[2])
      { old=0.5;                   /* arbitrary starting value */
        for (x=1; x<=xskip; x++) old=m*old*(1-old);    /* rm transients */
        fprintf(output, "%f\t%f\n", m, old);        
        for (x=1; x<=xcount; x++)          /* record xcount points */
         {  new=m*old*(1-old);              /* avoid some doubles */
            if (new != old) fprintf(output, "%f\t%f\n", m, new);
            old=new;
         } }
      fclose (output);
      pvm_initsend(PvmDataDefault);     /* tell master we're ready */
      pvm_send(ptid, 2);
   }
} while(type);
pvm_exit;                   /* type=0 means we are done */           
}
