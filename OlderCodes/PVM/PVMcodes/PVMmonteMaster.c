/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* parallel Monte Carlo integration master */
#include <stdio.h>
#include <pvm3.h>
#define task 100            /* number of tasks per quadrant */
main() {
struct pvmhostinfo *hostp;
int bufid, check, dum, htid, nhost, narch, ptid, stid, type;
int back[1], done[5], i, j, q, k, min_t, min_q, result;
char name[20], name2[20], tmp[2];
double area;
ptid = pvm_mytid();         /* get your PVM ID number */
pvm_config( &nhost, &narch, &hostp );   /* config of virtual machine */
gethostname(name, 20);
printf("The master process runs on %s \n", name);
printf("I found following hosts in your virtual machine\n");
for (i = 0; i < nhost; i++)
{ printf("\t%s\n", hostp[i].hi_name);  }
printf("\nStarting slaves\n");
for (i=0; i<4; i++) done[i]=0;      /* reset some counters */
i=0;
j=0;
result=0;
do              
{  i++;                 /* start slaves 1,2,3,4,1,... */
   if(i==5) i=1;            /* until all machines have a */
   strcpy(name, "monteslave");          /* job running */
   sprintf(tmp, "%i", i);
   strcat(name, tmp);
   check=pvm_spawn(name, 0,PvmTaskHost,hostp[j].hi_name, 1, &stid);
   if (!check) 
   {  printf("Couldn't start process on %s\n", hostp[j].hi_name);
      nhost--;     }
   else
   {  printf("started slave for quadrant %i on %s\n", i, hostp[j].hi_name);
      done[i]++;    }
   j++;
} while ((j<nhost) && (j<4*task));
for(i=j; i<4*task; i++)         /* wait for slaves to finish to */
{  bufid=pvm_recv(-1, -1);      /* any machine any message*/
   pvm_bufinfo(bufid, &dum, &type, &stid);  /* from which quadrant? */
   pvm_upkint(back, 1, 1);          /* the result of the task */
   result+=back[0];
   if (done[type]<task)         /* there are still open tasks */
   {   pvm_initsend(PvmDataDefault);    /* tell slave to continue */
       pvm_send(stid, 1);
       done[type]++;     }
   else                 /* no open tasks, start new slave */
   {  printf("quadrant %i is done\n", type);
      htid=pvm_tidtohost(stid);         /* find host of this slave */
      for (k=0; k<nhost; k++)
      { if (htid==hostp[k].hi_tid) strcpy(name2, hostp[k].hi_name); }   
      pvm_initsend(PvmDataDefault);     /* tell slave to shut down */
      pvm_send(stid, 0);
      min_t=done[1];                /* find quadrant with most */
      min_q=1;                  /* open tasks - this way */
      for (k=2; k<5; k++)           /* the fastest machine */
      { if (done[k]<min_t)         /* new quadrant or + slow */
         {  min_t=done[k];          /* after it's done */
            min_q=k;  }   }
      strcpy(name, "monteslave");       /* which slave to start */      
      sprintf(tmp, "%i", min_q);
      strcat(name, tmp);
      pvm_spawn(name, 0,PvmTaskHost, name2, 1, &stid);
      printf("started slave for quad %i on %s\n", min_q, name2);
      done[min_q]++;
   }  }
for(i=0; i<nhost; i++)          /* wait for last tasks to end */
 { bufid=pvm_recv(-1, -1);   /* any machine/ message*/
   pvm_bufinfo(bufid, &dum, &dum, &stid);   /* where from */ 
   pvm_upkint(back, 1, 1);
   result+=back[0];
   pvm_initsend(PvmDataDefault);        /* tell slave to shut down */
   pvm_send(stid, 0);
}
area=result;                    /* calculate final result */  
printf("the area is %f\n", area/(task*4*50000));
pvm_exit;
} 