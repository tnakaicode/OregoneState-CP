/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* PVM slave, returns machine name & local time */
#include <stdio.h>
#include <pvm3.h>
#include <time.h>
main() { 
time_t now;
char name[12], buf[60];
int ptid;
ptid = pvm_parent();        /* the ID of the master process */
pvm_setopt(PvmRoute, PvmRouteDirect);
gethostname(name, 64);      /* find name of machine */          
now=time(NULL);         /* get time */
strcpy(buf, name);      /* put name into string */
strcat(buf, "'s time is "); 
strcat(buf, ctime(&now));   /* add time to string */
pvm_initsend(PvmDataDefault);   /* allocate message buffer */   
pvm_pkstr(buf);         /* pack string into buffer */
pvm_send(ptid, 2);      /* send buffer to master */
pvm_exit;           /* slave is done and exits */
}
