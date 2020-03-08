/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/* PVM master for simple communication; starts slave, get's t's */
#include <stdio.h>
#include <pvm3.h>
main() {
struct pvmhostinfo *hostp;
int result, check, i, nhost, narch, stid;
char buf[64];
pvm_setopt(PvmRoute, PvmRouteDirect);       // communication channel
gethostname(buf, 20);               // get master's name         
printf("The master process runs on %s \n", buf);
// get & display parallel machine configuration
pvm_config( &nhost, &narch, &hostp );       // get configuration
printf("I found following hosts in your virtual machine\n");
for (i = 0; i < nhost; i++)
{    printf("\t%s\n", hostp[i].hi_name);  }
for (i=0; i<nhost; i++)             // spawn processes            
{  check=pvm_spawn("answer", 0,PvmTaskHost,hostp[i].hi_name, 1, &stid);
   if (!check) printf("Couldn't start on %s\n", hostp[i].hi_name); }
result=0;
while (result<nhost)
{  pvm_recv(-1, 2);         /* wait for reply message */
   pvm_upkstr(buf);         /* unpack message */
   printf("%s\n", buf);         /* print contents */
   result++;  }
pvm_exit;               /* we are done */
} 