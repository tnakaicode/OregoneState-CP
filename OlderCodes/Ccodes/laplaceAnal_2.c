/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/
// laplaceAnal.c Analytic solution of Laplace eqnt
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
/*___________________________________________________________
  | Solves Laplace equation in a two dimensional plate       |
  |           v(i,j) is the potential at grid point (i,j)    |
  |                 i:  x position  ,i=1,2,..,101            |
  |                 j:  y position  ,j=1,2,..,101            |
  |        ngrid: number of grid points in one side          |
  |        nitl: number of iterations for numerical solution |
  |        van(i) computed with the analytical solution      |
  |            i= 1,2,...,11                                 |    
  |__________________________________________________________|*/
FILE *fp;

void initial(int ngrid,double v[101][101])
{
  /* Initialize values for the potential  */
  int i,j,k;
  for(i=0;i<ngrid;i++){
    for(j=0;j<ngrid;j++){
     v[i][j]=0.0;
    }
  } 
  /*Initial potential at the top (ends not included)*/
  for( k=1;k<ngrid-1;k++){
    v[k][ngrid-1]=100.0;
  } 
  return; 
}      
      
void average(int ngrid,int nitl,double v[101][101]) 
{
 int i,j,k;
 printf("I am working, takes time\n");
 /* The potential at (i,j) is the average of its neighbours*/
 for(k=1;k<nitl;k++){
   for(i=1;i<ngrid-1;i++){
      for(j=1;j<ngrid-1;j++){
        v[i][j]=0.25*(v[i+1][j]+v[i-1][j]+v[i][j+1]+v[i][j-1]);
       }
     }  
 }
 printf("grid is %d\n",ngrid );
 printf("Number of iterations %d:\n",nitl);
 printf("          POTENTIAL DISTRIBUTION \n");
 for(j=ngrid-1;j>0;j=j-10){
   for(i=0;i<ngrid;i=i+10){
     printf("%7.2f",v[i][j]);     
   }  
   
   printf("\n");
 }
 /*for(j=0;j<ngrid;j=j+5){
  // for(i=0;i<ngrid;i=i+5)
     //fprintf(fp,"%f\n",v[i][j]);
     //fprintf(fp,"\n");  
 */
  return;
}


void analytc()
{

  /*finds the analytical solution for the same problem 
  prints the potential at the same points as in the
  average procedure above.     
  nsum is the numbers of terms to be summed in the series*/
  double van[41];
  double fact,xfc,sum,xova,yova,dincr,pi,pin;
  double facsin,facden,dexnpi,dex2npi;
  int i,j,k,n,nsum,ii,ngrid;
  pi=3.14159265358979323846;
  fact=400.00/pi;
 /*/ nsum=40001;*/
  nsum=101;   
  ngrid=81;
  dincr=1.0/(ngrid-1);
  printf("max number for n in sum is %d\n",nsum);
  printf("  FOURIER SERIES SOLUTION'     \n"); 
  /* starts at the plate top (v=100) and goes down.(y axis)*/
  for(j=ngrid;j>1;j=j-2){
    yova=dincr*(j-1);
    xova=0.00;
    /*computes each row (x axis)       */
    for(i=0;i<41;i++){
      sum =0.0;
      /*each point (x,y)  is computed summing 40000 terms       */
      for(n=nsum;n>=1;n=n-2){
        pin=n*pi;
        xfc=pin*xova;
        dexnpi=exp(-pin*(yova+1.00));
        if(dexnpi<(1.0e-10))dexnpi=0.00;
        facsin=exp(pin*(yova-1.00))-dexnpi;
        dex2npi=exp(-2*pin);
        if(dex2npi<(1.0e-10))dex2npi=0.0;
        facden=1.00-dex2npi;  
        sum=sum+sin(xfc)*facsin/(facden*n);
      }
      sum=sum*fact;
      van[i]=sum;
      xova=2.00*dincr+xova;
    }
    /* each row is printed. */
      
for(ii=0;ii<41;ii++){
    fprintf(fp,"\n%7.2f",van[ii]); 
    } 
   fprintf(fp,"\n");
 }
 return;
}

void main()
{  
 
   double v[101][101];
   int ngrid,nitl;   
   /* ngrid=101;
   nitl=15500; */
   fp=fopen("vplate1.dat","w"); 
   /*in file platepot.dat, the data for graphing*/
   /*initial potential      */
   /*initial (ngrid,v);
   //the potential everywhere is calculated      */
   /*average(ngrid,nitl,v); */
   /*the analytical solution is found*/
   analytc();
   fclose(fp);
}
