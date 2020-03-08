/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//DaubMariana.java: Wavelet image compression by M Paez of mariana.dat
// output = mariana.pgm, can be viewed with photo editor
// file comp-info.dat gives info on compression level and amount

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>

  double c0, c1, c2, c3;                              // Global variables for  Daub 4
 // int  n;                                          // Rows: power of 2
                                                                       // N = no data
 void daube4(double f[], int n, int sign) {
    double tr[n + 1];                                           // Temporary variable
    int i, j, mp, mp1; 
    if (n<4) return; 
    mp=n/2;                                                    // Midpoint of array
    mp1=mp+1;                                                // Midpoint plus one
    if (sign >= 0) {                                                           // DWT
      j = 1; 
      for (i=1;j<=n-3;i++ ){
        tr[i] = c0*f[j] + c1*f[j+1] + c2*f[j+2] + c3*f[j+3];              // Low-pass
        tr[i+mp] = c3*f[j]-c2*f[j+1]+c1*f[j+2]-c0*f[j+3];                  // Hi-pass
        j += 2;                                                         // Downsample
      }
      tr[i]=c0*f[n-1]+c1*f[n]+c2*f[1]+c3*f[2];                             // Lo-pass
      tr[i+mp]=c3*f[n-1]-c2*f[n]+c1*f[1]-c0*f[2];                          // Hi-pass
    } 
    else {                                                             // Inverse DWT
      tr[1] = c2*f[mp] + c1*f[n] + c0*f[1] + c3*f[mp1];                   // Low-pass
      tr[2] = c3*f[mp]-c0*f[n] + c1*f[1]-c2*f[mp1];                  // 2nd high-pass
      for ( i=1, j=3; i < mp; i++ )  {
        tr[j] = c2*f[i] + c1*f[i+mp] + c0*f[i+1] + c3*f[i+mp1];                 // Lo
        j += 1;  
        tr[j] = c3*f[i]-c0*f[i+mp] + c1*f[i+1]-c2*f[i+mp1];                     // Hi
        j += 1;                                            // Upsample d coefficients
      }                                                                        // For
    }                                                                         // Else
    for (i=1;i<=n;i++ )  f[i] = tr[i];                            // Copy TF to array
  }
 // 1-D DWTF via pyramid algorithm, replaces f[] by TF
 void pyram(double f[], int n, int sign) {
    int nd, nend; 
    double sq3, fsq2;                                            // sqrt(3), 4sqrt(2)
    if (n  <  4) return;                                              // Too few data
    sq3 = sqrt(3);     
    fsq2 = 4.*sqrt(2); 
    c0 = (1. + sq3)/fsq2;  c1 = (3. + sq3)/fsq2;          // Daubechies 4 coefficents
    c2 = (3.-sq3)/fsq2;  c3 = (1.-sq3)/fsq2; 
    nend = 2;                                              // Number of filter passes
    if (sign >= 0) for (nd = n; nd >= nend; nd /= 2) daube4(f, nd, sign);        //TF
    else for ( nd = 4; nd <= n; nd *= 2) daube4(f, nd, sign);               // Inv TF
  }                                                                          // Pyram
 main(){
   FILE *pf,*wf, *bf;
    pf=fopen("comp-info_c.dat","w");
    wf=fopen("mariana_c.pgm","w"); 
    bf=fopen("mariana.dat","r"); 
    int i, j, dn, zcount=0, nonz;                 // 0s & not in compressed file, tol
    int N=512,n;
    int eps = 50;                // pixels with values < eps less set =0 zero (black)
    double graypix; //gray color of pixel
    double xi=0., inxi, compratio, f[N+1];
    double fg[N+1][N+1];                                    // Contains image as ints
    double ft[N+1][N+1];                               // TFed image as array of ints
    for ( j=0;j<N;j++ )  {
      for (i=0;i<N;i++)  {
        fscanf(bf,"%lf",&graypix);                               // read line by line
        fg[i][j] = graypix;                 // For image
      }                                                         // For i;
    }                                                                        // For j
    n = N;                                      // Number datapoints must be pow of 2
    printf("Output is mariana_c.pgm\n"); 
    fprintf(wf,"P2\n");                     // Internal code of Netpbm for image type
    fprintf(wf,"512  512\n");                         // dimensions in pixel of image
    fprintf(wf,"255\n");                    // 1 B gray scale: 0 = black, 255 = white
    for (j=0; j<N;j++)  {                                      // Transform rows
      for ( i=0;i<N;i++ ) f[i]=fg[i][j];                       // Take row by row
      pyram(f, n, 1);                                                         // DWTF
      for (i=0; i<N;i++)ft[i][j] = f[i];                       // Array of files
    }
    for ( i=0;i<N;i++ )
      for (j=0;j<N;j++ )fg[i][j]=0;                                    // Reset array
    for (i=0;i< N;i++ ) {                                        // Transform columns
      for ( j=0; j < N; j++ ) f[j]=ft[i][j];                      // Column by column
      pyram(f, n, 1);                                                         // DWTF
      for (j=0;j <N;j++)fg[i][j]=f[j];                         // Form array with f's
    }
    for (j=0; j<N;j++ ) {                                // Compression, set data = 0
      for ( i=0; i < N; i++ ) { 
        if (fabs(fg[i][j]) < eps){
           fg[i][j] = 0;
           zcount = zcount + 1; 
        }                                                                    // Count
      }
    }
    for(i=0;i<N;i++)
     for(j=0;j<N;j++)ft[i][j]=0;//reset array
    for ( i=0; i < N; i++ ) {                                      // Inverse TF rows
      for ( j=0; j < N; j++ ) f[j] = fg[i][j];                       // TF row by row
      pyram(f, n, -1);                                                        // DWTF
      for ( j=0; j < N; j++ ) ft[i][j] = f[j];                    // Form image array
    }
    for ( j=0; j < N; j++ ) {                                   // Inverse TF columns
      for ( i=0; i< N; i++ ) f[i] = ft[i][j];                 // TF column by column
      pyram(f, n, -1);                                                        // DWTF
      for ( i=0; i< N; i++ ) fg[i][j] = f[i];                    // Form image array
    }
    nonz = N*N-zcount; 
    compratio = (double)nonz/(N*N); 
    printf("N nonz zcount  %d  %d %d \n",N,nonz, zcount);
    fprintf(pf,"Set coefficients < eps = 0 eps is %d\n", eps); 
    fprintf(pf,"Number of nonzero coefficients: %d \n", nonz); 
    fprintf(pf,"Compression ratio: %lf\n", compratio); 
    for ( j=0;j<N;j++ ) {                          // Trick to produce output in rows
      for ( i=0;i<N;i++){
        fprintf(wf," %d ",(int)fg[i][j]); 
      }
        fprintf(wf,"\n "); 
    }
   fclose(pf);
   fclose(wf);
   fclose(bf);
 }                                                                           // Main