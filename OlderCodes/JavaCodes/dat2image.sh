#!/bin/sh
#Author: Guillermo Avendano-Franco, Universidad de Antioquia

prefix=lapl
max=120
i=0
while test $i -lt $max
  do
  if test $i -lt 10
      then
      cp ${prefix}00$i.dat data.dat
      dx -script lapl.net
      convert image.tiff image.00$i.jpg   
  elif test $i -lt 100
      then
      cp ${prefix}0$i.dat data.dat
      dx -script lapl.net
      convert image.tiff image.0$i.jpg
  elif test $i -lt 1000
      then
      cp ${prefix}$i.dat data.dat
      dx -script lapl.net
      convert image.tiff image.$i.jpg
  fi
  i=`expr $i + 1`
done
mencoder"mf://  image.*.jpg" -mf fps=25 -o output.avi -ovc lavc 
                                                  -lavcopts vcodec=mpeg4 