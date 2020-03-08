README.txt for the JavaCodes directory on the CD from

  "A SURVEY OF COMPUTATIONAL PHYSICS"
   by RH Landau, MJ Paez, and CC BORDEIANU
   Copyright Princeton University Press, Princeton, 2008.

The electronic materials were developed with support from the US National
Science Foundation. Copyright for them are held by R Landau, Oregon State Univ,
2007;  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.

These are Java source files (mainly). A list of them is given in the top level
README file, as well as in an Appendix in the text.

They are compiled via the javac command:
    javac Area.java
This produces the file Area.class. The class file is executed via the java command
    java Area
N.B. you need to have the Java Development Kit (JDK) installed to use these
commands (see details in an Appendix in the text).

"Data": Some of these programs need data files (identified by suffix .dat), and
they can be found in the directory/folder "Data". The needed file should be
moved to the same working directory where you compile these source files. Also
in Data are some typical output data files for comparison.

"Fractals": Assorted Java source codes that create and analyze fractals

"Jama": The class files needed for the Java Matrix Library developed by NIST.
It contains their documentation, examples, tests, and a Java archive (.jar
file) of the library. N.B., this directory is needed here in order for those
java programs in JavaCodes that call Jama to find the Jama classes during
compilation. (Another copy of this directory is in JamaCP in order for the
programs there to compile.)

"JamaCP": Several of our Java programs that utilize the Jama matrix library.
Note, this directory contains a duplicate "Jama" subdirectory (see above) so
that its programs can be compiled within the "JamaCP" directory. You can
install from here, or, better yet, get the latest version off the Web.

"ptolemy": The class files needed for our programs calling PtPlot to compile.
(Copyright (c) 1995-2007 The Regents of the University of California. All
rights reserved.)

"ptplot5.6": The full ptplot package with all its documentation and makefiles.
(Copyright (c) 1995-2007 The Regents of the University of California. All
rights reserved.) You can install from here, or, better yet, get the latest
version off the Web.
