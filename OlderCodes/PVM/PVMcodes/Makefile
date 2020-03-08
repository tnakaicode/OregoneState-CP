# Makefile for MSTR/WRKR program -- using PVM 3.3

# PVM's "architecture" classification  -- tailor to your system
ARCH	 = $(PVM_ARCH) 
# Location and names of PVM files  -- tailor to your system
PVMLOC   = /usr/local/pvm3
PVMLIB	 = -L$(PVMLOC)/lib/$(ARCH) -lfpvm3 -lpvm3
# Name and options for FORTRAN compiler -- tailor to your system
FC       = f77
FFLAGS   = -O -I$(PVMLOC)/include

all:	logmstr logwrkr

logmstr:	logmstr.o
	$(FC) -o $(@) $(FFLAGS) logmstr.o $(PVMLIB) 

logwrkr:	logwrkr.o
	$(FC) -o $(@) $(FFLAGS) logwrkr.o $(PVMLIB) 

strp:
	strip logmstr logwrkr
	 
clean:
	rm -f *.o core *.lst
 
cleanall:
	rm -f logmstr logwrkr *.o core *.lst
