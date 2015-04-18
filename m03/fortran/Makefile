
# 
OPTIONS = -fbounds-check
BIN = ../lib
BINARIES = MAGmean MAGname MAGnew MAGrelativ MAGsort MAGsort2 MAGvariab \
   scattername
SOURCES = MAGmean.f MAGname.f MAGnew.f MAGrelativ.f MAGsort.f MAGsort2.f \
   MAGvariab.f scattername.f

all:	MAGmean MAGname MAGnew MAGrelativ MAGsort MAGsort2 MAGvariab \
   scattername
	mv $(BINARIES) $(BIN)

MAGmean:  MAGmean.f
	 g77 $(OPTIONS) -o MAGmean MAGmean.f

MAGname:    MAGname.f
	 g77 $(OPTIONS) -o MAGname MAGname.f

MAGnew:  MAGnew.f
	    g77 $(OPTIONS) -o MAGnew MAGnew.f

MAGrelativ:  MAGrelativ.f
	    g77 $(OPTIONS) -o MAGrelativ MAGrelativ.f

MAGsort:  MAGsort.f
	    g77 $(OPTIONS) -o MAGsort MAGsort.f

MAGsort2:  MAGsort2.f
	    g77 $(OPTIONS) -o MAGsort2 MAGsort2.f

MAGvariab:  MAGvariab.f
	    g77 $(OPTIONS) -o MAGvariab MAGvariab.f

scattername:  scattername.f
	    g77 $(OPTIONS) -o scattername scattername.f

clean:
	rm $(BIN)/*

list:	$(SOURCES)
#       rsh origo.ifa.au.dk a2ps -1 -Phpnew < `cat $(SOURCES)`
	mpage -2 -H -M20lr10t20b -m50l50r -bA4 < `cat $(SOURCES)` > p03.ps
