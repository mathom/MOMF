include ../../makefile.defs
# 
OPTIONS = -fbounds-check
#LIB = /opt/fitsio/fitsio/libfitsio.a
LIB = ../../fitsio/libfitsio.a
BIN = ../lib
BINARIES = maggen imtest
SOURCES = maggen.f imtest.f

all:	maggen imtest
	mv $(BINARIES) $(BIN)

maggen:	maggen.f
	$(g77) $(OPTIONS) -o maggen maggen.f $(LIB)

imtest:	imtest.f
	$(g77) $(OPTIONS) -o imtest imtest.f $(LIB)

clean:
	rm $(BIN)/*

list:	$(SOURCES)
#	rsh origo.ifa.au.dk a2ps -1 -Phpnew < `cat $(SOURCES)`
	mpage -2 -H -M20lr10t20b -m50l50r -bA4 < `cat $(SOURCES)` > p02.ps
