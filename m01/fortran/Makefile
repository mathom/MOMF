include ../../makefile.defs
OPTIONS = -fbounds-check 
#LIB = /opt/fitsio/fitsio/libfitsio.a
LIB = ../../fitsio/libfitsio.a
BIN = ../lib
BINARIES = Dchange Dcoor Dcoor2 Dnew Drename3 Dshow Dshow2 \
   Dsort Dspot2 Dsum MOMFfind MOMFname MOMFname2 SLP gradfind

SOURCES = Dchange.f Dcoor.f Dcoor2.f Dnew.f Drename3.f Dshow.f Dshow2.f \
    Dsort.f Dspot2.f Dsum.f MOMFfind.f MOMFname.f MOMFname2.f SLP.f \
    gradfind.f

all:	Dchange Dcoor Dcoor2 Dnew Drename3 Dshow Dshow2 \
   Dsort Dspot2 Dsum MOMFfind MOMFname MOMFname2 SLP gradfind
	mv $(BINARIES) $(BIN)

Dchange:  Dchange.f
	 $(g77) $(OPTIONS) -o Dchange Dchange.f

Dcoor:    Dcoor.f
	 $(g77) $(OPTIONS) -o Dcoor Dcoor.f

Dcoor2:  Dcoor2.f
	    $(g77) $(OPTIONS) -o Dcoor2 Dcoor2.f

Dnew:  Dnew.f
	    $(g77) $(OPTIONS) -o Dnew Dnew.f

Drename3:  Drename3.f
	    $(g77) $(OPTIONS) -o Drename3 Drename3.f

Dshow:  Dshow.f
	    $(g77) $(OPTIONS) -o Dshow Dshow.f

Dshow2:  Dshow2.f
	    $(g77) $(OPTIONS) -o Dshow2 Dshow2.f

Dsort:  Dsort.f
	    $(g77) $(OPTIONS) -o Dsort Dsort.f

Dspot2:  Dspot2.f
	    $(g77) $(OPTIONS) -o Dspot2 Dspot2.f $(LIB)

Dsum:  Dsum.f
	    $(g77) $(OPTIONS) -o Dsum Dsum.f $(LIB)

MOMFfind:  MOMFfind.f
	    $(g77) $(OPTIONS) -o MOMFfind MOMFfind.f $(LIB)

MOMFname:  MOMFname.f
	    $(g77) $(OPTIONS) -o MOMFname MOMFname.f

MOMFname2:  MOMFname2.f
	    $(g77) $(OPTIONS) -o MOMFname2 MOMFname2.f

SLP:  SLP.f
	    $(g77) $(OPTIONS) -o SLP SLP.f

gradfind:  gradfind.f
	    $(g77) $(OPTIONS) -o gradfind gradfind.f $(LIB)

clean:
	rm $(BIN)/*
