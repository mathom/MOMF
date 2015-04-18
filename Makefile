DIRS=\
 m01/fortran\
 m02/fortran\
 m03/fortran

all: fitsiolib
	@for each in $(DIRS); do $(MAKE) -C $$each $@ ; done
	
fitsiolib: fitsio/Makefile
	@cd fitsio; make all

clean: 
	@for each in $(DIRS); do $(MAKE) -C $$each $@ ; done
	@rm -f *~ core *.o
	@$(MAKE) -C fitsio clean
