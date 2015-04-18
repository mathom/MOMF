         program MOMF
c
c
c        Drename3.f
c
c        Hans Kjeldsen - MOMF version 3.0
c        Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c        First-version: DAOrename3.f
c---------------------------------------------
c        Hans Kjeldsen - MOMF version 2.1
c        Aarhus University 1992 - Denmark
c---------------------------------------------
c
c rename files....
c
         open(10,file='momfinp',status='unknown')
 
         open(23,file='fwhm.SUM',status='old')
         read(23,*)fxx,fxx
         close(23)
 
         seeing=1.1*fxx
 
         write(10,'(a6)')'im.SUM'
         write(10,'(f15.5)')seeing
         write(6,*)'Detection-level (e.g. 8)'
         read(5,*)sigma
         write(10,'(f15.5)')sigma
         close(10)
 
         end
