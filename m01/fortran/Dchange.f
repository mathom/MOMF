       program MOMF
c
c
c      Dchange.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c      First-version: DAOchange.f
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c

c
c This small routine change the MOMF-find output into a more
c organized data format. It also display some 'header'-information.
c
 
       open(31,file='find.coo',status='old')
       open(32,file='Dcoo.coo',status='unknown')
       open(33,file='coomin',status='unknown')
 
       read(33,*)coomin
 
       write(32,*)'  MOMF find:   '
       write(32,*)'  Multi Object Multi Frame photometric package'
       write(32,*)' '

       i=1
10     continue
 
          read(31,*,end=20)x,x,x,x,x,x,y,xmag,x2,x3
          write(32,777)i,x,y,xmag-coomin,x2,x3
777          format(i6,2f10.2,f12.4,f10.4,f10.2)
 
          i=i+1
 
       go to 10
20     continue

       close(31)
       close(32)
 
       end
