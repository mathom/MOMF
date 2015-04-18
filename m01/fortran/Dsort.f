       program MOMF
c
c
c      Dsort.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c      First-version: DAOsort.f
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c

       character*30 line
 
       open(22,file='coor.SUM',status='unknown')
       open(23,file='Dcoo.coo',status='old')
 
       read(23,'(a30)')line
       read(23,'(a30)')line
       read(23,'(a30)')line
 
6      continue
          read(23,*,end=7)n,x1,x2,x3
          write(22,777)x3,n,x1,x2
       go to 6
7      continue
 
777    format(f10.3,i8,2f12.2)
   
       close(22)
       close(23)
 
       end
