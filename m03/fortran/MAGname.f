       program MOMF
c
c
c      MAGname.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c
c-----------------------------------------------------
c

       character*60 name2

       open(34,file='offset.PHOT',status='old')
       read(34,'(2f10.3,i8,a60)')x,x,ncoo,name2
       close(34)
 
       open(32,file='Relname',status='unknown')
       open(33,file='Absname',status='unknown')
 
       do 10 i=1,min(9,ncoo),1
          write(32,'(a11,i1)')'REL/STAR.00',i
          write(33,'(a11,i1)')'ABS/STAR.00',i
10     continue
       do 11 i=10,min(99,ncoo),1
          write(32,'(a10,i2)')'REL/STAR.0',i
          write(33,'(a10,i2)')'ABS/STAR.0',i
11     continue
       do 12 i=100,min(999,ncoo),1
          write(32,'(a9,i3)')'REL/STAR.',i
          write(33,'(a9,i3)')'ABS/STAR.',i
12     continue

       close(32)
       close(33)
 
       end
