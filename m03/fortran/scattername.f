          program MOMF
c
c
c         scattername.f
c
c         Hans Kjeldsen - MOMF version 3.0
c         Aarhus University 1994 - Denmark
c         07.02.1994
c
c--------------------------------------------------------
c

          character*4 stt
          character*10 line
 
          open(10,file='starfile',status='old')
          open(11,file='starscatter',status='unknown')
 
10        continue
 
             read(10,'(a4,a10)',end=20)stt,line
             write(11,'(a4,a10)')'SCAT',line
 
          go to 10
 
20        continue
 
          close(10)
          close(11)
 
          end
