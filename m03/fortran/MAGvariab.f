      program MOMF
c
c
c      MAGvariab.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      09.02.1994
c
c--------------------------------------------------------------------
c

c
c------------
c Def. of arr. for magnitude-information
c------------
c
       parameter (NSTARS=50000)
       character*60 name2, value
       character*30 ABS(1:NSTARS), REL(1:NSTARS), line
       character*20 xx,blanck, stub
       dimension ntime(1:100),z1(1:100),z2(1:100),z3(1:100),z4(1:100)
       dimension z5(1:100), z6(1:100)
       dimension x1(1:NSTARS,1:100), x2(1:NSTARS,1:100)
       dimension x3(1:NSTARS,1:100), x4(1:NSTARS,1:100)
       dimension x5(1:NSTARS,1:100), x6(1:NSTARS,1:100)
       dimension y1(1:NSTARS,1:100), y2(1:NSTARS,1:100)
       dimension  y3(1:NSTARS,1:100), y4(1:NSTARS,1:100)
       dimension y5(1:NSTARS,1:100), y6(1:NSTARS,1:100)
       common x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6
 
       open(34,file='offset.PHOT',status='old')
       open(32,file='Absname',status='old')
       open(33,file='Relname',status='old')
 
       new=-1
       n=1
10     continue
       read(32,'(a30)',end=20)ABS(n)
       read(33,'(a30)')REL(n)
       n=n+1
       go to 10
20     continue
       n=n-1
 
       close(32)
       close(33)
 
29     continue
 
       m=0

30     continue
       m=m+1
c      possibility to put in the time here...
c
       ntime(m)=0.0
c
       read(34,'(2f10.3,i8,a5,a20)',end=31)x,x,nx,blanck,stub
       call getenv('MOMF_DATAOUT',value)
       lin = index(value, ' ') - 1
       name2 = value(:lin)//'/'//stub
       
       open(36,file=name2,status='old')
c      write(6,*)m,name2
       read(36,'(a30)')line
       read(36,*)z1(m),z2(m),z3(m),z4(m),z5(m),z6(m)
 
       z1(m)=20.0-z1(m)
       z2(m)=20.0-z2(m)
       z3(m)=20.0-z3(m)
       z4(m)=20.0-z4(m)
       z5(m)=20.0-z5(m)
       z6(m)=20.0-z6(m)
 
       do 301 i=1,n,1

c     Changed the read format to allow for 5 digit number of stars
c     the magfiles are written with (i5,2f8.2,6f10.5)
c     so to skip the id, x_cen, y_cen and read in the calculated
c     magnitudes, the format should be '(a21,6f10.5)' and not
c     '(a20,6f10.5)'.
          read(36,'(a21,6f10.5)')xx,xm1,xm2,xm3,xm4,xm5,xm6

c-----------------------------------------------------------
c
	  y1(i,m)=xm1
          y2(i,m)=xm2
          y3(i,m)=xm3
          y4(i,m)=xm4
          y5(i,m)=xm5
          y6(i,m)=xm6

c-----------------------------------------------------------
c
          x1(i,m)=xm1
          x2(i,m)=xm2
          x3(i,m)=xm3
          x4(i,m)=xm4
          x5(i,m)=xm5
          x6(i,m)=xm6
 

c-----------------------------------------------------------
c
          if (xm1-99.9) 101,100,100
100          x1(i,m)=xm1-z1(m)
101       if (xm2-99.9) 103,102,102
102          x2(i,m)=xm2-z2(m)
103       if (xm3-99.9) 105,104,104
104          x3(i,m)=xm3-z3(m)
105       if (xm4-99.9) 107,106,106
106          x4(i,m)=xm4-z4(m)
107       if (xm5-99.9) 109,108,108
108          x5(i,m)=xm5-z5(m)
109       if (xm6-99.9) 111,110,110
110          x6(i,m)=xm6-z6(m)
111       continue

301    continue
    
       close(36)
       if (m-100)30,311,311
31     m=m-1
       new=1
311    continue
 
       do 35 i=1,n,1
c         write(6,'(2a30)')ABS(i),REL(i)
          open(37,file=ABS(i),status='unknown')
          open(38,file=REL(i),status='unknown')
32        continue
             read(37,'(a30)',end=33)line
          go to 32
33        continue
             read(38,'(a30)',end=34)line
          go to 33
34        continue
          do 341 j=1,m,1
          write(37,777)ntime(j),x1(i,j)+z1(j),x2(i,j)+z2(j),
     +        x3(i,j)+z3(j),x4(i,j)+z4(j),x5(i,j)+z5(j),x6(i,j)+z6(j)
          write(38,777)ntime(j),y1(i,j),y2(i,j),y3(i,j),y4(i,j),
     +          y5(i,j),y6(i,j)
777          format(i10,f12.5,5f10.5)
341       continue
 
          close(37)
          close(38)
35     continue
 
       close(36)
 
       if (new)29,29,40
       
40     continue

c-----------------------------------------------------------
c
 
       close(34)
 
       end
