       program MOMF
c
c
c      MAGnew.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c
c--------------------------------------------------------

       dimension PHOT(1:5000,1:7),XMEAN(1:5000,1:7)
       character*60 name
 
       open(10,file='starfile',status='old')
       open(33,file='image.mean',status='unknown')
 
       n=1

4      continue
          read(33,*,end=5) (XMEAN(n,j),j=1,7)
          n=n+1
       go to 4
5      continue
       n=n-1
 
       close(33)
      
10     continue
 
          read(10,'(a60)',end=200)name
          open(11,file=name,status='old')
 
          do 16 ni=1,n,1

             read(11,*,end=16) (PHOT(ni,j),j=1,7)
             if (PHOT(ni,2)-99.9) 45,46,46
45           PHOT(ni,2)=PHOT(ni,2)-XMEAN(ni,2)
46           if (PHOT(ni,3)-99.9) 47,48,48
47           PHOT(ni,3)=PHOT(ni,3)-XMEAN(ni,3)
48           if (PHOT(ni,4)-99.9) 49,50,50
49           PHOT(ni,4)=PHOT(ni,4)-XMEAN(ni,4)
50           if (PHOT(ni,5)-99.9) 51,52,52
51           PHOT(ni,5)=PHOT(ni,5)-XMEAN(ni,5)
52           if (PHOT(ni,6)-99.9) 53,54,54
53           PHOT(ni,6)=PHOT(ni,6)-XMEAN(ni,6)
54           if (PHOT(ni,7)-99.9) 55,16,16
55           PHOT(ni,7)=PHOT(ni,7)-XMEAN(ni,7)
       
16        continue
        
          close(11)
 
          open(12,file=name,status='old')
c         write(6,*)name
 
          do 116 ni=1,n,1
 
          write(12,777)nint(PHOT(ni,1)),PHOT(ni,2),PHOT(ni,3),
     c       PHOT(ni,4),PHOT(ni,5),PHOT(ni,6),PHOT(ni,7)
777       format(i10,f12.5,5f10.5)
 
116       continue 
 
          close(12)
 
       go to 10

200    continue
 
       close(10)
 
       end
