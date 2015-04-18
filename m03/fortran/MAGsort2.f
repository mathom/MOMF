       program MOMF
c
c
c      MAGsort2.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c

       dimension PHOT(1:6,1:7)
 
       open(31,file='scatter',status='old')
       open(32,file='scatter6',status='unknown')
 
10     continue
 
          read(31,*,end=20)n,(PHOT(1,j),j=1,7)

          do 11 i=2,6,1
             read(31,*)(PHOT(i,j),j=1,7)
11        continue
 
          nmax=0
          do 12 i=1,6,1
             nmax=max(nmax,nint(PHOT(i,5)))
12        continue
 
          if (nint(PHOT(1,5))-nmax) 13,14,14
13        vvv=100.0
          go to 15
14        vvv=0.0
15        www=PHOT(1,4)+vvv
          nii=1
          do 16 i=2,6,1
             if (nint(PHOT(i,5))-nmax) 151,152,152
151          vvv=100.0
             go to 153
152          vvv=0.0
153          ww2=PHOT(i,4)+vvv
             if (ww2-www) 154,16,16
154          www=ww2
             nii=i
16        continue
 
          xmag=PHOT(5,2)
          nn1=nint(PHOT(nii,3))
          x1=PHOT(nii,4)
          nn2=nint(PHOT(nii,5))
          x2=PHOT(nii,6)
          x3=PHOT(nii,7)
       
c         write(6,777)n,nii,xmag,nn1,x1,nn2,x2,x3
          write(32,777)n,nii,xmag,nn1,x1,nn2,x2,x3
777          format(i7,i6,f11.5,i7,f11.5,i7,2f11.5)
 
       go to 10
20     continue

       close(31)
       close(32)
 
       end
