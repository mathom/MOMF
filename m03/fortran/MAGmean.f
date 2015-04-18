      program MOMF
c
c
c      MAGmean.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c      Modified to enable selection of subset of reference stars
c      by Soren Frandsen 08.08.1997
c
c
      parameter (NSTARS=50000)
      dimension PHOT(1:NSTARS,1:7),XMEAN(1:NSTARS,1:7)
      dimension YMEAN(1:NSTARS,1:7)
      character*60 name
      integer refst(1:500),rflag
      logical inset
c  
c  Read file with information on reference stars if it exists
c
      rflag = 0
      nref = 0
      open(12,file='MOMF.ref',status='old',err=21)
      read(12,*) rflag
      if (rflag) 20, 21, 20
 20   nref = 1
 22   read(12,*, end=23) refst(nref)
      nref = nref + 1
      goto 22
 23   nref = nref - 1 
      goto 24
 21   continue
      print 886
 886  format('No reference star file')
 24   print 887, nref
 887  format('# of reference stars:',i5)
      if (nref.eq.0) goto 19
      print 889, (refst(i), i=1,nref)
 889  format(' ref stars:',20i3)
 19   if (nref.eq.0) rflag = 0
c     
      open(10,file='starfile',status='old')
      open(32,file='stat.V',status='old')
      open(33,file='image.mean',status='unknown')
      
      n=1
      
      do 4 i=1,NSTARS
         
         XMEAN(i,2)=0.0
         XMEAN(i,3)=0.0
         XMEAN(i,4)=0.0
         XMEAN(i,5)=0.0
         XMEAN(i,6)=0.0
         XMEAN(i,7)=0.0
         
         YMEAN(i,2)=0.0
         YMEAN(i,3)=0.0
         YMEAN(i,4)=0.0
         YMEAN(i,5)=0.0
         YMEAN(i,6)=0.0
         YMEAN(i,7)=0.0
         
 4    continue
      
 10   continue
      
      read(10,'(a60)',end=200)name
c     
c     check if star should be used as reference star
c     
      if (rflag) 25,26,27
 25   if ( inset(n,refst,nref) ) goto 99
      goto 26
 27   if ( inset(n,refst,nref) ) goto 26
          goto 99
 26       continue
c     
          open(11,file=name,status='old')
          
          read(32,*)x,v2,v3,v4,v5,v6,v7
c     write(6,666)n,' ',name
c     666  format(i10,a3,a60)
          
          ni=1     
          
 15       continue
          
          read(11,*,end=16) (PHOT(ni,j),j=1,7)
          XMEAN(ni,2)=XMEAN(ni,2)+PHOT(ni,2)*v2
          XMEAN(ni,3)=XMEAN(ni,3)+PHOT(ni,3)*v3
          XMEAN(ni,4)=XMEAN(ni,4)+PHOT(ni,4)*v4
          XMEAN(ni,5)=XMEAN(ni,5)+PHOT(ni,5)*v5
          XMEAN(ni,6)=XMEAN(ni,6)+PHOT(ni,6)*v6
          XMEAN(ni,7)=XMEAN(ni,7)+PHOT(ni,7)*v7
          YMEAN(ni,2)=YMEAN(ni,2)+v2
          YMEAN(ni,3)=YMEAN(ni,3)+v3
          YMEAN(ni,4)=YMEAN(ni,4)+v4
          YMEAN(ni,5)=YMEAN(ni,5)+v5
          YMEAN(ni,6)=YMEAN(ni,6)+v6
          YMEAN(ni,7)=YMEAN(ni,7)+v7
          ni=ni+1
          
          go to 15
          
 16       continue
c     write(6,666)n,' ','   ...all magnitudes calculated'
          
          close(11)
          
 99       n=n+1
          
          go to 10
          
 200      continue
          
          n=ni-1
          
          do 210 ni=1,n,1
             X2=XMEAN(ni,2)/YMEAN(ni,2)
             X3=XMEAN(ni,3)/YMEAN(ni,3)
             X4=XMEAN(ni,4)/YMEAN(ni,4)
             X5=XMEAN(ni,5)/YMEAN(ni,5)
             X6=XMEAN(ni,6)/YMEAN(ni,6)
             X7=XMEAN(ni,7)/YMEAN(ni,7)
             write(33,777)ni,X2,X3,X4,X5,X6,X7
c     write(6,777)ni,X2,X3,X4,X5,X6,X7
 777         format(i8,6f11.5)
 210      continue
          
          close(10)
          close(32)
          close(33)
          
          end
c     
c     function to check whether ni belongs to the set nset
c     
      
      logical function inset(ni,nset,n)
      integer nset(n)
c     
      inset = .false.
      do i=1,n
         if (ni.eq.nset(i)) then
            inset = .true.
            return
         endif
      enddo
      return
      end
      
      
      
