      program MOMF
c
c
c      Dshow2.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c      First-version: DAOshow2.f
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c
c-----------------------------------------------------------
c In principle a special version of the Dshow routine
c

c     
c     Definition of global and local frame
c     
      parameter (NDIM=4096,maxoff=40)
      
      integer*2 image(1:NDIM,1:NDIM), image2(-10:10,-10:10)
      common image
      character*2 image3(-10:10,-10:10)
      
      integer calcsize
      
      open(23,file='fwhm.SUM',status='old')
      read(23,*)fwhm,fwhm
      fwhm=(fwhm*0.6005)**2
      close(23)
      
      open(15,file='factor.SUM',status='old')
      read(15,*)ccc01
      close(15)
      
      i01=0
      
      write(6,*)'Saturation-limit after biassub. (less than 32767 ADU):'
      write(6,*)'The used limit will be 15 % lower'
      read(5,*)xsatur
      nsatur=nint(0.85*xsatur/ccc01)
      write(6,*)'Number of PSF stars to be used (max 10):'
      read(5,*)numpsf
      open(10,file='FORMAT',status='old')
      
      read(10,*)COL
      read(10,*)ROW
      read(10,*)pfix
      
      close(10)
      
      mmx=nint(COL)
      mmy=nint(ROW)
      
c     SGI convention defines record length given in words = 4 bytes
      Nrecl=calcsize(mmx,mmy)
      
      open (10,file='Dcoo.fit',access='direct',
     c     recl=Nrecl,status='old')
      
      read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
      close(10)
      
      open (10,file='PSFstars2',status='old')
      open (11,file='PSFstars',status='new')
      
      i=0
      
 10   continue
      read(10,*,end=100)x,x,x1,x2,x3,nmomf
      n1=nint(x1)
      n2=nint(x2)
c     
c     check for PSF stars too close to edge of frame
c     
      if (n1.lt.maxoff+1) then 
         goto 10
      else if (n1.gt.mmx-maxoff) then 
         goto 10
      else if (n2.lt.maxoff+1) then 
         goto 10
      else if (n2.gt.mmy-maxoff) then 
         goto 10
      else
         continue
      endif
      sum=0.0
      do 15 ni=-10,10,1
         do 14 nj=-10,10,1
            image2(ni,nj)=image((n1+ni),(n2+nj))
             sum=sum+float(image2(ni,nj))
 14       continue
 15    continue
       nmax=nint(sum/441.0)
       nmin=nmax
       do 17 ni=-10,10,1
          do 16 nj=-10,10,1
             nmax=max(image2(ni,nj),nmax)
             nmin=min(image2(ni,nj),nmin)
 16       continue
 17    continue
       if (nmax-nsatur) 171,171,10
 171   continue
       dif=0.05*log(float(nmax-nmin+1))
       do 19 ni=-10,10,1
          do 18 nj=-10,10,1
             dif2=log(float((image2(ni,nj)-nmin+1)))
             nnimg=nint(dif2/dif)
             if (nnimg) 201,201,202
 201         image3(ni,nj)='  '
             go to 18
 202         if (nnimg-1) 203,203,204
 203         image3(ni,nj)=' .'
             go to 18
 204         if (nnimg-2) 205,205,206
 205         image3(ni,nj)='. '
             go to 18
 206         if (nnimg-3) 207,207,208
 207         image3(ni,nj)='..'
             go to 18
 208         if (nnimg-4) 209,209,210
 209         image3(ni,nj)='--'
             go to 18
 210         if (nnimg-5) 211,211,212
 211         image3(ni,nj)='~~'
             go to 18
 212         if (nnimg-6) 213,213,214
 213         image3(ni,nj)='++'
             go to 18
 214         if (nnimg-7) 215,215,216
 215         image3(ni,nj)='=='
             go to 18
 216         if (nnimg-8) 217,217,218
 217         image3(ni,nj)='::'
             go to 18
 218         if (nnimg-9) 219,219,300
 219         image3(ni,nj)='\\'
             go to 18
 300         if (nnimg-10) 301,301,302
 301         image3(ni,nj)='//'
             go to 18
 302         if (nnimg-11) 303,303,304
 303         image3(ni,nj)='[['
             go to 18
 304         if (nnimg-12) 305,305,306
 305         image3(ni,nj)='**'
             go to 18
 306         if (nnimg-13) 307,307,308
 307         image3(ni,nj)='%%'
             go to 18
 308         if (nnimg-14) 309,309,310
 309         image3(ni,nj)='&&'
             go to 18
 310         if (nnimg-15) 311,311,312
 311         image3(ni,nj)='88'
             go to 18
 312         if (nnimg-16) 313,313,314
 313         image3(ni,nj)='OO'
             go to 18
 314         if (nnimg-17) 315,315,316
 315         image3(ni,nj)='##'
             go to 18
 316         if (nnimg-18) 317,317,318
 317         image3(ni,nj)='||'
             go to 18
 318         if (nnimg-19) 319,319,320
 319         image3(ni,nj)='$$'
             go to 18
 320         image3(ni,nj)='@@'
 18       continue
 19    continue
 
       write(6,*)' '
       write(6,888) (image3(ia,-10),ia=-10,10)
       write(6,888) (image3(ia,-9),ia=-10,10)
       write(6,888) (image3(ia,-8),ia=-10,10)
       write(6,888) (image3(ia,-7),ia=-10,10)
       write(6,888) (image3(ia,-6),ia=-10,10)
       write(6,888) (image3(ia,-5),ia=-10,10)
       write(6,888) (image3(ia,-4),ia=-10,10)
       write(6,888) (image3(ia,-3),ia=-10,10)
       write(6,888) (image3(ia,-2),ia=-10,10)
       write(6,888) (image3(ia,-1),ia=-10,10)
       write(6,888) (image3(ia,0),ia=-10,10)
       write(6,888) (image3(ia,1),ia=-10,10)
       write(6,888) (image3(ia,2),ia=-10,10)
       write(6,888) (image3(ia,3),ia=-10,10)
       write(6,888) (image3(ia,4),ia=-10,10)
       write(6,888) (image3(ia,5),ia=-10,10)
       write(6,888) (image3(ia,6),ia=-10,10)
       write(6,888) (image3(ia,7),ia=-10,10)
       write(6,888) (image3(ia,8),ia=-10,10)
       write(6,888) (image3(ia,9),ia=-10,10)
       write(6,888) (image3(ia,10),ia=-10,10)
 888   format(a12,20a2)
       
       i01=i01+1
       write(6,921)i01,i
 921   format(2i5)
       write(6,922)nmomf,x1,x2,x3
 922   format(i5,3f10.3)
       write(6,*)
     .      'Can this star be used as a PSF.star ? (2=stop/1=y/0=n):'
       read(5,*)nnn
       
       if (nnn-1)10,221,100
 221   continue
 
       back=0.0
       sback=16384.0
 
       do 510 h=1,4,1
 
       s1=0.0
       s2=0.0
       s3=0.0
 
       do 502 n4=(nint(x1)-22),(nint(x1)+22),1
          do 501 n5=(nint(x2)-22),(nint(x2)+22),1
             dist=(float(n4)-x1)**2+(float(n5)-x2)**2
             if (dist-121.0) 501,500,500
 500         continue
             d=((float(image(n4,n5))-back)/sback)**2
             s1=s1+float(image(n4,n5))*exp(-d)
             s2=s2+(float(image(n4,n5))**2)*exp(-d)
             s3=s3+exp(-d)
 501      continue
 502   continue
       
       back=s1/s3
       s4=s2/s3
       sba=3.0*(s4-back**2)
       if (sba) 504,504,503
 503   continue
       sback=sqrt(sba)
 504   continue
       write(6,412)'Sky-background',h,back,sback
 412   format(a15,i10,2f12.2)
       
 510  continue
      
      write(6,413)'Star-coordinate (x,y):  ',x1,x2
 413  format(a25,2f12.2)
      
      do 530 h=1,3,1
         
         s1=0.0
         s2=0.0
         s3=0.0
         
         do 522 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
            do 521 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
               dist=(float(n4)-x1)**2+(float(n5)-x2)**2
               if (dist-81.0) 520,520,521
 520           continue
               d=(float(image(n4,n5))-back)*exp(-dist/fwhm)
               s1=s1+float(n4)*d
               s2=s2+float(n5)*d
               s3=s3+d
 521        continue
 522     continue
         
         x1=s1/s3
         x2=s2/s3
         write(6,413)'Star-coordinate (x,y):  ',x1,x2
         
 530  continue
      write(6,*)'Can this star still be used ? (1=y/0=n):'
      read(5,*)nnn
      
      if (nnn)10,10,227
 227  continue
      
      i=i+1
      
      write(11,222)i,nmomf,x1,x2,x3
 222  format(2i7,f11.2,f9.2,f14.3)
      
      if (i-numpsf)10,100,100
      
 100  continue
      
      close(10)
      close(11)
      
      end
      
      include '../../common/calcsize.f'
