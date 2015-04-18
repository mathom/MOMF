      program GRAD
c
c  gradfind.f
c
c  Developed and tested by Kim Lang and Martin R. Knudsen
c  Integrated into MOMF by Soeren Frandsen
c
c  Aarhus University - January 2001
c
c-----------------------------------------------------------
c
c  Part of MOMF version 3.4
c-----------------------------------------------------------
c
      INTEGER i,j,ncol,nrow,nseg,delsize,ndelrad,iscale
      real sigma,detlim,cosmic,fwhm,skyback
      character*80 name,value

      common /size/ ncol, nrow,delsize,ndelrad
      common /settings/ fwhm,sigma,detlim,nseg,cosmic,skyback
      common /names/ iscale, name

c     FIND algorithm convolve+, segments-, max-, grad+

c     Read info from find.param - imagename,ncol,nrow,fwhm
c     on separate lines
      open(11,file='find.param',status='unknown')
      read(11,*)SIG
      read(11,*)SEG
      read(11,*)COSM
      read(11,*)SKYB
      close(11)
c
c   *****************************
c    first version of input 
c
c File with input for other routines
c
c      open(11,file='Dstars3',status='unknown')

c
c
c      call getenv('MOMF_DATAIN',value)
c      lin = index(value, ' ') - 1

c      open(12,file='input.SUM',status='old')
c      read(12,'(a60)')name
c      close(12)
c      name=value(:lin)//'/'//name

c      write(6,*)'Approx. seeing in pixels:'
c      read(5,*)fwhm
c       write(11,'(f15.5)')fwhm

c      write(6,*)'Detection-level (e.g. 8)'
c      read(5,*)detlim
c
c  enable scaling
c
c      call getenv('ISFLAG',value)
c      read(value,'(i5)') isflag

c      close(11)
c
c   end of first version
c  **********************************************
c      second input version
c
c Reading data from basic file: imname
c
       open(10,file='imname',status='old')

       read(10,'(a80)')name
       read(10,*)fwhm

       read(10,*)detlim
       read(10,*)isflag

       write(6,'(a80,f15.5,f15.5,i5)')name,fwhm,detlim,isflag
       close(10)

       write(6,*)'Input file: ',name
c
c   end of second version
c  **********************************************

c
c Reading information on the frame-format
c

c
c
c Reading information on the frame-format
c
       open(10,file='FORMAT',status='old')

       read(10,*)COL
       read(10,*)ROW
       read(10,*)pfix
       do i=1,4
         read(10,*) idum
       enddo
       read(10,*) iscale
       close(10)
c
c  set iscale to zero if second round of FIND
c
       iscale = iscale*isflag
c
c      transfer parameters
c
      ncol=nint(COL)
      nrow=nint(ROW)
c
      sigma=REAL(SIG)
      nseg=nint(SEG)
      cosmic=REAL(COSM)
      skyback=REAL(SKYB)

c      detlim=10.0
      print*,'name,ncol,nrow,fwhm,sigma,detlim,nseg,cosmic,skyback'
      print*,name,ncol,nrow,fwhm,sigma,detlim,nseg,cosmic,skyback


      CALL submain

      end


      SUBROUTINE  submain
c      IMPLICIT NONE
      PARAMETER ( NDIM=4096 )
      PARAMETER ( NSTARS=50000 )
      INTEGER i,j,im,jm,mxx,mxy,starid,ncosmic,ncol,nrow,delsize
      integer*2 starlist(1:2,1:NSTARS),locval,maxv
      integer*2 delshape(1:31,1:31)
      integer*2 img(1:NDIM,1:NDIM),cimg(1:NDIM,1:NDIM)
      character*80 name
      REAL indexsp,rad,rad2,delrad2,delrad,sigma,detlim
      REAL cosmic,fwhm,skyback
      integer nhalf,middle,nseg,ndelrad
      
      integer segnr,seglow,seghigh,gradcountx,gradcounty
      integer xold1(1:NSTARS),yold1(1:NSTARS)
      LOGICAL*2 iempty(1:NDIM,1:NDIM)
      integer iem,ima,nfind,Nrecl,jkk,iimp,iscale
      integer starnr,ncc,id,jd,nw,ndd
      real cmu,c2mu,Ssx,Ssy,Ssw,c2c,csigma,c2sigma,xloc
      real sum1,sum2,xcoo,ycoo,xmag,xm0,ddi,sky
      real coomin,tempv
      integer calcsize
c      integer x,y,ig,jg
c      LOGICAL ATMAX
      integer ATMAX,x,y,ig,jg

      common img
      common /cimag/ cimg
      common /shapes/ delshape
      common /size/ ncol, nrow,delsize,ndelrad
      common /settings/ fwhm,sigma,detlim,nseg,cosmic,skyback
      common /names/ iscale, name

      iem=0
      ima=0
      nfind=0
c
      Nrecl=calcsize(ncol,nrow)

      do i=1,ncol
         do j=1,nrow
            iempty(i,j)= .FALSE.
         end do
      end do
      coomin=0.0
      ncosmic=0


C     ============ Making delshape ==========
C     == Make radius of the sphere to be deleted 2.5sigma
C     == Since 1FWHM = 2.35482sigma then 2.5sigma = 1.1 FWHM
      delrad=1.1*fwhm
      ndelrad=nint(delrad)
      delsize=2*ndelrad+1
      delrad2=(delrad)**2
      do i=1,delsize
         do j=1,delsize
            IF ((i-ndelrad-1)**2+(j-ndelrad-1)**2 .GT. delrad2) THEN
               delshape(i,j) = 0
            ELSE
               delshape(i,j) = 1
            ENDIF
         end do
      end do
C     ============ END of making delshape ==========
C     ============ Reading in img and cimg ==========

c      CALL readheader(name)
      CALL rimage(name,Nrecl,img,ncol,nrow,iscale)
c      print*,'Img'
c      print*,img(99,230)
c      print*,img(135,230)
      CALL convolve

c      CALL readfits(cname,cimg,ncol,nrow)
c      print*,'Convolved img'
c      print*,cimg(99,230)
c      print*,cimg(135,230)
c      print*,'Img'
c      print*,img(99,230)
c      print*,img(135,230)
c      print*,''
C     ============ END Reading in img and cimg ==========
C     ============ Do search for local max on cimg ==========

      starid=1
      print*,'Name of file : ',name
      print*,'ncol, nrow, FWHM ,sigma, detlim'
      print*,ncol, nrow, fwhm ,sigma, detlim
      print*,''
      open(41,file='maxfind.coo',status='unknown')
c     ============ Begin GRADfind  ========
c      nseg=5 is defined in find.param
c      rad=0.637*fwhm
c      nhalf=INT(rad)
c      MIDDLE=nhalf-1

      gradcounty=ndelrad-1
      DO WHILE (gradcounty .LT. (nrow-ndelrad-1))
         gradcounty=gradcounty+1
         gradcountx=ndelrad-1
         DO WHILE (gradcountx .LT. (ncol-ndelrad-1))
            gradcountx=gradcountx+1
            IF (cimg(gradcountx,gradcounty) .GT. detlim) THEN
c               print*,'*'
c               print*,'ncol-ndelrad-1 ',ncol-ndelrad-1
               mxx=gradcountx
               mxy=gradcounty
               maxv= cimg(mxx,mxy)
               ATMAX=0
               
C     ========== Gradient method 1 ==================
c     The following 'do-while' loop does not work under linux???
               DO WHILE (ATMAX .eq. 0)
                  ATMAX= 1
                  x=mxx
                  y=mxy
c                  print*,mxx,mxy
                  do ig=-1,1
                     do jg=-1,1
                        locval= cimg(ig+x,jg+y)
                        if (locval .GT. maxv) then
                           maxv=locval
                           mxx=ig+x
                           mxy=jg+y
                           ATMAX=0
c                           print*,mxx,mxy,locval
                        end if
                     end do
                  END DO
               END DO
               gradcountx=gradcountx+2
c     End of annoying 'do-while' loop
C     ========== ================= ==========
C     ========== Gradient method 2 ==================
c     The following 'do-while' loop should work under linux...
c 160           ATMAX= 1
c               x=mxx
c               y=mxy
c               print*,mxx,mxy
c               do ig=-1,1
c                  do jg=-1,1
c                     locval= cimg(ig+x,jg+y)
c                     if (locval .GT. maxv) then
c                        maxv=locval
c                        mxx=ig+x
c                        mxy=jg+y
c                        ATMAX=0
c                        print*,mxx,mxy,locval
c                     end if
c                  end do
c               END DO
c               if (atmax .eq. 0) goto 160
c               gradcountx=gradcountx+2
c     End of 'do-while' loop
C     ========== ================= ==========




C     ========== End Gradfind ==================
               IF (mxx .EQ. 1) go to 20 

C     ==== Include a SHARP criteria or cosmic reject ====
C     Rejected if maxv lt 0.3*img(mxx, mxy)
               maxv=cimg(mxx,mxy)
               tempv=float(maxv)-cosmic*(float(img(mxx,mxy))-skyback)
               IF (tempv)19,18,18
            
 18            CALL CONKILL (mxx,mxy)
            
c     cimg(mxx,mxy)=-maxv
C     CALL SPIRAL (cimg,detlim,mxx,mxy,ncol,nrow )
            
            
c     write(41,30)starid,mxx,mxy,maxv
               print*,starid,mxx,mxy,maxv
               starlist(1,starid)=mxx
               starlist(2,starid)=mxy
               starid=starid+1
	       if (starid.gt.NSTARS) then
		  print *, 'Too many stars'
		  stop
		  endif
               go to 20 
 19            ncosmic=ncosmic+1
           print*,'***COSMIC***', maxv, cosmic, img(mxx,mxy), skyback
               do im=-1,1,1
                  do jm=-1,1,1
                     cimg(mxx+im,mxy+jm)=detlim-1
                  end do
               end do
 20         END IF
         END DO
      END DO

c     30   format(i14,i14,i14)
c     close(41)

      print*,'After MAXFIND and CONKILL : ',starid,' loc.max '
      print*,'and ',ncosmic,' cosmic detections'
C     ============ END of search for local max on cimg ==========
C     ============ Do BACKGROUND (as in MOMF) ==========

      do 2010 starnr=1,starid-1
         
         i=starlist(1,starnr)
         j=starlist(2,starnr)

         iem=iem+1
         cmu=0.0
         csigma=0.0
         ncc=0
         do 201 id=max(1,(i-15)),min(ncol,(i+15)),1
            do 200 jd=max(1,(j-15)),min(nrow,(j+15)),1
               rad=(float(id-i))**2+(float(jd-j))**2
               if (rad-70.0) 200,199,199
 199           continue
               cmu=cmu+img(id,jd)
               ncc=ncc+1
 200        continue
 201     continue
         
         cmu=cmu/float(ncc)
         do 211 id=max(1,(i-15)),min(ncol,(i+15)),1
            do 210 jd=max(1,(j-15)),min(nrow,(j+15)),1
               rad=(float(id-i))**2+(float(jd-j))**2
               if (rad-70.0) 210,209,209
 209           continue
               csigma=csigma-(img(id,jd)-cmu)**2
 210        continue
 211     continue
         
         csigma=csigma/float(ncc)
c     
c     Init. counter for filter - Gaussian
c     
         do 320 jkk=1,2,1

c     Double check - 320 continue
c
	     c2mu=0.0
	     c2sigma=0.0
	     c2c=0.0
             
	     do 301 id=max(1,(i-15)),min(ncol,(i+15)),1
	        do 300 jd=max(1,(j-15)),min(nrow,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 300,299,299
                   
 299               continue
		   xloc=exp(((img(id,jd)-cmu)**2)/csigma)
		   c2mu=c2mu+img(id,jd)*xloc
		   c2c=c2c+xloc
                   
 300            continue
 301         continue
 
	     cmu=c2mu/c2c
	     c2c=0.0
             
	     do 311 id=max(1,(i-15)),min(ncol,(i+15)),1
	        do 310 jd=max(1,(j-15)),min(nrow,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 310,309,309

 309               continue
		   xloc=exp(((img(id,jd)-cmu)**2)/csigma)
		   c2c=c2c+xloc
		   c2sigma=c2sigma-xloc*(img(id,jd)-cmu)**2

 310            continue
 311         continue
 
	     csigma=c2sigma/c2c
c
c Double check....
c
 320      continue
 
          csigma=sqrt(-1.0*csigma)
c          print*,'Csigma ',csigma


c Sky mean and scatter calculated
c
c direct check II:
 
          sum1=0.0
          sum2=0.0
          
          do 401 id=(i-1),(i+1),1
             do 400 jd=(j-1),(j+1),1
                
                sum1=sum1+img(id,jd)
                
 400         continue
 401      continue
          
          do 411 id=(i-2),(i+2),1
             do 410 jd=(j-2),(j+2),1
                
                sum2=sum2+img(id,jd)
                
 410         continue
 411      continue
          
          sum2=9.0*((sum2-sum1)/16.0)
          
          sum1=(sum1-sum2)/3.0
          
          csigma=csigma*sigma
          
c     
c     Check II: sigma-check
c     
          
          if (csigma-sum1) 450,2010,2010
          
 450      continue
          
          ima=ima+1
c          print*,'Csigma ',csigma
          
c     
c     End: check II
c     
          
          
          
          Ssx=0.0
          Ssy=0.0
          Ssw=0.0
          
          do 611 id=max(1,(i-5)),min(ncol,(i+5)),1
             do 610 jd=max(1,(j-5)),min(nrow,(j+5)),1
                
                rad=-0.3*(float(id-i))**2-0.3*(float(jd-j))**2
                rad=exp(rad)
                
                Ssx=Ssx+(float(img(id,jd))-cmu)*rad*float(id)
                Ssy=Ssy+(float(img(id,jd))-cmu)*rad*float(jd)
                Ssw=Ssw+(float(img(id,jd))-cmu)*rad
                
 610         continue
 611      continue
          
          if (Ssw) 2010,2010,612
       
 612      continue
          
c          print*,'Ssw ',Ssw
          do 712 iimp=1,5,1
             
	     xcoo=Ssx/Ssw
	     ycoo=Ssy/Ssw
             
	     Ssx=0.0
	     Ssy=0.0
	     Ssw=0.0
             
	     do 711 id=max(1,(i-5)),min(ncol,(i+5)),1
	        do 710 jd=max(1,(j-5)),min(nrow,(j+5)),1
                   
		   rad=-0.3*(float(id)-xcoo)**2-0.3*(float(jd)-ycoo)**2
		   rad=exp(rad)
                   
		   Ssx=Ssx+(float(img(id,jd))-cmu)*rad*float(id)
		   Ssy=Ssy+(float(img(id,jd))-cmu)*rad*float(jd)
		   Ssw=Ssw+(float(img(id,jd))-cmu)*rad
                   
 710            continue
 711         continue
             
	     if (Ssw) 2010,2010,712
             
 712      continue
c          print*,'Ssw ',Ssw

          xcoo=Ssx/Ssw
          ycoo=Ssy/Ssw
          
          xmag=20.0-2.5*log10(Ssw)
          
c     Test the iempty value
          id=nint(xcoo)
          jd=nint(ycoo)
          if (id.lt.1) then
             id=1
          end if
          if (jd.lt.1) then
             jd=1
          end if

          if (id.gt.ncol) then
             id=ncol
          end if
          if (jd.lt.nrow) then
             jd=nrow
          end if
          if (iempty(id,jd)) go to 2010
	      
 630      iempty(id,jd)= .TRUE.
          xm0=2.5*log10(1.0+csigma/Ssw)
          sky=cmu
          ndd=(i-id)**2+(j-jd)**2
          
          do 5131 nw=1,nfind,1
             ddi = (xcoo-xold1(nw))**2 +(ycoo-yold1(nw))**2
             if (ddi-1.5) 20041,20041,5131
 5131     continue
          
          nfind=nfind+1
          
          xold1(nfind)=xcoo
          yold1(nfind)=ycoo
          
          write(6,5141)i,j,nfind,ima,iem,xcoo,ycoo,xmag,xm0,sky,ndd
          write(41,5141)i,j,nfind,ima,iem,xcoo,ycoo,xmag,xm0,sky,ndd
 5141     format(4i6,i6,2f8.2,2f9.4,f8.2,i6)
c     FORMAT has been edited a little from the orig. MOMFfind
c     5141            format(4i6,i8,2f7.2,2f9.4,f8.2,i8)
c
          if (xmag.gt.20.0) goto 2004
          if (coomin-xmag) 2003,2004,2004
 2003     coomin=xmag
          print 12345, coomin
12345	  format('Faintest star: ',f12.3)
 2004     continue
          
20041     continue
          
          
 2010  continue
       
       close(41)
       
c     
c     Making output for cont. MOMF-routines
c     
       open (41,file='Dcoo.fit',access='direct',
     c      recl=Nrecl,status='unknown')

       
       write(41,rec=1) ((img(i,j),i=1,ncol),j=1,nrow)
       close(41)
 
       open(42,file='coomin',status='unknown')
       write(42,'(f12.5)')coomin
       close(42)
       
       print*,''
       print*,'After GRADFIND &CONKILL with a detectionlimit of'
       print*,detlim,'ADU above zero in the convolved frame :'
       print*,starid,' local maxima detected'
       print*,ncosmic,' cosmic detections'
       print*,'After Gauss, Round and Sharpness criteria with a'
       print*,'sigma of ',sigma,' we are left with ',nfind,' stars!'

       END
C     ************************************************************

C     ====================================

      SUBROUTINE CONKILL (x,y)
c      IMPLICIT NONE
      PARAMETER ( NDIM=4096 )
      INTEGER i,j,x,y,ncol, nrow,delsize,ndelrad
      INTEGER nseg
      REAL sigma,detlim,cosmic,fwhm,skyback
      integer*2 delshape(1:31,1:31)
      integer*2 cimg(1:NDIM,1:NDIM)
      common /cimag/ cimg
      common /shapes/ delshape
      common /size/ ncol, nrow,delsize,ndelrad
      common /settings/ fwhm,sigma,detlim,nseg,cosmic,skyback

c      print*,'Conkill at ',x,y
      do i=1,delsize
         do j=1,delsize
            if (delshape(i,j) .EQ. 1) then
 4000          if (x-ndelrad-1+i)4001,4005,4005
 4001          if (ncol-(x-ndelrad-1+i))4002,4002,4005
 4002          if (y-ndelrad-1+j)4003,4005,4005
 4003          if (nrow-(y-ndelrad-1+j))4004,4004,4005
 4004          cimg(x-ndelrad-1+i,y-ndelrad-1+j)=detlim-1
 4005       end if
         end do
      END DO

      RETURN
      END

   
C     ====================================
C     ====================================

      SUBROUTINE CONVOLVE
C     ===== Make the gaussian convolver ====

      PARAMETER ( NDIM=4096 )
      real DENOM,PIXELS,RADIUS,RSQ,SIGSQ,SGOP,SUMG,SUMGSQ,RELERR
      integer JSQ,NBOX,i,j,im,jm,MIDDLE,NHALF,ncol,nrow,delsize
      REAL G(1:31,1:31),locval,delrad,skyback
      LOGICAL SKIP(1:31,1:31)
      integer*2 img(1:NDIM,1:NDIM),cimg(1:NDIM,1:NDIM)
      integer nseg
      REAL sigma,detlim,cosmic,fwhm
      real locmin,locmax
      common img
      common /cimag/ cimg
      common /settings/ fwhm,sigma,detlim,nseg,cosmic,skyback
      common /size/ ncol, nrow,delsize,ndelrad

      RADIUS=MAX(2.001, 0.637*fwhm)
      SIGSQ=(fwhm/2.35482)**2
      NHALF=min(15,int(RADIUS))
      MIDDLE=NHALF+1
      NBOX=2*NHALF+1
      RADIUS=RADIUS**2
      SUMG=0.0
      SUMGSQ=0.0
      PIXELS=0.0
      do j=1,NBOX
         JSQ=(j-MIDDLE)**2
         do i=1,NBOX
            RSQ=REAL((i-MIDDLE)**2+JSQ)
            G(i,j)=EXP(-0.5*RSQ/SIGSQ)
            IF (RSQ .LE. RADIUS) THEN
               SKIP(i,j)=.FALSE.
               SUMG=SUMG+G(i,j)
               SUMGSQ=SUMGSQ+G(i,j)**2
               PIXELS=PIXELS+1.0
            ELSE
               SKIP(i,j)=.TRUE.
            END IF
         end do
      end do

      DENOM=SUMGSQ-(SUMG**2)/PIXELS
      SGOP=SUMG/PIXELS
      RELERR=1.0/DENOM
      RELERR=SQRT(RELERR)

      do j=1,NBOX
         do i=1,NBOX
C     ***c[good] = (c[good] - sumc)/sumcsq	; 
            IF (SKIP(i,j)) THEN
               G(i,j)=0.0
            ELSE
               G(i,j)=(G(i,j)-SGOP)/DENOM
            ENDIF
         end do
      end do
      
c     do j=1,NBOX,1
c        print '(20F4.2)',(G(i,j),i=1,NBOX,1)
c     end do
c     ====== Do the convolving
      do j=MIDDLE,nrow-MIDDLE
         do i=MIDDLE,ncol-MIDDLE
            locval=0
            do im=-NHALF,NHALF,1
               do jm=-NHALF,NHALF,1
                  locval=locval+img(i+im,j+jm)*G(MIDDLE+im,MIDDLE+jm)
               end do
            end do
            cimg(i,j)=int(locval)
         end do
      end do
      do j=1,NHALF
         do i=1,NHALF
            cimg(i,j)=0
            cimg(ncol-i,j)=0
            cimg(i,nrow-j)=0
            cimg(ncol-i,nrow-j)=0
         end do
      end do



      RETURN
      END

       include '../../common/rimage.f'
c       include './rimage.f'
       include '../../fitsio/fitsinput.f'
       include '../../common/calcsize.f'
