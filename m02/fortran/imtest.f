       program main
c
c     This is imtest.f and uses the exact same i/o as maggen.f
c
c     PVM main program model
       parameter (NDIM=4096,NSTARS=50000)
c
c  common blocks to store general information
	common/cb1/npsf,ndao(1:10),psfstars(1:2,1:10)
        common/cb2/COL,ROW,pfix,maxoff
        integer*2 nnc(1:NSTARS),rowval
        dimension coorsum(1:2,1:NSTARS)
        common/cb3/ncoo,nnc,coorsum
        common/cb4/radsc1,radsc2,radsc3,radsc4,radsc5,radsc6,xinner,
     .   xout,skymult,psfout,psfkill,psfexp,memp
        common/cb5/dx,dy,back,seeing
        character*80 name,name2
        common/cb6/name,name2
c
       integer*2 image(1:NDIM,1:NDIM)
       integer calcsize
       common image
c
c  Get data:
c 
c------------------------------------------------------------
c   Scaling-values for Aperture photometry, max offset
c------------------------------------------------------------
c
          open(54,file='APrad',status='old')
 
	  maxoff = 150
          read(54,*)radsc1
          read(54,*)radsc2
          read(54,*)radsc3
          read(54,*)radsc4
          read(54,*)radsc5
          read(54,*,end=99)radsc6
c     read(54,*,end=99)maxoff
 
   99     close(54)
 
 
c------------------------------------------------------------
c   Tuning sky-parameters
c------------------------------------------------------------
c
          open(54,file='SKYparam',status='old')
 
          read(54,*)xinner
          read(54,*)xout
          read(54,*)skymult
 
          close(54)
c------------------------------------------------------------
c   Tuning PSF-parameters
c------------------------------------------------------------
c
          open(54,file='PSFparam',status='old')
 
          read(54,*)psfout
          read(54,*)psfkill
          read(54,*)psfexp
          read(54,*)memp
 
          close(54)
c---------------------------------------------------------
c   List of PSF stars
c---------------------------------------------------------
       open(23,file='PSFstars',status='old')
 
       npsf=1
 
       write(6,*)'Reading psf.stars'
 
8      continue
       read(23,*,end=9)x,ndao(npsf),psfstars(1,npsf),psfstars(2,npsf)
          npsf=npsf+1
       go to 8
9      continue
       npsf=npsf-1
       close(23)
 
c----------------------------------------------------------
c   Image format
c---------------------------------------------------------
       open(10,file='FORMAT',status='old')
 
       read(10,*)COL
       read(10,*)ROW
       read(10,*)pfix
       do i=1,4
	read(10,*)
       enddo
       read(10,*) iscale
 
       close(10)
       if (COL.gt.NDIM.or.ROW.gt.NDIM) then
	print *,'Image too large: increase dimensions in program'
	stop
       endif
c----------------------------------------------------------
c  Read list of objects
c----------------------------------------------------------
       open(23,file='coor.SUM',status='old')
 
       ncoo=1
 
       write(6,*)'Reading coo.stars'
 
6      continue
          read(23,*,end=7)x,nnc(ncoo),coorsum(1,ncoo),coorsum(2,ncoo)
          ncoo=ncoo+1
       go to 6
7      continue
       ncoo=ncoo-1
       close(23)
c-------------------------------------------------------------
c   no .... Read bad pixel image
c-------------------------------------------------------------
       mmx=nint(COL)
       mmy=nint(ROW)
 
c   SGI convention defines record length given in words = 4 bytes
       Nrecl=calcsize(mmx,mmy)
 
c----------------------------------------------------------
c Reading the frame (photometry-frame)
c---------------------------------------------------------
 
       write(6,*)'Reading the frame....'
       open(21,file='input.IMAGE',status='old')
       read(21,'(a80)')name
       read(21,'(a80)')name2
       close(21)
          
       write(6,*)name
       write(6,*)name2

       call rimage(name,Nrecl,image,mmx,mmy,iscale)
 
       open(22,file=name2,status='unknown')

       rowval=100
      write(6,*)'Which row? '
      read(5,*)rowval

      do 15 i=1,mmx,1
c     do 14 j=1,mmy,1
         print*,image(i,rowval)
c     14           continue
 15   continue
 
 
      
      close(22)

      END

       include '../../common/rimage.f'
c       include './rimage.f'
       include '../../fitsio/fitsinput.f'
       include '../../common/calcsize.f'


