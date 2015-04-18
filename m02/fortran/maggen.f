       program main
c
c     PVM main program model
       parameter (NDIM=4096,NSTARS=50000)
c
c  common blocks to store general information
	common/cb1/npsf,ndao(1:10),psfstars(1:2,1:10)
        common/cb2/COL,ROW,pfix,maxoff
        integer*2 nnc(1:NSTARS)
        dimension coorsum(1:2,1:NSTARS)
        common/cb3/ncoo,nnc,coorsum
        common/cb4/radsc1,radsc2,radsc3,radsc4,radsc5,radsc6,xinner,
     .   xout,skymult,psfout,psfkill,psfexp,memp
        common/cb5/dx,dy,back,seeing
        character*80 name,name2
        common/cb6/name,name2
c
       integer*2 image(1:NDIM,1:NDIM),iemty(1:NDIM,1:NDIM)
       integer*2 izero(1:NDIM,1:NDIM)
       integer calcsize
       common image, izero, iemty
c
c  Get data:
c 
c------------------------------------------------------------
c   Scaling-values for Aperture photometry, max offset
c------------------------------------------------------------
c
          open(54,file='APrad',status='old')
 
	  maxoff = 50
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
c   Read bad pixel image
c-------------------------------------------------------------
       mmx=nint(COL)
       mmy=nint(ROW)
 
c   SGI convention defines record length given in words = 4 bytes
       Nrecl=calcsize(mmx,mmy)
 
       open (10,file='im.ZERO',access='direct',
     .          recl=Nrecl,status='old')
       read(10,rec=1) ((izero(i,j),i=1,mmx),j=1,mmy)
       close(10)
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
c      open (10,file=name,access='direct',
c    c          recl=Nrecl,status='old')
 
       open(22,file=name2,status='unknown')

 
c      read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
c      close(10)

c----------------------------------------------------------------
c   Call programs
c  ---------------------------------------------------------------
	write(6,'(''Calling dspot2'')')
	call dspot2(dx,dy)
	write(6,'(''Calling dsum'')')
	call dsum(dx,dy)
	call MAGseries
c
	end
c
	subroutine MAGseries
c
c      MAGseries.f - single frame version
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c
c This routine is the main-routine for the MOMF-software. It contains
c the major photometric reductions and all methods used in the software
c is described in the following paper;
c
c    Kjeldsen, Hans & Frandsen Soeren., 1992, "High-precision time-resolved
c    CCD photometry", Publ. of the Astron. Soc. of the Pacific, (PASP)
c    vol. 104, p. 413.
c
c The needed information on how to run the software is given in the manual
c which kan be found in the ../Manual-directory.
c
       parameter (NDIM=4096,NSTARS=50000,SATLIM=30000.0)
       dimension ndao(1:10),psfstars(1:2,1:10)
       common/cb1/npsf,ndao,psfstars
       common/cb2/COL,ROW,pfix,maxoff
       integer*2 nnc(1:NSTARS)
       dimension coorsum(1:2,1:NSTARS)
       common/cb3/ncoo,nnc,coorsum
       common/cb4/radsc1,radsc2,radsc3,radsc4,radsc5,radsc6,xinner,
     .   xout,skymult,psfout,psfkill,psfexp,memp
       common/cb5/dx,dy,back,seeing
       character*80 name,name2
       common/cb6/name,name2

       integer calcsize

       dimension SATARR(1:NSTARS)
       integer*2 image(1:NDIM,1:NDIM),iemty(1:NDIM,1:NDIM)
       integer*2 izero(1:NDIM,1:NDIM),nsttt(1:NSTARS)
       dimension APERTURE(1:6,1:NSTARS)
       dimension STARparam(1:2,1:NSTARS),off(1:2,1:NSTARS)
       dimension psf(1:2,1:10), sse(1:10,1:10)
       dimension coor(1:2,1:NSTARS), sse1(1:10)
       dimension PSFarr(1:10,-13:13,-13:13)
       dimension PSFcalc(-13:13,-13:13), EXPcalc(-13:13,-13:13)
       dimension DIScalc(-13:13,-13:13), PSFparam(1:3,1:10)
       dimension DIS2calc(-13:13,-13:13), sumMAG(1:NSTARS)
       dimension APzero(1:6),xq(1:6),x45coor(1:2,1:NSTARS)
       character*30 line,qqww
       character*20 stub
       common image, izero, iemty, coor, sumMAG, 
     .   APERTURE, STARparam, off, x45coor
 
 
          ns1=nint(xinner)
          xinner=xinner**2
          ns2=nint(xout)
          xout=xout**2
          mp=nint(psfout)
          mmx=nint(COL)
          mmy=nint(ROW)
c   SGI convention defines record length given in words = 4 bytes
          Nrecl=calcsize(mmx,mmy)
 
c
c Files for output-information: seeing, sky, offsets etc.
c
c
 
       open(33,file='basic.PHOT',status='new')
       open(34,file='offset.PHOT',status='new')
       open(44,file='saturated.PHOT',status='unknown')
 
       qqww='------------------------------'
c
c Definition of different values for FWHM/seeing used on different
c locations in the MAGseries.f - program
c

c---------------------------------------------------- 
          fwhm2=(seeing*0.6006)**2
          fwhm2=fwhm2/2.0
          fwhm1=fwhm2/2.0
          fwhm3=fwhm2*2.0
 
c---------------------------------------------------- 
          fwhm4=fwhm2*3.00
 
c---------------------------------------------------- 
          dist1=fwhm4/4.0
          dist2=fwhm4
          dist3=fwhm4*2.25
          dist4=fwhm4*4.0
 
c---------------------------------------------------- 
          vv1=dist1/-2.25
          vv2=dist2/-2.25
          vv3=dist3/-2.25
          vv4=dist4/-2.25
 
c---------------------------------------------------- 
 
          write(6,*)'Image :',name
 
          do 15 i=1,mmx,1
             do 14 j=1,mmy,1
                image(i,j)=image(i,j)*izero(i,j)
                iemty(i,j)=image(i,j)
14           continue
15        continue
 
c
c   Calculating new coordinates
c 
          do 21 ncoo2=1,ncoo

          coor(1,ncoo2)=coorsum(1,ncoo2)+dx
          coor(2,ncoo2)=coorsum(2,ncoo2)+dy
 
21        continue
 
          do 22 npsf2=1,npsf

          psf(1,npsf2)=psfstars(1,npsf2)+dx
          psf(2,npsf2)=psfstars(2,npsf2)+dy
 
22        continue
c
c   PSFarray: values: 100.0
c
          write(6,809)0,sqrt(dist1),sqrt(dist2),sqrt(dist3),sqrt(dist4)
809          format(f8.3,4f12.3)
          write(6,*)qqww,qqww
 
          do 95 k1=1,10,1
             do 94 k2=-13,13,1
                do 93 k3=-13,13,1
 
                   PSFarr(k1,k2,k3)=100.0
 
93              continue
94           continue
95        continue
 
          xmesky=0.0

          do 531 npsf2=1,npsf,1
 
          back=0.0
          sback=16384.0
          x1=psf(1,npsf2)
          x2=psf(2,npsf2)
    
          do 510 h=1,4,1
 
          s1=0.0
          s2=0.0
          s3=0.0
 
          s21=0.0
          s23=0.0
 
          do 502 n4=max(1,(nint(x1)-ns2)),min(mmx,(nint(x1)+ns2))
             do 501 n5=max(1,(nint(x2)-ns2)),min(mmy,(nint(x2)+ns2))
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-xinner) 501,500,500
500             continue
                   d=((float(image(n4,n5))-back)/sback)**2
                   s1=s1+float(image(n4,n5))*exp(-d)
                   s2=s2+(float(image(n4,n5))**2)*exp(-d)
                   s3=s3+exp(-d)
                   s21=s21+float(image(n4,n5))*exp(-d/4.0)
                   s23=s23+exp(-d/4.0)
501          continue
502       continue
    
          back=s1/s3
          s4=s2/s3
          sba=3.0*(s4-back**2)+1.0
          back=2.0*back-s21/s23
          if (sba-1.0) 504,504,503
503       continue
          sback=sqrt(sba)
504       continue
    
510       continue
 
          xmesky=xmesky+back
 
          back=back*skymult
          PSFparam(1,npsf2)=back
 
          do 530 h=1,3,1
c
c----------------------------------
c Init arr. s...
c
c----------------------------------
c
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
          s5=0.0
          s6=0.0
          s7=0.0
 
          do 522 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
             do 521 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-81.0) 520,520,521
520             continue
                   dexp1=exp(-dist/fwhm1)
                   dexp2=exp(-dist/fwhm2)
                   dexp3=exp(-dist/fwhm3)
                   dstar=float(image(n4,n5))-back
                   d1=dstar*dexp1
                   d2=dstar*dexp2
                   d3=dstar*dexp3
                   s1=s1+float(n4)*d2
                   s2=s2+float(n5)*d2
                   s3=s3+d1
                   s4=s4+d2
                   s5=s5+d3
                   s6=s6+dstar
                   s7=s7+dexp1
521          continue
522       continue
 
          x1=s1/s4
          x2=s2/s4
 
          d1=(x1-psf(1,npsf2))**2+0.01
          d2=(x2-psf(2,npsf2))**2+0.01
 
          x1=((x1/d1)+psf(1,npsf2))/((1.0/d1)+1.0)
          x2=((x2/d2)+psf(2,npsf2))/((1.0/d2)+1.0)
 
530       continue
 
          PSFparam(2,npsf2)=x1
          PSFparam(3,npsf2)=x2
 
          seeing1=sqrt(((s6-s3)*fwhm1)/s3)
          seeing2=sqrt(((s6-s4)*fwhm2)/s4)
          seeing3=sqrt(((s6-s5)*fwhm3)/s5)
          spsf1=(s3*s6)/((s6-s3)*fwhm1*3.1415927)
          spsf2=(s4*s6)/((s6-s4)*fwhm2*3.1415927)
          spsf3=(s5*s6)/((s6-s5)*fwhm3*3.1415927)
 
          qw1=seeing1
          qw2=seeing2
          qw3=seeing3
          qp1=spsf1
          qp2=spsf2
          qp3=spsf3
 
          do 410 iuu=1,4,1

c
c -------------------------------------------------------------
c The following lines are not included in the MOMF version 2.1
c -------------------------------------------------------------
c 
c ...........
c 
c         seeing1=sqrt(s3/(qp1*3.1415927-s3/fwhm1))
c         seeing2=sqrt(s4/(qp2*3.1415927-s4/fwhm2))
c         seeing3=sqrt(s5/(qp3*3.1415927-s5/fwhm3))
c         qw1=(seeing1+qw1)/2.0
c         qw2=(seeing2+qw2)/2.0
c         qw3=(seeing3+qw3)/2.0
c         qp1=(s3*((qw1**2)+fwhm1))/(3.1415927*(qw1**2)*fwhm1)
c         qp2=(s4*((qw2**2)+fwhm2))/(3.1415927*(qw2**2)*fwhm2)
c         qp3=(s5*((qw3**2)+fwhm3))/(3.1415927*(qw3**2)*fwhm3)
          spsf1=(s3*((qw1**2)+fwhm1))/(3.1415927*(qw1**2)*fwhm1)
          spsf2=(s4*((qw2**2)+fwhm2))/(3.1415927*(qw2**2)*fwhm2)
          spsf3=(s5*((qw3**2)+fwhm3))/(3.1415927*(qw3**2)*fwhm3)
          qp1=(spsf1+qp1)/2.0
          qp2=(spsf2+qp2)/2.0
          qp3=(spsf3+qp3)/2.0
          qw1=sqrt(s3/(qp1*3.1415927-s3/fwhm1))
          qw2=sqrt(s4/(qp2*3.1415927-s4/fwhm2))
          qw3=sqrt(s5/(qp3*3.1415927-s5/fwhm3))
 
410       continue
 
          seeing1=qw1
          seeing2=qw2
          seeing3=qw3
          spsf1=qp1
          spsf2=qp2
          spsf3=qp3
 
          seeing0=seeing1+0.5*seeing2-0.5*seeing3
 
          sse(1,npsf2)=seeing0
          sse(2,npsf2)=seeing1
          sse(3,npsf2)=seeing2
          sse(4,npsf2)=seeing3
 
          PSFarr(npsf2,0,0)=seeing0
          s0=spsf1+0.5*spsf2-0.5*spsf3
          sse(5,npsf2)=(s0/s3)*s7
 
c
c   Making the PSF-fit
c
 
          do 572 m4=-13,13,1
             do 571 m5=-13,13,1
 
                PSFcalc(m4,m5)=100.0
                EXPcalc(m4,m5)=1000.0
                DIScalc(m4,m5)=1000.0
 
571          continue
572       continue
 
          do 574 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 573 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                m4=n4-nint(x1)
                m5=n5-nint(x2)

                PSFcalc(m4,m5)=(float(image(n4,n5))-back)/s0
 
                if (PSFcalc(m4,m5)-1.0) 36,573,573
36              if (PSFcalc(m4,m5)) 573,573,37
37              continue
 
                dist=0.0

                do 39 md1=-1,1,1
                   do 38 md2=-1,1,1
                      d1=float(md1)/3.0
                      d2=float(md2)/3.0
                   dist=dist-(float(n4)-x1+d1)**2-(float(n5)-x2+d2)**2
38                 continue
39              continue
 
                dist=dist/9.0
 
                EXPcalc(m4,m5)=sqrt(dist/log(PSFcalc(m4,m5)))
                DIScalc(m4,m5)=dist
 
573          continue
574       continue

c
c---------------------
c Init arr. s and w
c---------------------
c
c
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
          w1=0.0
          w2=0.0
          w3=0.0
          w4=0.0
 
          do 576 m4=-13,13,1
             do 575 m5=-13,13,1
                   dist=DIScalc(m4,m5)
                   dexp1=exp(abs(dist+dist1)/vv1)
                   dexp2=exp(abs(dist+dist2)/vv2)
                   dexp3=exp(abs(dist+dist3)/vv3)
                   dexp4=exp(abs(dist+dist4)/vv4)
                   dstar=EXPcalc(m4,m5)
                   d1=dstar*dexp1
                   d2=dstar*dexp2
                   d3=dstar*dexp3
                   d4=dstar*dexp4
                   s1=s1+d1
                   s2=s2+d2
                   s3=s3+d3
                   s4=s4+d4
                   w1=w1+dexp1
                   w2=w2+dexp2
                   w3=w3+dexp3
                   w4=w4+dexp4
575          continue
576       continue
          
          seeing10=(2.0*s1)/w1-s2/w2
          write(6,809)seeing10,s1/w1,s2/w2,s3/w3,s4/w4
 
          sse(6,npsf2)=seeing10
          sse(7,npsf2)=s1/w1
          sse(8,npsf2)=s2/w2
          sse(9,npsf2)=s3/w3
          sse(10,npsf2)=s4/w4
          
531       continue
 
          xmesky=xmesky/float(npsf)
 
          do 534 ic=1,10,1
 
             s1=0.0
             s2=0.0
 
             do 532 npsf2=1,npsf,1

                s1=s1+sse(ic,npsf2)

532          continue
 
             s3=s1/float(npsf)
 
             do 5321 npsf2=1,npsf,1

                s2=s2+(sse(ic,npsf2)-s3)**2

5321         continue
 
             s2=s2/float(npsf)
             s4=sqrt(s2)
 
             if (s4) 5322,5322,5323
5322         s4=0.0000001
5323         continue
 
             s1=0.0
             s2=0.0
 
             do 533 npsf2=1,npsf,1

                d=(sse(ic,npsf2)-s3)/s4
                d=exp(-(d**2))
                s1=s1+sse(ic,npsf2)*d
                s2=s2+d

533          continue
 
             sse(ic,1)=s1/s2
             sse1(ic)=sse(ic,1)*1.6651
 
534       continue          
 
          write(6,*)qqww,qqww
          write(6,809)sse(6,1),sse(7,1),sse(8,1),sse(9,1),sse(10,1)
          write(6,809)sse1(6),sse1(7),sse1(8),sse1(9),sse1(10)
          write(6,*)qqww,qqww
          write(6,725)0.0,sqrt(fwhm1),sqrt(fwhm2),sqrt(fwhm3)
          write(6,*)qqww,qqww
          write(6,725)sse(1,1),sse(2,1),sse(3,1),sse(4,1)
          write(6,725)sse1(1),sse1(2),sse1(3),sse1(4)
          write(6,725)sse(5,1)
725          format(f20.3,3f12.3)
          write(6,*)qqww,qqww
 
          xmesee=(sse1(1)+sse1(2)+sse1(3)+sse1(4))/4.0
 
c
c   PSFarr - calculation
c

          do 1531 npsf2=1,npsf,1
 
          back=PSFparam(1,npsf2)
          x1=PSFparam(2,npsf2)
          x2=PSFparam(3,npsf2)
 
          sumPSFc=0.0
          sumEXPc=0.0
 
 
          do 1574 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 1573 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                m4=n4-nint(x1)
                m5=n5-nint(x2)
 
                dist=0.0

                do 1139 md1=-1,1,1
                   do 1138 md2=-1,1,1
                      d1=float(md1)/3.0
                      d2=float(md2)/3.0
                   dist=dist-(float(n4)-x1+d1)**2-(float(n5)-x2+d2)**2
1138               continue
1139            continue
 
                dist=dist/9.0
 
                w0=abs(dist)
                w1=abs(dist+dist1)
                w2=abs(dist+dist2)
                w3=abs(dist+dist3)
                w4=abs(dist+dist4)
                wv0=w1*w2*w3*w4
                wv1=w0*w2*w3*w4
                wv2=w0*w1*w3*w4
                wv3=w0*w1*w2*w4
                wv4=w0*w1*w2*w3
                wv5=wv0+wv1+wv2+wv3+wv4
                sseeing=wv0*(sse(6,1)**2)+wv1*(sse(7,1)**2)
                sseeing=sseeing+wv2*(sse(8,1)**2)+wv3*(sse(9,1)**2)
                sseeing=(sseeing+wv4*(sse(10,1)**2))/wv5
 
          PSFcalc(m4,m5)=(float(image(n4,n5))-back)*float(izero(n4,n5))
          EXPcalc(m4,m5)=exp(dist/sseeing)*float(izero(n4,n5))
          DIScalc(m4,m5)=dist
          DIS2calc(m4,m5)=sseeing
 
                sumPSFc=sumPSFc+PSFcalc(m4,m5)*EXPcalc(m4,m5)**2
                sumEXPc=sumEXPc+EXPcalc(m4,m5)**3
 
1573         continue
1574      continue
 
          s0=sumPSFc/sumEXPc

c
c----------------------------------------------------------------
c PSF information - output to screen...
c
c----------------------------------------------------------------
c
c
 
       s11=s0*EXPcalc(0,-3)
       s=PSFcalc(0,-3)-s11
c      write(6,829)0,-3,DIScalc(0,-3),DIS2calc(0,-3),s11,PSFcalc(0,-3),s
       s11=s0*EXPcalc(0,-2)
       s=PSFcalc(0,-2)-s11
c      write(6,829)0,-2,DIScalc(0,-2),DIS2calc(0,-2),s11,PSFcalc(0,-2),s
       s11=s0*EXPcalc(0,-1)
       s=PSFcalc(0,-1)-s11
c      write(6,829)0,-1,DIScalc(0,-1),DIS2calc(0,-1),s11,PSFcalc(0,-1),s
       s11=s0*EXPcalc(0,0)
       s=PSFcalc(0,0)-s11
c      write(6,829)0,0,DIScalc(0,0),DIS2calc(0,0),s11,PSFcalc(0,0),s
       s11=s0*EXPcalc(0,1)
       s=PSFcalc(0,1)-s11
c      write(6,829)0,1,DIScalc(0,1),DIS2calc(0,1),s11,PSFcalc(0,1),s
       s11=s0*EXPcalc(0,2)
       s=PSFcalc(0,2)-s11
c      write(6,829)0,2,DIScalc(0,2),DIS2calc(0,2),s11,PSFcalc(0,2),s
       s11=s0*EXPcalc(0,3)
       s=PSFcalc(0,3)-s11
c      write(6,829)0,3,DIScalc(0,3),DIS2calc(0,3),s11,PSFcalc(0,3),s
829       format(2i4,2f10.3,3f10.2)
c         write(6,*)qqww,qqww
 
          do 1576 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 1575 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                m4=n4-nint(x1)
                m5=n5-nint(x2)
 
                   if (izero(n4,n5)) 1575,1575,1177
1177               continue
                   
                PSFarr(npsf2,m4,m5)=PSFcalc(m4,m5)/s0-EXPcalc(m4,m5)
 
                if (psfkill-0.5) 1575,1575,1178
 
1178   PSFarr(npsf2,m4,m5)=PSFarr(npsf2,m4,m5)*EXPcalc(m4,m5)**psfexp
 
1575         continue
1576      continue
          
1531      continue
 
c
c  Calculation the PSFarray
c

          do 2535 ix=-13,13,1
             do 2534 jy=-13,13,1
 
                s1=0.0
                s2=0.0
 
                do 2532 npsf2=1,npsf,1

                   s1=s1+PSFarr(npsf2,ix,jy)
                   s2=s2+PSFarr(npsf2,ix,jy)**2

2532            continue
 
                s3=s1/float(npsf)
                s2=s2/float(npsf)
c
c UNDER/OVER-FLOW check - FORTRAN - check
c Hans Kjeldsen, 26.10.92 - ESO/Garching
c
		if (s2-s3**2) 25321,25321,25322
25321           s4=0.0
		go to 25323
25322           s4=sqrt(s2-s3**2)
25323           continue
                sorig=(s4**2)/50.0
 
                do 25331 icle=1,4,1
 
                s1=0.0
                s2=0.0
                s22=0.0
 
                do 2533 npsf2=1,npsf,1

c
c UNDER/OVER-FLOW check - FORTRAN - check
c Hans Kjeldsen, 26.10.92 - ESO/Garching
c
		   if (s4-(1e-10)) 2533,2533,25324
25324              d=(PSFarr(npsf2,ix,jy)-s3)/s4
                   d=d**2
                   if (d-5.0) 25329,25329,2533
25329              d=exp(-1.0*d)
                   s1=s1+PSFarr(npsf2,ix,jy)*d
                   s22=s22+(PSFarr(npsf2,ix,jy)**2)*d
                   s2=s2+d

2533            continue
 
		if (s2) 25342,25342,25343
25342           s3=0.0
		s2=0.0
		sorig=0.0
                go to 25344
 
25343           s3=s1/s2
                s2=s22/s2
25344           continue
c
c UNDER/OVER-FLOW check - FORTRAN - check
c Hans Kjeldsen, 26.10.92 - ESO/Garching
c
		if (s2-s3**2+sorig) 25331,25331,25341
25341           s4=1.2*sqrt(s2-s3**2+sorig)
 
25331           continue
 
                PSFarr(1,ix,jy)=s3
 
2534         continue          
2535      continue          
 

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,-9)*1000.0
          q2=PSFarr(1,-2,-9)*1000.0
          q3=PSFarr(1,-1,-9)*1000.0
          q4=PSFarr(1,0,-9)*1000.0
          q5=PSFarr(1,1,-9)*1000.0
          q6=PSFarr(1,2,-9)*1000.0
          q7=PSFarr(1,9,-9)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,-2)*1000.0
          q2=PSFarr(1,-2,-2)*1000.0
          q3=PSFarr(1,-1,-2)*1000.0
          q4=PSFarr(1,0,-2)*1000.0
          q5=PSFarr(1,1,-2)*1000.0
          q6=PSFarr(1,2,-2)*1000.0
          q7=PSFarr(1,9,-2)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,-1)*1000.0
          q2=PSFarr(1,-2,-1)*1000.0
          q3=PSFarr(1,-1,-1)*1000.0
          q4=PSFarr(1,0,-1)*1000.0
          q5=PSFarr(1,1,-1)*1000.0
          q6=PSFarr(1,2,-1)*1000.0
          q7=PSFarr(1,9,-1)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,0)*1000.0
          q2=PSFarr(1,-2,0)*1000.0
          q3=PSFarr(1,-1,0)*1000.0
          q4=PSFarr(1,0,0)*1000.0
          q5=PSFarr(1,1,0)*1000.0
          q6=PSFarr(1,2,0)*1000.0
          q7=PSFarr(1,9,0)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,1)*1000.0
          q2=PSFarr(1,-2,1)*1000.0
          q3=PSFarr(1,-1,1)*1000.0
          q4=PSFarr(1,0,1)*1000.0
          q5=PSFarr(1,1,1)*1000.0
          q6=PSFarr(1,2,1)*1000.0
          q7=PSFarr(1,9,1)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,2)*1000.0
          q2=PSFarr(1,-2,2)*1000.0
          q3=PSFarr(1,-1,2)*1000.0
          q4=PSFarr(1,0,2)*1000.0
          q5=PSFarr(1,1,2)*1000.0
          q6=PSFarr(1,2,2)*1000.0
          q7=PSFarr(1,9,2)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          q1=PSFarr(1,-9,9)*1000.0
          q2=PSFarr(1,-2,9)*1000.0
          q3=PSFarr(1,-1,9)*1000.0
          q4=PSFarr(1,0,9)*1000.0
          q5=PSFarr(1,1,9)*1000.0
          q6=PSFarr(1,2,9)*1000.0
          q7=PSFarr(1,9,9)*1000.0

c ---------------------------------------------------- 
c 
c         write(6,819)q1,q2,q3,q4,q5,q6,q7

c ---------------------------------------------------- 
c 
          write(6,*)qqww,qqww
819          format(7f8.2)

c ------------------------------------------------------
c ----------- Begin loop 'ncoo2' -----------------------
c ----------- loop through all the stars ---------------

          do 3531 ncoo2=1,ncoo,1
 
          back=0.0
          sback=16384.0
          x1=coor(1,ncoo2)
          x2=coor(2,ncoo2)
 
 
          do 3510 h=1,4,1
 
          s1=0.0
          s2=0.0
          s3=0.0
  
          s21=0.0
          s23=0.0
 
          do 3502 n4=max(1,(nint(x1)-ns2)),min(mmx,(nint(x1)+ns2))
             do 3501 n5=max(1,(nint(x2)-ns2)),min(mmy,(nint(x2)+ns2))
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-xinner) 3501,3500,3500
3500            continue
                   d=((float(iemty(n4,n5))-back)/sback)**2
                   s1=s1+float(iemty(n4,n5))*exp(-d)
                   s2=s2+(float(iemty(n4,n5))**2)*exp(-d)
                   s3=s3+exp(-d)
                   s21=s21+float(iemty(n4,n5))*exp(-d/4.0)
                   s23=s23+exp(-d/4.0)
3501         continue
3502      continue
    
          if (s3) 3510,3510,35021
35021     back=s1/s3
          s4=s2/s3
          sba=3.0*(s4-back**2)+1.0
          back=2.0*back-s21/s23
          if (sba-1.0) 3504,3504,3503
3503      continue
          sback=sqrt(sba)
3504      continue
    
3510      continue
 
          back=back*skymult
 
 
          do 3530 h=1,3,1
 

c ---------------------------------------------------- 
c 
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
          s5=0.0
          s6=0.0
          s7=0.0
 
          do 3522 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
             do 3521 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-81.0) 3520,3520,3521
3520            continue
                   dexp2=exp(-dist/fwhm2)
                   dstar=float(iemty(n4,n5))-back
                   d2=dstar*dexp2
                   s1=s1+float(n4)*d2
                   s2=s2+float(n5)*d2
                   s4=s4+d2
3521         continue
3522      continue
 
 
          if (s4) 3525,3525,3526
3525         x1=coor(1,ncoo2)
             x2=coor(2,ncoo2)
             go to 3777
3526      continue
 
          x1=s1/s4
          x2=s2/s4
 
          d1=abs(x1-coor(1,ncoo2))
          if (d1-400.0) 35252,35252,35251
35251     d1=400.0
35252     d1=exp(sqrt(d1)-2.5)
          d2=abs(x2-coor(2,ncoo2))
          if (d2-400.0) 35254,35254,35253
35253     d2=400.0
35254     d2=exp(sqrt(d2)-2.5)
 
          x1=((x1/d1)+coor(1,ncoo2))/((1.0/d1)+1.0)
          x2=((x2/d2)+coor(2,ncoo2))/((1.0/d2)+1.0)

3530      continue
 
3777      continue
 
c ----------------------------------------------------
c ORIG: if out of image: reset to orig centroid
c Changed to use orig coor.
c and later set all aperture phot to 99.999
c The q[1.6] variables are not being used
c ----------------------------------------------------

 
c          if (x1-float(mmx)) 35262,35262,35261
c35261     x1=coorsum(1,ncoo2)
c          coor(1,ncoo2)=coorsum(1,ncoo2)
c35262     if (x1-1.0) 35263,35264,35264
c35263     x1=coorsum(1,ncoo2)
c          coor(1,ncoo2)=coorsum(1,ncoo2)
c35264     if (x2-float(mmy)) 35266,35266,35265
c35265     x2=coorsum(2,ncoo2)
c          coor(2,ncoo2)=coorsum(2,ncoo2)
c35266     if (x2-1.0) 35267,35268,35268
c35267     x2=coorsum(2,ncoo2)
c          coor(2,ncoo2)=coorsum(2,ncoo2)
c35268     continue
 
 
          STARparam(1,ncoo2)=x1
          STARparam(2,ncoo2)=x2

          sumPSFc=0.0
          sumEXPc=0.0
 
          do 4574 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 4573 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                m4=n4-nint(x1)
                m5=n5-nint(x2)
 
                dist=0.0

                do 4139 md1=-1,1,1
                   do 4138 md2=-1,1,1
                      d1=float(md1)/3.0
                      d2=float(md2)/3.0
                   dist=dist-(float(n4)-x1+d1)**2-(float(n5)-x2+d2)**2
4138               continue
4139            continue
 
                dist=dist/9.0
 
                w0=abs(dist)
                w1=abs(dist+dist1)
                w2=abs(dist+dist2)
                w3=abs(dist+dist3)
                w4=abs(dist+dist4)

c ---------------------------------------------------- 
c 
                wv0=w1*w2*w3*w4
                wv1=w0*w2*w3*w4
                wv2=w0*w1*w3*w4
                wv3=w0*w1*w2*w4
                wv4=w0*w1*w2*w3

c ---------------------------------------------------- 
c 
                wv5=wv0+wv1+wv2+wv3+wv4

c ---------------------------------------------------- 
c 
                sseeing=wv0*(sse(6,1)**2)+wv1*(sse(7,1)**2)
                sseeing=sseeing+wv2*(sse(8,1)**2)+wv3*(sse(9,1)**2)
                sseeing=(sseeing+wv4*(sse(10,1)**2))/wv5
 
                PSFcalc(m4,m5)=(float(iemty(n4,n5))-back)
                EXPcalc(m4,m5)=exp(dist/sseeing)
                gg=EXPcalc(m4,m5)**2
 
                sumPSFc=sumPSFc+PSFcalc(m4,m5)*float(izero(n4,n5))*gg
                sumEXPc=sumEXPc+EXPcalc(m4,m5)*float(izero(n4,n5))*gg
 
4573         continue
4574      continue
 
 
          if (sumEXPc-0.0001) 4881,4881,4880
4880      s0=sumPSFc/sumEXPc
          go to 4882
4881      s0=0.0
4882      continue
 
          if (s0) 4881,4883,4883
4883      continue
 
          sumMAG(ncoo2)=0.0
 
          do 5576 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 5575 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                m4=n4-nint(x1)
                m5=n5-nint(x2)
 
                   nsignal=nint(s0*(EXPcalc(m4,m5)+PSFarr(1,m4,m5)))
                   sumMAG(ncoo2)=sumMAG(ncoo2)+float(nsignal)
 
                   if (izero(n4,n5)) 5178,5178,5177
5177               continue
                   
                   iemty(n4,n5)=iemty(n4,n5)-nsignal
                   go to 5575
5178               continue
                   iemty(n4,n5)=nint(back)
                   
5575         continue
5576      continue
 
c
c----------Including larger PSF-subtraction-----------------------------
c
c----------HK/STScI/Baltimore/92/03/30----------------------------------
c
 
          dsw1=0.0
          dsw2=0.0
          dsw3=0.0
 
          do 21800 n4=-13,13,1
             do 21799 n5=-13,13,1
 
                ndlocal=n4**2+n5**2
 
                if (ndlocal-220) 21797,21799,21799
21797           if (ndlocal-170) 21799,21799,21798
21798              dsw1=dsw1+1
                   dsw2=dsw2+EXPcalc(n4,n5)+PSFarr(1,n4,n5)
                   dsw3=dsw3+float(ndlocal)
 
21799        continue
21800     continue
 
          dsw2=dsw2/dsw1
          dsw3=dsw3/dsw1
 
          if (dsw2*s0-0.5) 22000,22000,21805
21805        gaussi=dsw3/log(dsw2)
             if (sse(6,1)**2+gaussi) 21806,21807,21807
21806        gaussi=3.0*sqrt(-1.0*gaussi)-2.0*sse(6,1)
             gaussi=-1.0*gaussi**2
21807        s0new=s0*dsw2/(exp(dsw3/gaussi))
 
          do 25576 n4=max(1,(nint(x1)-mp)),min(mmx,(nint(x1)+mp)),1
          do 25575 n5=max(1,(nint(x2)-mp)),min(mmy,(nint(x2)+mp)),1
 
                mm4=n4-nint(x1)
 
                if (mm4**2-170) 25175,25175,25176

25175           mm5=n5-nint(x2)
 
                if (mm5**2-170) 25575,25575,25176

25176           continue

                fm4=float(n4)-x1
                fm5=float(n5)-x2
                fm6=fm4**2+fm5**2
 
                   nsignal=nint(s0new*exp(fm6/gaussi))
                   sumMAG(ncoo2)=sumMAG(ncoo2)+float(nsignal)
 
                   if (izero(n4,n5)) 25178,25178,25177
25177              continue
                   
                   iemty(n4,n5)=iemty(n4,n5)-nsignal
                   go to 25575
25178              continue
                   iemty(n4,n5)=nint(back)
                   
25575        continue
25576     continue
 
22000     continue
c
c
c----------END: HK/STScI/Baltimore/92/03/30----------------------------------
c
c
          
          if (sumMAG(ncoo2))5590,5590,5591
5590          xmagnn=99.999
              go to 5592
5591          xmagnn=10.0-2.5*log10(sumMAG(ncoo2))
5592      continue
  
          if (x1-float(mmx))55922,55922,55921
55921     x1=float(mmx)
          coor(1,ncoo2)=x1
          back=0.0
55922     if (x1-1.0)55923,55924,55924
55923     x1=1.0
          coor(1,ncoo2)=x1
          back=0.0
55924     if (x2-float(mmy))55926,55926,55925
55925     x2=float(mmy)
          coor(2,ncoo2)=x2
          back=0.0
55926     if (x2-1.0)55927,55928,55928
55927     x2=1.0
          coor(2,ncoo2)=x2
          back=0.0
55928     continue
c          write(6,5599)nnc(ncoo2),x1,x2,coor(1,ncoo2),
c     +      coor(2,ncoo2),back,xmagnn,iemty(nint(x1),nint(x2))
c5599            format(i5,f10.2,f8.2,f10.2,f8.2,f11.2,f12.3,i8)
          off(1,ncoo2)=x1-coorsum(1,ncoo2)
          off(2,ncoo2)=x2-coorsum(2,ncoo2)
          
3531      continue
 
c ----------- End loop 'ncoo2' -------------------------
c ------------------------------------------------------

 
          do 5534 ic=1,2,1
 
             s1=0.0
             s2=0.0
 
             do 5532 ncoo2=1,ncoo,1

                s1=s1+off(ic,ncoo2)
                s2=s2+off(ic,ncoo2)**2

5532         continue
 
             s3=s1/float(ncoo)
             s2=s2/float(ncoo)
             s4=sqrt(s2-s3**2)
 
             s1=0.0
             s2=0.0
 
             do 5533 ncoo2=1,ncoo,1

                d=(off(ic,ncoo2)-s3)/s4
                d=exp(-(d**2))
                s1=s1+off(ic,ncoo2)*d
                s2=s2+d

5533         continue
 
             off(ic,1)=s1/s2

5534      continue
 
          lname = len(name2)
          n1 = lname
          do i=1,lname
            if (name2(n1:n1).eq.'/') then 
		n1 = n1 + 1
		go to 100
	    endif
            n1 = n1 - 1
          enddo
  100     continue
	  stub = name2(n1:)
c         write(6,'(2f10.3,i8,a20)')off(1,1),off(2,1),ncoo,stub
          write(34,'(2f10.3,i8,5x,a20)')off(1,1),off(2,1),ncoo,stub
c          write(6,'(4f10.3)')off(1,1),off(2,1),xmesky,xmesee
          write(33,'(4f10.3)')off(1,1),off(2,1),xmesky,xmesee

c------------------------------------------------------
          rad00=1.665*sqrt(fwhm2)

c------------------------------------------------------
          rad1=rad00*radsc1
          rad2=rad00*radsc2
          rad3=rad00*radsc3
          rad4=rad00*radsc4
          rad5=rad00*radsc5
          rad6=rad00*radsc6

c------------------------------------------------------
          drad1=rad1**2
          drad2=rad2**2
          drad3=rad3**2
          drad4=rad4**2
          drad5=rad5**2
          drad6=rad6**2

c------------------------------------------------------
 
          do 7531 ncoo2=1,ncoo,1
             nsttt(ncoo2)=nnc(ncoo2)
7531      continue
 
          nminst=ncoo+1
 
c --------- NOW for a big loop over all the stars ----

          do 7537 incoo=1,ncoo,1
 
          do 7533 ncoo3=1,ncoo,1
             if (nsttt(ncoo3)-nminst) 7532,7532,7533
7532            ncoo2=ncoo3
                nminst=nsttt(ncoo3)
7533      continue
 
          nsttt(ncoo2)=ncoo+1
          nminst=ncoo+1
 
          back=0.0
          sback=16384.0
          x1=STARparam(1,ncoo2)
          x2=STARparam(2,ncoo2)
    
          do 7510 h=1,4,1
 
          s1=0.0
          s2=0.0
          s3=0.0
 
          s21=0.0
          s23=0.0
 
          do 7502 n4=max(1,(nint(x1)-ns2)),min(mmx,(nint(x1)+ns2))
             do 7501 n5=max(1,(nint(x2)-ns2)),min(mmy,(nint(x2)+ns2))
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-xinner) 7501,7500,7500
7500            continue
                   d=((float(iemty(n4,n5))-back)/sback)**2
                   s1=s1+float(iemty(n4,n5))*exp(-d)
                   s2=s2+(float(iemty(n4,n5))**2)*exp(-d)
                   s3=s3+exp(-d)
                   s21=s21+float(iemty(n4,n5))*exp(-d/4.0)
                   s23=s23+exp(-d/4.0)
7501         continue
7502      continue
    
          back=s1/s3
          s4=s2/s3
          sba=3.0*(s4-back**2)+1.0
          back=2.0*back-s21/s23
          if (sba-1.0) 7504,7504,7503
7503      continue
          sback=sqrt(sba)
7504      continue
    
7510      continue
 
          back=back*skymult
 
          n45=nnc(ncoo2)
          x45coor(1,n45)=STARparam(1,ncoo2)
          x45coor(2,n45)=STARparam(2,ncoo2)
          do 8120 in45=1,6,1
             APERTURE(in45,n45)=sumMAG(ncoo2)
8120      continue
 
          do 8574 n4=max(1,(nint(x1)-13)),min(mmx,(nint(x1)+13)),1
             do 8400 n5=max(1,(nint(x2)-13)),min(mmy,(nint(x2)+13)),1
 
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                fff=float(iemty(n4,n5))-back
                if (drad6-dist)8400,8300,8300
8300               APERTURE(6,n45)=APERTURE(6,n45)+fff
                if (drad5-dist)8400,8301,8301
8301               APERTURE(5,n45)=APERTURE(5,n45)+fff
                if (drad4-dist)8400,8302,8302
8302               APERTURE(4,n45)=APERTURE(4,n45)+fff
                if (drad3-dist)8400,8303,8303
8303               APERTURE(3,n45)=APERTURE(3,n45)+fff
                if (drad2-dist)8400,8304,8304
8304               APERTURE(2,n45)=APERTURE(2,n45)+fff
                if (drad1-dist)8400,8305,8305
8305               APERTURE(1,n45)=APERTURE(1,n45)+fff
 
8400         continue
8574      continue
 
          if (APERTURE(1,n45))9000,9000,9001
9000          q1=99.999
              go to 9002
9001          q1=10.0-2.5*log10(APERTURE(1,n45))
9002      if (APERTURE(2,n45))9003,9003,9004
9003          q2=99.999
              go to 9005
9004          q2=10.0-2.5*log10(APERTURE(2,n45))
9005      if (APERTURE(3,n45))9006,9006,9007
9006          q3=99.999
              go to 9008
9007          q3=10.0-2.5*log10(APERTURE(3,n45))
9008      if (APERTURE(4,n45))9009,9009,9010
9009          q4=99.999
              go to 9011
9010          q4=10.0-2.5*log10(APERTURE(4,n45))
9011      if (APERTURE(5,n45))9012,9012,9013
9012          q5=99.999
              go to 9014
9013          q5=10.0-2.5*log10(APERTURE(5,n45))
9014      if (APERTURE(6,n45))9015,9015,9016
9015          q6=99.999
              go to 9017
9016          q6=10.0-2.5*log10(APERTURE(6,n45))
9017      continue
 
c          write(6,'(i5,3f8.2,6f8.3)')n45,x1,x2,back,q1,q2,q3,q4,q5,q6
7537      continue
c
c          write(6,'(f28.4,5f10.5)')rad1,rad2,rad3,rad4,rad5,rad6
c
c This format reflects the use of standard AP values - only 4 decimals
c
          write(22,'(f28.4,5f10.5)')rad1,rad2,rad3,rad4,rad5,rad6
c

c ---------------------------------------------------- 
c 
          sumAP1=0.0
          sumAP2=0.0
          sumAP3=0.0
          sumAP4=0.0
          sumAP5=0.0
          sumAP6=0.0

c ---------------------------------------------------- 
c 
 
          do 7637 inpsf=1,npsf,1

c ---------------------------------------------------- 
c 
              sumAP1=sumAP1+APERTURE(1,ndao(inpsf))
              sumAP2=sumAP2+APERTURE(2,ndao(inpsf))
              sumAP3=sumAP3+APERTURE(3,ndao(inpsf))
              sumAP4=sumAP4+APERTURE(4,ndao(inpsf))
              sumAP5=sumAP5+APERTURE(5,ndao(inpsf))
              sumAP6=sumAP6+APERTURE(6,ndao(inpsf))

c ---------------------------------------------------- 
c 
7637      continue

c
c-------------------------------------------
c Zero-values for aperture photometry...
c
c-------------------------------------------
c
c

          APzero(1)=2.5*log10(sumAP1)
          APzero(2)=2.5*log10(sumAP2)
          APzero(3)=2.5*log10(sumAP3)
          APzero(4)=2.5*log10(sumAP4)
          APzero(5)=2.5*log10(sumAP5)
          APzero(6)=2.5*log10(sumAP6) 

c          write(6,'(f28.4,5f10.5)')APzero(1),APzero(2),APzero(3),
c     +       APzero(4),APzero(5),APzero(6)
          write(22,'(f28.4,5f10.5)')APzero(1),APzero(2),APzero(3),
     +       APzero(4),APzero(5),APzero(6)

c ----------------------------------
c TESTING

          do 9288 n45=1,ncoo,1

             x1=x45coor(1,n45)
             x2=x45coor(2,n45)

c *******SATURATION TESTING within 5x5 box
             SATARR(n45)=0
             do 9268 n4=max(1,(nint(x1)-5)),min(mmx,(nint(x1)+5))
                do 9267 n5=max(1,(nint(x2)-5)),min(mmy,(nint(x2)+5))
                   if (float(image(n4,n5)) - SATLIM) 9266,9264,9264
 9264              SATARR(n45)=1
c     write(6,9265)'Saturated : ',n45,x1,x2
c     9265           format(a12,i5,2f10.2)
 9266              continue
 9267           continue
 9268        continue
c *******END SAT TEST


             do 9287 n46=1,6,1

c ----------------------------------
c *******NOT IN IMAGE TESTING
c added 12 lines here
 9270           if (x1-1)9271,9271,9272
 9271           xq(n46)=99.9999
                go to 9283
 9272           if (x2-1)9273,9273,9274
 9273           xq(n46)=99.9999
                go to 9283
 9274           if (x1-mmx)9276,9275,9275
 9275           xq(n46)=99.9999
                go to 9283
 9276           if (x2-mmy)9278,9277,9277
 9277           xq(n46)=99.9999
                go to 9283
c     *******SATURATION TESTING
             
 9278           continue
                if (SATARR(n45))9280,9280,9279
 9279           xq(n46)=99.9999
                go to 9283
c *******END OF SAT AND NOT IN IM. TEST
c ----------------------------------

 9280           if (APERTURE(n46,n45))9281,9281,9282
 9281           xq(n46)=99.9999
                go to 9283
 9282           APz=2.5*log10(APERTURE(n46,n45))
                xq(n46)=APzero(n46)-APz
 9283           continue

 9287        continue

c*******SATURATION WRITING
92871        if (SATARR(n45))92876,92876,92872
92872        write(6,92873)'Saturated : ',n45,x1,x2
92873        format(a12,i5,2f10.2)
92874        write(44,92875)n45,x1,x2,'  ',stub
92875        format(i5,2f10.2,a5,a20)
92876        continue
c     *******END OF SAT WRITING

c          write(6,'(i5,2f8.2,6f10.5)')n45,x1,x2,xq(1),xq(2),xq(3),
c     +        xq(4),xq(5),xq(6)
             write(22,'(i5,2f8.2,6f10.5)')n45,x1,x2,xq(1),xq(2),xq(3),
     +            xq(4),xq(5),xq(6)
 
 9288     continue
 
          close(22)
          close(44)

          if (memp) 300,300,9289
 9289     continue
 
c-------- writing out the empty-image on file: empty.frame ---------------------
 
          write(6,*)'writing out the empty-image on file: empty.frame'
 
          open (10,file='empty.frame',access='direct',
     c         recl=Nrecl,status='unknown')
 
          write(10,rec=1) ((iemty(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
 
          write(6,*)'writing out the PSF-arr: PSFarr.test'
 
          open (10,file='PSFarr.test',status='unknown')
 
          do 9390 j=-13,13,1

             write(10,9389)(PSFarr(1,i,j),i=-13,13)
9389            format(27f10.6)
 
9390      continue
          close(10)
 
          write(6,*)'writing out the EXP-arr: EXParr.test'
 
          open (10,file='EXParr.test',status='unknown')
 
          do 9391 j=-13,13,1

             write(10,9389)(EXPcalc(i,j),i=-13,13)
 
9391      continue
          close(10)
 
c-------------------------------------------------------------------------------
 
300       continue

c ---------------------------------------------------- 
c 
 
          close(34)
 
          end
c
c   
       subroutine dspot2(x,y)
c
c
c      Dspot2.f - single frame version
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c     
c---------------------------------------------
c      First-version: DAOspot2.f
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c      Special version: ESO 1993: Large offsets...
c---------------------------------------------
c
c  NDIM is the maximum dimension of the image, maxoff max offset
c  in x and y
c
       parameter (NDIM=4096)
       dimension coorstars(1:2,1:10),ndao(1:10)
       common/cb1/ncoo,ndao,coorstars
       common/cb2/COL,ROW,pfix,maxoff

       integer*2 image(1:NDIM,1:NDIM)
       common image

c 
c Reading input-info: stars used to calculate offsets
c
c
 
       fwhm=5.0
       fwhm2=(fwhm*0.6005)**2
 
       mmx=nint(COL)
       mmy=nint(ROW)
 
c
c   Calculating off-set

	  write(6,100) maxoff
  100	  format('Maximum offset assumed is',i8)
          s1=0.0
 
          do 502 n4=-maxoff,maxoff,1
             do 501 n5=-maxoff,maxoff,1
                
c   Internal loop

		xval=0.0

                do 531 ncoo2=1,ncoo
              
                   n1a=nint(coorstars(1,ncoo2))+n4
                   n2a=nint(coorstars(2,ncoo2))+n5
c     print*,n1a,coorstars(1,ncoo2),n4
c     print*,n2a,coorstars(2,ncoo2),n5
                   nval2=max(1,image(n1a,n2a))
 
		   xval=xval+log(float(nval2))

531             continue
            
		xval=exp(0.1*xval)
                if (xval-s1) 501,501,5009
5009            s1=xval
                x1=float(n4)
	        x2=float(n5)
 
501          continue
502       continue
    
          back=s1
          seeing=fwhm
 
          write(6,413)'(x,y) , ADU , FWHM (pix)',ncoo,x1,x2,back,seeing
413          format(a25,i5,4f10.3)
          x = x1
          y = x2
 
300       continue
 
          end
c
c
c
       subroutine dsum(offxax,offyax)
c
c
c      Dsum.f - single frame version
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c     
c---------------------------------------------
c      First-version: DAOsum.f
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c
c---------------------------------------------------------------

c Calculation of sum-image..
c
       parameter (NDIM=4096)
       common/cb1/ncoo,ndao(1:10),coorstars(1:2,1:10)
       common/cb2/COL,ROW,pfix,maxoff
       common/cb5/dx,dy,back,seeing
       character*80 name,name2
       common/cb6/name,name2
c
       integer*2 image(1:NDIM,1:NDIM),imzero(1:NDIM,1:NDIM)
       common image, imzero
       dimension xoff(1:4,1:10),vi(1:2),vj(1:2)
       integer idelta(1:2), jdelta(1:2)
       character*20 stub
 
       fwhm=5.0
       fwhm2=(fwhm*0.6005)**2
 
       mmx=nint(COL)
       mmy=nint(ROW)
 
c
c Basic input from files
c
 
1546      open(16,file='offset.SUM',status='new')
          ntt=8
c
c   Calculating off-set
c 
          do 531 ncoo2=1,ncoo

          x1=coorstars(1,ncoo2)+offxax
          x2=coorstars(2,ncoo2)+offyax

          back=0.0
          sback=16384.0
 
          do 510 h=1,8,1
 
          s1=0.0
          s2=0.0
          s3=0.0
 
          do 502 n4=(nint(x1)-22),(nint(x1)+22),1
             do 501 n5=(nint(x2)-22),(nint(x2)+22),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-121.0) 501,500,500
500             continue
                   d=((float(image(n4,n5))-back)/sback)**2
                   s1=s1+float(image(n4,n5))*exp(-d)
                   s2=s2+(float(image(n4,n5))**2)*exp(-d)
                   s3=s3+exp(-d)
501          continue
502       continue
    
          back=s1/s3
          s4=s2/s3
          sba=3.5*(s4-back**2)
          if (sba) 504,504,503
503       continue
          sback=sqrt(sba)
504       continue
    
510       continue
 
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
 
          do 512 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
             do 511 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-81.0) 5101,5101,511
5101            continue
                   d=float(image(n4,n5))-back
                   s1=s1+float(n4)*d
                   s2=s2+float(n5)*d
                   s3=s3+d
511          continue
512       continue
 
          x1=s1/s3
          x2=s2/s3
 
          do 530 h=1,8,1
 
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
 
          do 522 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
             do 521 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-81.0) 520,520,521
520             continue
                   dexp=dist/fwhm2
                   if (dexp-40.0) 5201,5202,5202
5201                  dexp=exp(-dexp)
                      go to 5203
5202                  dexp=0.0
5203               continue
                   dstar=float(image(n4,n5))-back
                   d=dstar*dexp
c  If wanted, the next two lines will correct for dstar<0
c                     if (dstar) 5204,5205,5205
c  5204               d=0.0
5205               s1=s1+float(n4)*d
                   s2=s2+float(n5)*d
                   s3=s3+d
                   s4=s4+dstar
521          continue
522       continue
 
          x1=s1/s3
          x2=s2/s3
          seeing=((s4-s3)*fwhm2)/s3
          seeing=1.6651*sqrt(seeing)
 
530       continue
 
          ii=image(nint(x1),nint(x2))
          x1=x1-coorstars(1,ncoo2)
          xoff(1,ncoo2)=x1
          x2=x2-coorstars(2,ncoo2)
          xoff(2,ncoo2)=x2
          xoff(3,ncoo2)=back
          xoff(4,ncoo2)=seeing
          ss=seeing
          write(6,413)'(x,y) , Sky , FWHM (pix)',ncoo2,x1,x2,back,ss,ii
413          format(a25,i5,4f10.3,i8)
 
 
531       continue
 
          if (ncoo-1) 5341,5341,5311
5311      continue
 
c     The following do-loop loops ic (1st coord. in xoff)
c     from 1 to ntt=8, but xoff is only dim:(4,ncoo)
c     Changed to loop over the first two coords as they are
c     offsets in the x- and y-direction
c          do 534 ic=1,ntt,1
          do 534 ic=1,2,1
 
             s1=0.0
             s2=0.0
 
             do 532 ncoo2=1,ncoo,1

                s1=s1+xoff(ic,ncoo2)
                s2=s2+xoff(ic,ncoo2)**2

532          continue
 
             s3=s1/float(ncoo)
             s2=s2/float(ncoo)
             s4=sqrt(s2-s3**2)
 
             s1=0.0
             s2=0.0
 
             do 533 ncoo2=1,ncoo,1

                d=(xoff(ic,ncoo2)-s3)/s4
                d=exp(-(d**2))
                s1=s1+xoff(ic,ncoo2)*d
                s2=s2+d

533          continue
 
             xoff(ic,1)=s1/s2
 
534       continue          

5341      continue
 
          dx=xoff(1,1)
          dy=xoff(2,1)
          back=xoff(3,1)
          seeing=xoff(4,1)
          write(6,413)'                   mean:',ncoo,dx,dy,back,seeing
 
          idelta(1)=nint(dx-0.5)
          jdelta(1)=nint(dy-0.5)
          idelta(2)=idelta(1)+1
          jdelta(2)=jdelta(1)+1
          vi(1)=float(idelta(2))-dx
          vj(1)=float(jdelta(2))-dy
          vi(2)=1.0-vi(1)
          vj(2)=1.0-vj(1)
 
          write(6,519)idelta(1),idelta(2),vi(1),vi(2)
          write(6,519)jdelta(1),jdelta(2),vj(1),vj(2)
519          format(2i10,2f14.6)

          lname = len(name)
          n1 = lname
          do i=1,lname
            if (name(n1:n1).eq.'/') then 
		n1 = n1 + 1
		go to 100
	    endif
            n1 = n1 - 1
          enddo
  100     continue
	  stub = name(n1:)
c         write(6,755)dx,dy,back,seeing,'  ',stub
          write(16,755)dx,dy,back,seeing,'  ',stub
755          format(4f10.3,a5,a20)
 
300       continue
 
          open(66,file='fwhm.SUM',status='unknown')
 
          write(66,667)back,seeing
          write(6,667)
667          format(2f13.3)
 
          close(66)
	  close(16)
 
          end

	  include '../../common/rimage.f'
          include '../../fits/fitsinput.f'
	  include '../../common/calcsize.f'
