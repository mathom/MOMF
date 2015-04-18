       program MOMF
c
c
c      Dsum.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
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
       PARAMETER (NDIM=4096)
       integer*2 image(1:NDIM,1:NDIM),imzero(1:NDIM,1:NDIM)
       dimension ximage(1:NDIM,1:NDIM),yimage(1:NDIM,1:NDIM)
       common image, imzero, ximage, yimage
       dimension coorstars(1:2,1:10), xoff(1:4,1:10),vi(1:2),vj(1:2)
       character*80 name 
       character*60 stub, value
       character*30 line
       integer idelta(1:2), jdelta(1:2)
       integer calcsize
 
       open(23,file='Dstars',status='old')
       read(23,*)fwhm
       fwhm2=(fwhm*0.6005)**2
       ncoo=1
       write(6,*)'Reading coo.stars'
6      continue
          read(23,*,end=7)x,x,coorstars(1,ncoo),coorstars(2,ncoo)
          ncoo=ncoo+1
       go to 6
7      continue
       ncoo=ncoo-1
       close(23)
 
       open(10,file='FORMAT',status='old')
 
       read(10,*)COL
       read(10,*)ROW
       read(10,*)pfix
       do i=1,4
         read(10,*)idum
       enddo
       read(10,*)iscale
 
       close(10)
 
       mmx=nint(COL)
       mmy=nint(ROW)
 
c   SGI convention defines record length given in words = 4 bytes
       Nrecl=calcsize(mmx,mmy)

c
c Basic input from files
c
c

          open (10,file='im.SUM',access='direct',
     c          recl=Nrecl,status='old')
 
          write(6,*)'Reading image: im.SUM'
          read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
          open (11,file='factor.SUM',status='old')
          read(11,*)xfactor
          read(11,*)yfactor
          close(11)
 
          do 13 i=1,mmx,1
             do 12 j=1,mmy,1
                ximage(i,j)=float(image(i,j))*xfactor
12           continue
13        continue
 
          open (10,file='im.STAT',access='direct',
     c          recl=Nrecl,status='old')
 
          write(6,*)'Reading image: im.STAT'
          read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
          do 15 i=1,mmx,1
             do 14 j=1,mmy,1
                yimage(i,j)=float(image(i,j))*yfactor
                ximage(i,j)=yimage(i,j)*ximage(i,j)
14           continue
15        continue
 
          write(6,*)'Reading image: im.ZERO'
          open (10,file='im.ZERO',access='direct',
     c          recl=Nrecl,status='old')
 
          read(10,rec=1) ((imzero(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
          open(11,file='input.SUM',status='old')
 
          open(71,file='input.OFF',status='old')
1546      open(16,file='offset.SUM',status='unknown')
          write(6,*)'Number of offset-itterations:'
          read(5,*)ntt
          offxax=0.0
          offyax=0.0
 
16        continue
             read(16,'(a30)',end=17)line
          go to 16
17        continue
 
20        continue
         
          call getenv('MOMF_DATAIN',value)
          lin = index(value, ' ') - 1

          read(11,'(a60)',end=300)stub
	  name = value(:lin)//'/'//stub
         
2545      read(71,*)offxax,offyax
2546      write(6,*)'Image :',name,offxax,offyax
 
c
	  call rimage(name,Nrecl,image,mmx,mmy,iscale)
c         open (10,file=name,access='direct',
c    c          recl=Nrecl,status='old')
c
c         read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
c         close(10)
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
 
          do 534 ic=1,ntt,1
 
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
 
          do 273 iid=1,2,1
 
             do 272 jjd=1,2,1
 
             v=vi(iid)*vj(jjd)
 
             imin=max(1,(idelta(iid)+1))
             jmin=max(1,(jdelta(jjd)+1))
             imax=min(mmx,(mmx+idelta(iid)))
             jmax=min(mmy,(mmy+jdelta(jjd)))
             write(6,517)imin,imax,jmin,jmax
517             format(4i12)
 
             do 27 i=imin,imax,1
                do 26 j=jmin,jmax,1
                h=i-idelta(iid)
                k=j-jdelta(jjd)
                ximage(h,k)=ximage(h,k)+float(imzero(i,j)*image(i,j))*v
                yimage(h,k)=yimage(h,k)+float(imzero(i,j))*v
26              continue
27           continue
 
272          continue
 
273       continue
 
          write(16,755)dx,dy,back,seeing,'  ',name
755          format(4f10.3,a5,a60)
 
          go to 20
300       continue
 
          xmaxx=1.0
          ymaxx=1.0
 
          write(6,*)'Calculation of sum-image'
 
          do 43 i=1,mmx,1
             do 42 j=1,mmy,1
                if (yimage(i,j)) 42,42,41
41              ximage(i,j)=ximage(i,j)/yimage(i,j)
42           continue
43        continue
 
          do 47 i=1,mmx,1
             do 46 j=1,mmy,1
                if (ximage(i,j)-xmaxx) 451,451,45
45              xmaxx=ximage(i,j)
451             if (yimage(i,j)-ymaxx) 46,46,452
452             ymaxx=yimage(i,j)
46           continue
47        continue
 
          xfactor=xmaxx/15000.0
          yfactor=ymaxx/15000.0
 
          open (11,file='factor.SUM',status='old')
          write(11,*)xfactor
          write(6,*)xfactor
          write(11,*)yfactor
          write(6,*)yfactor
          close(11)
 
          do 57 i=1,mmx,1
             do 56 j=1,mmy,1
                image(i,j)=nint(ximage(i,j)/xfactor)
56           continue
57        continue
 
          open (10,file='im.SUM',access='direct',
     c          recl=Nrecl,status='unknown')
 
          write(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
c
c   Calculating FWHM in im.SUM
c 
          do 831 ncoo2=1,ncoo

          x1=coorstars(1,ncoo2)
          x2=coorstars(2,ncoo2)

          back=0.0
          sback=16384.0
    
          do 810 h=1,4,1
 
          s1=0.0
          s2=0.0
          s3=0.0
 
          do 802 n4=(nint(x1)-22),(nint(x1)+22),1
             do 801 n5=(nint(x2)-22),(nint(x2)+22),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-121.0) 801,800,800
800             continue
                   d=((float(image(n4,n5))-back)/sback)**2
                   s1=s1+float(image(n4,n5))*exp(-d)
                   s2=s2+(float(image(n4,n5))**2)*exp(-d)
                   s3=s3+exp(-d)
801          continue
802       continue
    
          back=s1/s3
          s4=s2/s3
          sba=3.0*(s4-back**2)
          if (sba) 804,804,803
803       continue
          sback=sqrt(sba)
804       continue
    
810       continue
 
          do 830 h=1,3,1
 
          s1=0.0
          s2=0.0
          s3=0.0
          s4=0.0
 
          do 822 n4=max(1,(nint(x1)-9)),min(mmx,(nint(x1)+9)),1
             do 821 n5=max(1,(nint(x2)-9)),min(mmy,(nint(x2)+9)),1
                dist=(float(n4)-x1)**2+(float(n5)-x2)**2
                if (dist-81.0) 820,820,821
820             continue
                   dexp=exp(-dist/fwhm2)
                   dstar=float(image(n4,n5))-back
                   d=dstar*dexp
                   s1=s1+float(n4)*d
                   s2=s2+float(n5)*d
                   s3=s3+d
                   s4=s4+dstar
821          continue
822       continue
 
          x1=s1/s3
          x2=s2/s3
          seeing=((s4-s3)*fwhm2)/s3
          seeing=1.6651*sqrt(seeing)
 
830       continue
 
          xoff(1,ncoo2)=back
          xoff(2,ncoo2)=seeing
          write(6,413)'(x,y) , Sky , FWHM (pix)',ncoo2,x1,x2,back,seeing
 
831       continue
 
          do 834 ic=1,2,1
 
             s1=0.0
             s2=0.0
 
             do 832 ncoo2=1,ncoo,1

                s1=s1+xoff(ic,ncoo2)
                s2=s2+xoff(ic,ncoo2)**2

832          continue
 
             s3=s1/float(ncoo)
             s2=s2/float(ncoo)
             s4=sqrt(s2-s3**2)
 
             s1=0.0
             s2=0.0
 
             do 833 ncoo2=1,ncoo,1

                d=(xoff(ic,ncoo2)-s3)/s4
                d=exp(-(d**2))
                s1=s1+xoff(ic,ncoo2)*d
                s2=s2+d

833          continue
 
             xoff(ic,1)=s1/s2
 
834       continue          
 
          open(66,file='fwhm.SUM',status='unknown')
 
          write(66,667)xoff(1,1),xoff(2,1)
          write(6,667)xoff(1,1),xoff(2,1)
667          format(2f13.3)
 
          close(66)
 
          do 67 i=1,mmx,1
             do 66 j=1,mmy,1
                image(i,j)=nint(yimage(i,j)/yfactor)
66           continue
67        continue
 
          open (10,file='im.STAT',access='direct',
     c          recl=Nrecl,status='unknown')
 
          write(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(10)
 
          end

       include '../../common/rimage.f'
       include '../../fits/fitsinput.f'
       include '../../common/calcsize.f'
