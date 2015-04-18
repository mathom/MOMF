       program MOMF
c
c
c      Dspot2.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
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
       parameter (NDIM=4096)
       parameter (maxoff=40)

       integer*2 image(1:NDIM,1:NDIM)
       common image
       dimension coorstars(1:2,1:10)
       character*80 name,stub,value
       character*30 line
       integer calcsize

c 
c Reading input-info: stars used to calculate offsets
c
c
 
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
 
          open(11,file='input.SUM',status='old')
          open(16,file='offset.SUM',status='unknown')
 
16        continue
             read(16,'(a30)',end=17)line
          go to 16
17        continue
 
20        continue
         
          call getenv('MOMF_DATAIN',value)
          lin = index(value, ' ') - 1

          read(11,'(a60)',end=300)stub
	  name = value(:lin)//'/'//stub
          write(6,*)'Image :',name
 
c         open (10,file=name,access='direct',
c    c          recl=Nrecl,status='old')
 
c         read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
c         close(10)
	  call rimage(name,Nrecl,image,mmx,mmy,iscale)
c
c   Calculating off-set

          s1=0.0
 
          do 502 n4=-maxoff,maxoff,1
             do 501 n5=-maxoff,maxoff,1
                
c   Internal loop

		xval=0.0

                do 531 ncoo2=1,ncoo

                   n1a=nint(coorstars(1,ncoo2))+n4
                   n2a=nint(coorstars(2,ncoo2))+n5

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
 

          write(16,755)x1,x2,back,seeing,'  ',stub
755          format(4f10.3,a5,a60)
 
          go to 20
300       continue
 
          end

       include '../../common/rimage.f'
       include '../../fits/fitsinput.f'
       include '../../common/calcsize.f'
