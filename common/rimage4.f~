	subroutine rimage(name,nrecl,image,mmx,mmy,scale)
c
	parameter (NDIM=4096)
	integer*2 image(1:NDIM,1:NDIM)
        integer idata,scale
	character*80 name
c
c   read raw file or FITS file according to value of scale
c   scale < 0 means FITS file
  	if (scale.ge.0) then
	  open (10,file=name,access='direct',
     c          recl=nrecl,status='old')

          read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(10)
	else 
          print 334
  334     format('ready for a FITS image')
	  call readimage(name)
	endif
        print 333
  333   format('image read')
c
c  now check the image for underflow/overflow
c
	xmin = 0.0
	xmax = 0.0
        neg = 0
	do i=1,mmx
	  do j=1,mmy
            if (image(i,j).lt.xmin) xmin = image(i,j)
            if (image(i,j).lt.-100) neg = neg + 1
            if (image(i,j).gt.xmax) xmax = image(i,j)
          enddo
	enddo
	if ((neg .gt. 1000).and.(scale.eq.0)) then
          print 200,neg
  200	  format('WARNING: negative data values i',I10,' no rescaling done')
        endif
	print 100, xmin, xmax, neg
  100	format(' min max negative ',2f10.0,i8)
c

c
c  no signed 16 bit integers converted to 15 bit positive integers
	if (iabs(scale).eq.1) then
        do i=1,mmx
	  do j=1,mmy
          image(i,j) = iand(ishft(image(i,j),-1),'7FFF'X)
          enddo
        enddo
        endif
	  
c
c  signed 16 bit signed integers converted to positive 15 bit integers
	if (iabs(scale).eq.2) then
        do i=1,mmx
          do j=1,mmy
          idata = image(i,j)
	  idata = (idata - xmin)/2
          image(i,j) = idata
          enddo
        enddo
	endif
c


c  now check the image for underflow/overflow
c
	xmin = 0.0
	xmax = 0.0
        neg = 0
	do i=1,mmx
	  do j=1,mmy
            if (image(i,j).lt.xmin) xmin = image(i,j)
            if (image(i,j).lt.-100) neg = neg + 1
            if (image(i,j).gt.xmax) xmax = image(i,j)
          enddo
	enddo
	if (neg .gt. 10) then
          print 500,neg
  500	  format('WARNING: Still ',I10,' negative values after rescaling')
        endif
	print 600, xmin, xmax, neg
  600	format(' min max negative ',2f10.0,i8)
c



	return
	end


