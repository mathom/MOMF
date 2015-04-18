      subroutine readheader(filename,nx,ny)
      integer nx,ny

C     Print out all the header keywords in all extensions of a FITS file

      integer status,unit,readwrite,blocksize
      integer naxes(2)
      character filename*80,record*80

 1    status=0

C     Get an unused Logical Unit Number to use to open the FITS file
 2    call ftgiou(unit,status)

C     open the FITS file, with read-only access
      readwrite=0
 3    call ftopen(unit,filename,readwrite,blocksize,status)

100   continue

C   get the size of the file
 4    call ftgknj(unit,'NAXIS',1,2,naxes,nfound,status)

C     check that it found both NAXIS1 and NAXIS2 keywords
 5    if (nfound .ne. 2)then
          print *,'READIMAGE failed to read the NAXISn keywords.'
          return
       end if

C  return the size of the image
      nx = naxes(1)
      ny = naxes(2)

C     close the file, free the unit number, and exit
 9    call ftclos(unit, status)
      call ftfiou(unit, status)

C     check for any error, and if so print out error messages
 10   if (status .gt. 0)call printerror(status)
      end



      subroutine readimage(filename)

C     Read a FITS image and determine the minimum and maximum pixel value
      parameter (NDIM=4096)
      character filename*80
      integer*2 image(1:NDIM,1:NDIM), image2(-10:10,-10:10)
      common image,image2

      integer status,unit,readwrite,blocksize,naxes(2),nfound
      integer group,firstpix,nbuffer,npixels,i,np
      real datamin,datamax,nullval,buffer(NDIM)
      logical anynull

 1    status=0

C     Get an unused Logical Unit Number to use to open the FITS file
 2    call ftgiou(unit,status)

C     Open the file
      readwrite=0
 3    call ftopen(unit,filename,readwrite,blocksize,status)

C     determine the size of the image
 4    call ftgknj(unit,'NAXIS',1,2,naxes,nfound,status)

C     check that it found both NAXIS1 and NAXIS2 keywords
 5    if (nfound .ne. 2)then
          print *,'READIMAGE failed to read the NAXISn keywords.'
          return
       end if

C     initialize variables
      npixels=naxes(1)*naxes(2)
      nx = naxes(1)
      group=1
      np=1
      firstpix=1
      nullval=-999
      datamin=1.0E30
      datamax=-1.0E30

      do while (npixels .gt. 0)
C         read up to nx pixels at a time 
          nbuffer=min(nx,npixels)
      
 6        call ftgpve(unit,group,firstpix,nbuffer,nullval,
     &            buffer,anynull,status)

C         find the min and max values
          do i=1,nbuffer
              datamin=min(datamin,buffer(i))
              datamax=max(datamax,buffer(i))
          end do
	  do i=1,nbuffer
	      image(i,np) = ifix(buffer(i))
          enddo

C         increment pointers and loop back to read the next group of pixels
          npixels=npixels-nbuffer
          firstpix=firstpix+nbuffer
          np = np + 1
      end do

C     print out the min and max values
      print *,'Min and max values in the image are:',datamin,datamax

C     close the file and free the unit number
 7    call ftclos(unit, status)
      call ftfiou(unit, status)

C     check for any error, and if so print out error messages
 8    if (status .gt. 0)call printerror(status)
      end


      subroutine printerror(status)

C     Print out the FITSIO error messages to the user

      integer status
      character errtext*30,errmessage*80

C     check if status is OK (no error); if so, simply return
      if (status .le. 0)return

C     get the text string which describes the error
 1    call ftgerr(status,errtext)
      print *,'FITSIO Error Status =',status,': ',errtext

C     read and print out all the error messages on the FITSIO stack
 2    call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          print *,errmessage
          call ftgmsg(errmessage)
      end do
      end
