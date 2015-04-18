      program speed

C     measure the speed of writing and reading FITS files with FITSIO

      integer dimsiz
      parameter (dimsiz = 20000)
      integer array(dimsiz)
      integer i,xsize,ysize,binrow,ascrow
      integer status,unit
      character fname*80

C     the following parameters determine the size of the FITS file
C      xsize  =  number of pixels in each row of the image
C      ysize  =  number of rows in the I*2 image
C      binrow =  number of rows in the binary table
C      ascrow =  number of rows in the ASCII table
C      dimsiz =  number of image pixels to write or read at one time

      xsize =     3000
      ysize =     3000
      binrow = 1250000
      ascrow =  100000

      status=0

C     initialize the values in the array
      do i=1,dimsiz
          array(i)=i
      end do

C     Name of the FITS file to be created:
      fname='speedff.fit'
      print *,'Create large FITS file, speedff.fits, to test FITSIO'
      print *,'                                                    ',
     &  '  SIZE / ELAPSE= RATE'
      print *,'                                                    ',
     &  '  (Mb)   (sec)  (Mb/s)'

C     Delete the file if it already exists, so we can then recreate it
      call dfile(fname,status)

C     Get an unused Logical Unit Number to use to open the FITS file
      call ftgiou(unit,status)

C     create the new empty file to test raw write and read speed
      call ftinit(unit,fname,1,status)

C     write than read a file using raw Fortran writes and reads
      call wraw(unit, array, xsize, ysize, status)
      call rraw(unit, array, xsize, ysize, status)

C     delete the raw file
      call ftdelt(unit,status)

C     create the new empty file FITS file
      call ftinit(unit,fname,1,status)

      call wimage(unit, array, xsize, ysize, dimsiz, status)
      call wbin(unit, array, binrow, dimsiz, status)
      call wascii(unit, array, ascrow, dimsiz, status)

      call rimage(unit, array, xsize, ysize, dimsiz,status)
      call rbin(unit, array, binrow, dimsiz, status)
      call rascii(unit, array, ascrow, dimsiz, status)

      end
C************************************************************
      subroutine wraw(unit, array, xsize, ysize, status)

C     test the speed of raw Fortran writes

      integer xsize, ysize, nloop, j
      integer status,unit,bitpix,naxis,naxes(2)
      integer array(720)
      real elapse,size,rate

C     write a dummy header, to prevent errors later when deleting the file
      bitpix=16
      naxis=0
      call ftphpr(unit,.true.,bitpix,naxis,naxes,0,1,.true.,status)

C     flush the header to disk
      call ftflus(unit,status)

C     number of loops required to write the same amount of data as
C     in the FITS image, writing 2880 bytes at a time 
      nloop = (xsize * ysize) / 720

C     'mark' the starting time
      call mktime

C     write the array to the FITS file
      do j=2,nloop+1
         write(unit,rec=j,err=900)array
      end do

      go to 950
900   status = 106
950   continue

C     get the elapsed time, in seconds
      call gttime(elapse)

C     size of file, in MB
      size = nloop * 2880. / 1000000.
      rate = size / elapse
      write(*,1000)size,elapse,rate
1000  format(' Raw Fortran Writes (2880 bytes/record)              ',
     & '  ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine rraw(unit, array, xsize, ysize, status)

C     test the speed of raw Fortran reads

      integer xsize, ysize, nloop, j
      integer status,unit
      integer array(720)
      real elapse,size,rate

C     number of loops required to read the same amount of data as
C     in the FITS image, reading 2880 bytes at a time 
      nloop = (xsize * ysize) / 720

C     'mark' the starting time
      call mktime

C     read the array from the FITS file
      do j=2,nloop+1
         read(unit,rec=j,iostat=status)array
      end do

      if (status .ne. 0)status=108

C     get the elapsed time, in seconds
      call gttime(elapse)

C     size of file, in MB
      size = nloop * 2880. / 1000000.
      rate = size / elapse
      write(*,1000)size,elapse,rate
1000  format(' Raw Fortran Reads                                   ',
     & '  ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine wimage(unit, array, xsize, ysize, dimsiz, status)

C     write a FITS primary array containing a 2-D image

      integer xsize, ysize, dimsiz
      integer status,unit,bitpix,naxis,naxes(2)
      integer j,nsize
      integer array(xsize)
      real elapse,size,rate

C     initialize parameters about the FITS image 
      bitpix=32
      naxis=2
      naxes(1)=xsize
      naxes(2)=ysize

C     write the required header keywords
      call ftphpr(unit,.true.,bitpix,naxis,naxes,0,1,.true.,status)

      nsize = xsize * ysize

C     'mark' the starting time
      call mktime

C     write the array to the FITS file
      do j=1,nsize,dimsiz
         call ftpprj(unit,1,j,dimsiz,array,status)
      end do

C     flush the file to disk
      call ftflus(unit,status)

C     get the elapsed time, in seconds
      call gttime(elapse)

C     size of file, in MB
      size = xsize * ysize * 4. / 1000000.
      rate = size / elapse
      write(*,1000)xsize,ysize,dimsiz,size,elapse,rate
1000  format(' Write ',i5,' x ',i4,' row I*4 image (',i6,
     & ' pixels/loop) ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine wbin(unit, array, nrows, dimsiz ,status)

C     write a binary table

      integer status,unit,nrows,dimsiz,width,ngroup,remain,frow,ntodo
      integer array(dimsiz)
      real elapse,size,rate

      character extnam*16
      character*16 ttype(2),tform(2),tunit(2)
      data ttype/'int1','int2'/
      data tform/'1J','1J'/
      data tunit/' ',' '/

      width=8
      extnam='speed'
C     write the required header parameters for the binary table
      call ftibin(unit,nrows,2,ttype,tform,tunit,
     &            extnam,0,status)

      call ftgrsz(unit,ngroup,status)
      ngroup = min(ngroup,dimsiz)
      remain = nrows
      frow=1

C     'mark' the starting time
      call mktime

10    ntodo = min(ngroup,remain)
      call ftpclj(unit,1,frow,1,ntodo,array,status)
      call ftpclj(unit,2,frow,1,ntodo,array,status)
      frow = frow + ntodo
      remain=remain-ntodo
      if (remain .gt. 0)go to 10

C     flush the file to disk
      call ftflus(unit,status)

C     get the elapsed time, in seconds
      call gttime(elapse)

C     size of table in MB
      size = nrows * width / 1000000.
      rate = size / elapse 
      write(*,1000)nrows,ngroup,size,elapse,rate
1000  format(' Write ',i7,' row x 2 col BINTABLE  (',i5,
     & ' rows/loop) ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine wascii(unit, array, nrows, dimsiz, status)

C     write an ASCII table

      integer status,unit,nrows,dimsiz,tbcol(2),width,ngroup
      integer remain,ntodo,frow
      integer array(dimsiz)
      real elapse,size,rate

      character extnam*16
      character*16 ttype(2),tform(2),tunit(2)
      data ttype/'int1','int2'/
      data tform/'I6','I6'/
      data tunit/' ',' '/
      data tbcol/1, 8/

      width = 13
      extnam='aspeed'
C     write the required header parameters for the binary table
      call ftitab(unit,13,nrows,2,ttype,tbcol,tform,tunit,
     &            extnam,status)

      call ftgrsz(unit,ngroup,status)
      ngroup = min(ngroup,dimsiz)
      remain=nrows
      frow=1

C     'mark' the starting time
      call mktime

10    ntodo = min(ngroup,remain)
      call ftpclj(unit,1,frow,1,ntodo,array,status)
      call ftpclj(unit,2,frow,1,ntodo,array,status)
      frow = frow + ntodo
      remain=remain-ntodo
      if (remain .gt. 0)go to 10

C     flush the file to disk
      call ftflus(unit,status)

C     get the elapsed time, in seconds
      call gttime(elapse)

C     size of table in MB
      size = nrows * width / 1000000.
      rate = size / elapse
      write(*,1000)nrows,ngroup,size,elapse,rate
1000  format(' Write ',i7,' row x 2 col ASCII tab (',i5,
     & ' rows/loop) ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine rimage(unit, array, xsize, ysize, dimsiz, status)

C     read a FITS primary array containing a 2-D image

      integer status,unit, xsize, ysize,dimsiz,hdutype,j,nsize
      integer array(xsize), nulval
      real elapse,size,rate
      logical anynul

C     move to the primary array
      call ftmahd(unit,1,hdutype,status)

      nulval = 0
      nsize = xsize * ysize

C     'mark' the starting time
      call mktime

C     read the array from the FITS file
      do j=1,nsize,dimsiz
         call ftgpvj(unit,1,j,dimsiz,nulval,
     &              array,anynul,status)
      end do

C     get the elapsed time, in seconds
      call gttime(elapse)

      size = xsize * ysize * 4. / 1000000.
      rate = size / elapse

      write(*,1000)dimsiz,size,elapse,rate
1000  format(' Read back I*4 image  (',i6,' pixels/loop)      ',
     & '       ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine rbin(unit, array, nrows, dimsiz, status)

C     read a binary table

      integer status,unit,nrows,dimsiz,width,hdutyp,ngroup
      integer remain,ntodo,frow
      integer array(dimsiz), nulval
      real elapse,size,rate
      logical anynul

C     move to the next extension
      call ftmrhd(unit,1,hdutyp,status)

      call ftgrsz(unit,ngroup,status)
      ngroup = min(ngroup,dimsiz)
      remain=nrows
      frow=1

      nulval = 0
      width=8

C     'mark' the starting time
      call mktime

10    ntodo = min(ngroup,remain)
      call ftgcvj(unit,1,frow,1,ntodo,nulval,array,
     &                    anynul,status) 
      call ftgcvj(unit,2,frow,1,ntodo,nulval,array,
     &                    anynul,status) 
      frow = frow + ntodo
      remain=remain-ntodo
      if (remain .gt. 0)go to 10

C     get the elapsed time, in seconds
      call gttime(elapse)

      size = nrows * width / 1000000.
      rate = size / elapse
      write(*,1000)ngroup,size,elapse,rate
1000  format(' Read back Binary table  (',i5,' rows/loop)       ',
     & '      ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine rascii(unit, array, nrows, dimsiz, status)

C     read an ASCII table

      integer status,unit,nrows,dimsiz,width,hdutyp,ngroup
      integer remain,ntodo,frow
      integer array(dimsiz), nulval
      real elapse,size,rate
      logical anynul

C     move to the next extension
      call ftmrhd(unit,1,hdutyp,status)

      call ftgrsz(unit,ngroup,status)
      ngroup = min(ngroup,dimsiz)
      remain=nrows
      frow=1

      nulval = 0
      width=13

C     'mark' the starting time
      call mktime

10    ntodo = min(ngroup,remain)
      call ftgcvj(unit,1,frow,1,ntodo,nulval,array,
     &                    anynul,status) 
      call ftgcvj(unit,2,frow,1,ntodo,nulval,array,
     &                    anynul,status) 
      frow = frow + ntodo
      remain=remain-ntodo
      if (remain .gt. 0)go to 10

C     get the elapsed time, in seconds
      call gttime(elapse)

      size = nrows * width / 1000000.
      rate = size / elapse
      write(*,1000)ngroup,size,elapse,rate
1000  format(' Read back ASCII table   (',i5,' rows/loop)       ',
     & '      ',f5.2,1x,f6.2,1x,f6.2)

C     check for any error, and if so print out error messages
      if (status .gt. 0)call perror(status)
      end
C************************************************************
      subroutine perror(status)

C     Print out the FITSIO error messages to the user

      integer status
      character errtext*30,errmessage*80

C     check if status is OK (no error); if so, simply return
      if (status .le. 0)return

C     get the text string which describes the error
      call ftgerr(status,errtext)
      print *,'FITSIO Error Status =',status,': ',errtext

C     read and print out all the error messages on the FITSIO stack
      call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          print *,errmessage
          call ftgmsg(errmessage)
      end do
      end
C************************************************************
      subroutine dfile(filename,status)

C     A simple little routine to delete a FITS file

      integer status,unit,block
      character*(*) filename

C     simply return if status is greater than zero
      if (status .gt. 0)return

C     Get an unused Logical Unit Number to use to open the FITS file
      call ftgiou(unit,status)

C     try to open the file, to see if it exists
      call ftopen(unit,filename,1,block,status)

      if (status .eq. 0)then
C         file was opened;  so now delete it 
          call ftdelt(unit,status)
      else if (status .eq. 103)then
C         file doesn't exist, so just reset status to zero and clear errors
          status=0
          call ftcmsg
      else
C         there was some other error opening the file; delete the file anyway
          status=0
          call ftcmsg
          call ftdelt(unit,status)
      end if

C     free the unit number for later reuse
      call ftfiou(unit, status)
      end
C************************************************************
      subroutine mktime

C     'mark' the start time, i.e., start the timer

      integer hh,mm,ss,dd
      common/times/hh,mm,ss,dd

      integer iarray(8),stemp

C     start recording the elapsed time on a seconds tick.
C     This  provides more consistent measurements if
C     the times are only accurate to the nearest second

      call date_and_time(values=iarray)
      stemp=iarray(7)
10    call date_and_time(values=iarray)
C     keep getting the time, until the seconds change
      if (iarray(7) .eq. stemp)go to 10

      hh=iarray(5)
      mm=iarray(6)
      ss=iarray(7)
      dd=iarray(8)
      end
C************************************************************
      subroutine gttime(elapse)

C     get the time that has elapsed since mktime was last called

      real elapse,frac
      integer hh,mm,ss,dd
      common/times/hh,mm,ss,dd

      integer iarray(8),jarray(8),itemp,nloop1,nloop2

      call date_and_time(values=iarray)

      if (dd .eq. 0 .and. iarray(8) .eq. 0) then
C         clock is not reporting the milliseconds, so use an
C         alternate method to estimate the factional part of the seconds

         itemp=iarray(7)
         nloop1=0
10       call date_and_time(values=jarray)
         nloop1=nloop1+1
C        keep getting the time, until the seconds change
         if (jarray(7) .eq. itemp)go to 10

C        now count the number of loops in 1 second
         itemp=jarray(7)
         nloop2=0
20       call date_and_time(values=jarray)
         nloop2=nloop2+1
C        keep getting the time, until the seconds change
         if (jarray(7) .eq. itemp)go to 20

         frac = nloop1 * 1.0 / nloop2
         frac = min(frac,1.0)
      else
         frac = (iarray(8) - dd)/1000.
      end if

      elapse = (iarray(5)-hh)*3600. + (iarray(6)-mm)*60. +
     &    (iarray(7) - ss) + frac

      end

