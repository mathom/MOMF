C------------------------------------------------------------------------------
C       The following routines are part of the FITSIO library
C       and are specific to DEC Alpha OpenVMS computers
C------------------------------------------------------------------------------
C   This software was prepared by High Energy Astrophysic Science Archive
C   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
C   shall not, without prior written permission of the U.S. Government,
C   establish a claim to statutory copyright.  The Government and others acting
C   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
C   worldwide license for Government purposes to publish, distribute,
C   translate, copy, exhibit, and perform such material. 
C--------------------------------------------------------------------------
        subroutine ftopnf(funit,fname,oldnew,rwmode,block,size,status)

C       low-level, machine-dependent routine to create or open a new file 
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       oldnew  i  file status: 0 = open old/existing file; else open new file
C       rwmode  i  file access mode: 0 = readonly; else = read/write
C       block   i  FITS record blocking factor 
C       size    i  min size of file, in bytes
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, December 1996

        integer funit,oldnew,rwmode,block,size,status
        integer recln
        logical opn
        character*(*) fname
        character*3 fstat

        if (oldnew .eq. 0)then
                fstat='OLD'
C               get record length of existing file
                inquire(file=fname,recl=recln,opened=opn)
C               calculate bytes per record (if file is opened)
                if (opn)then
                        recln=recln*4
                end if

C               compute blocking factor
                block=recln/2880
                if (block*2880 .ne. recln)then
C                       record length is not an integer multiple of 2880
C                       so return actual record length in block
                        block=recln
                end if

C               no easy way to determine the size of the file, 
C               so just set value to a huge number
                size=1000000000
        else
C               create a new file
                fstat='NEW'
                recln=2880
                size = 0
        end if

C       convert record length from bytes to words:
        recln = recln / 4

C       11/95: adding the blocksize parameter makes writing of large
C       blocks of data go much faster (factor of 3 in some cases).

        if (rwmode .eq. 0)then
                open(unit=funit,file=fname,status=fstat,err=900,
     &          blocksize=10000,
     &          recl=recln,form='UNFORMATTED',access='DIRECT',readonly)
        else
                open(unit=funit,file=fname,status=fstat,err=900,
     &          blocksize=10000,
     &          recl=recln,form='UNFORMATTED',access='DIRECT')
        end if
        return

C       error opening file:
900     status=104 + oldnew
        end
C----------------------------------------------------------------------
        subroutine ftgsdt(dd,mm,yy,status)

C       get the current date from the system

C       dd      i  day of the month (1-31)
C       mm      i  month of the year (1-12)
C       yy      i  last 2 digits of the year (1992 = 92, 2001 = 01)

        integer dd,mm,yy,status

        if (status .gt. 0)return

        call idate(mm,dd,yy)
        yy=yy-(yy/100)*100
        end
C----------------------------------------------------------------------
        subroutine ftpbyt(ounit,nbytes,array,status)

C       write string of data bytes to output buffer.

C       ounit   i  fortran unit number
C       nbytes  i  number of bytes
C       array   i  integer array
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer array(*)
        integer nbytes,ounit,status

C       pass numerical array by descriptor to character array:
        call ftpcbf(ounit,nbytes,%descr(array(1)),status)
        end
C--------------------------------------------------------------------------
        subroutine ftpbyo(ounit,gsize,ngroup,offset,array,status)
 
C       "Put Bytes with Offsets"
C       copy input buffer of bytes to the output character buffer.
 
C       ounit   i  Fortran output unit number
C       gsize   i  size of each group of bytes
C       ngroup  i  number of groups to write
C       offset  i  size of gap between groups
C       array   i  input array of bytes
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1996
 
        integer ounit,gsize,ngroup,offset,status
        integer array(*)

C       pass numerical array by descriptor to character array:
        call ftpcbo(ounit,gsize,ngroup,offset,%descr(array(1)),status)
        end
C----------------------------------------------------------------------
        subroutine ftgbyt(iunit,nbytes,array,status)

C       read string of data bytes from input buffer.

C       iunit   i  fortran unit number
C       nbytes  i  number of bytes
C       array   i  integer array
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer nbytes,iunit,status
        integer array(*)

C       pass numerical array by descriptor to character array:
        call ftgcbf(iunit,nbytes,%descr(array(1)),status)
        end
C--------------------------------------------------------------------------
        subroutine ftgbyo(iunit,gsize,ngroup,offset,array,status)

C       "Get BYtes with Offsets"
C       read bytes from the character buffer.

C       iunit   i  Fortran output unit number
C       gsize   i  size of each group of bytes
C       ngroup  i  number of groups to read
C       offset  i  size of gap between groups
C       array   i  output array of bytes
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1996

        integer iunit,gsize,ngroup,offset,status
        integer array(*)

C       pass numerical array by descriptor to character array:
        call ftgcbo(iunit,gsize,ngroup,offset,%descr(array(1)),status)
        end
C------------------------------------------------------------------------
        integer function cray2ieg(i1,i2,i3,i4,i5,i6,s1)
        integer i1,i2,i3,i4,i5,i6
        character s1
C       dummy routine; only used on Cray supercomputers
        cray2ieg=0
        end
C------------------------------------------------------------------------
        integer function ieg2cray(i1,i2,i3,i4,i5,i6,s1)
        integer i1,i2,i3,i4,i5,i6
        character s1
C       dummy routine; only used on Cray supercomputers
        ieg2cray=0
        end

