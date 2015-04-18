C------------------------------------------------------------------------------
C       The following routines are part of the FITSIO library
C       and are specific to Macintosh computers using the
C       Absoft MacFortran II compiler.

C       With Absoft's MacFortran II the fitsio.f file must be split
C       using fsplit before compiling and the options used were -s for static
C       allocation (possibly not needed). The compile line for this file is
C       f77compiler -q -s -N8 -t3000 fitsmac.f The -t3000 allows use of
C       the long character buffers.
C------------------------------------------------------------------------------
C   This software was prepared by High Energy Astrophysic Science Archive
C   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
C   shall not, without prior written permission of the U.S. Government,
C   establish a claim to statutory copyright.  The Government and others acting
C   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
C   worldwide license for Government purposes to publish, distribute,
C   translate, copy, exhibit, and perform such material. 
C--------------------------------------------------------------------------
        subroutine ftopnf(funit,fname,oldnew,rwmode,block,fsize,status)

C       low-level, machine-dependent routine to create or open a new file 
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       oldnew  i  file status: 0 = open old/existing file; else open new file
C       rwmode  i  file access mode: 0 = readonly; else = read/write
C       block   i  FITS record blocking factor 
C       fsize    i  min size of file, in bytes
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, December 1996

        integer funit,oldnew,rwmode,block,fsize,status
        character*(*) fname
        character*3 fstat

        if (oldnew .eq. 0)then
                fstat='OLD'
C               The blocking factor is irrelevant for files on a this machine, 
C               therefore, simply return the default block size.
                block=1
        else
C               create a new file
                fstat='NEW'
        end if

C       Note that record size is given in units of bytes, not words.
        open(unit=funit,file=fname,status=fstat,err=900,
     &       recl=2880,form='UNFORMATTED',access='DIRECT')

C       determine the number of existing records in the file
        inquire(funit, size=fsize)

        return

C       error opening file:
900     status=104 + oldnew
        end
C----------------------------------------------------------------------
        subroutine ftgsdt(dd,mm,yy,status)

C       get the current date from the system
C       this is the Absoft MacFortran II routine to get the system date:

C       dd      i  day of the month (1-31)
C       mm      i  month of the year (1-12)
C       yy      i  last 2 digits of the year (1992 = 92, 2001 = 01)

        integer dd,mm,yy,status

        INTEGER*4 GetTime
        PASCAL EXTERNAL GetTime

        STRUCTURE /DateTimeRec/
          INTEGER*2 year
          INTEGER*2 month
          INTEGER*2 day
          INTEGER*2 hour
          INTEGER*2 minute
          INTEGER*2 second
          INTEGER*2 dayOfWeek
        END STRUCTURE

        RECORD /DateTimeRec/ DateTime

        if (status .gt. 0)return

        CALL GetTime(DateTime)

        mm = DateTime.month
        dd = DateTime.day
        yy = mod(DateTime.year,100)
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

C       simply call character writing routine:
        call ftpcbf(ounit,nbytes,array,status)
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

C       simply call character writing routine:
        call ftpcbo(ounit,gsize,ngroup,offset,array,status)
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

C       simply call character reading routine:
        call ftgcbf(iunit,nbytes,array,status)
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

C       simply call character reading routine:
        call ftgcbo(iunit,gsize,ngroup,offset,array,status)
        end
C------------------------------------------------------------------------
        subroutine ieevud(dbl1, dbl2, int)
        double precision dbl1,dbl2
        integer int
C       dummy routine; only used on Vax VMS computers
        end
C------------------------------------------------------------------------
        subroutine ieevpd(dbl1, dbl2, int)
        double precision dbl1,dbl2
        integer int
C       dummy routine; only used on Vax VMS computers
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

