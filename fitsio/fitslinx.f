C------------------------------------------------------------------------------
C       The following routines are part of the FITSIO library
C       and are specific to Linux on a PC and other similar computers 
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
        character*(*) fname
        character*3 fstat

        if (oldnew .eq. 0)then
                fstat='OLD'
C               The blocking factor is irrelevant for files on a this machine, 
C               therefore, simply return the default block size.
                block=1
C               no easy way to determine the size of the file, 
C               so just set value to a huge number
                size=1000000000
        else
C               create a new file
                fstat='NEW'
                size = 0
        end if

C       Note that record size is given in units of bytes, not words.
        open(unit=funit,file=fname,status=fstat,err=900,
     &       recl=2880,form='UNFORMATTED',access='DIRECT')
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

        integer dd,mm,yy,status,unit

        if (status .gt. 0)return

        dd=0
        mm=0
        yy=0

C   If you are using FITSIO in the context of the FTOOLS package
C   the following uses a routine defined in xanlib/sysdep/sysclnx.c
C       call gtsdate(yy, mm, dd)

C   The following system calls can also be used to get the 
C   current date. This approach is inefficient, but in practice
C   this routine is only called at most once per output FITS file.

        call system ("date +%y%m%d > /tmp/idate.data")
        call ftgiou (unit, status)
        open (file='/tmp/idate.data', unit=unit)
        read (unit, '(3I2)') yy, mm, dd
        close (unit)
        call ftfiou (unit, status)
        call system ("rm /tmp/idate.data")

C   Alternatively, the following C routine may be used.  
C   Delete the first character from each line (the 'C'). 
C   The Makefile will then need to be modified accordingly, 
C   to compile and add this 'gtsdate' routine to the final FITSIO library.
C
C #include <time.h>
C /* Returns the current date in YR - year (last two digits), 
C    MON - month (1 - 12), DAY - day (1 - 31) */
C  
C void gtsdate_(int *yr, int *mon, int *day )
C {
C    struct tm* tm;    /* date/time structure */
C    time_t clock;     /* current time in seconds  */
C  
C    clock = time(0);
C    tm = localtime(&clock);
C   *yr = (int) ((tm->tm_year < 100) ? tm->tm_year : tm->tm_year-100);
C   *mon = (int) tm->tm_mon+1;
C   *day = (int) tm->tm_mday;
C }
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

