# These routines are part of the FITSIO library and are designed to run in
# the IRAF/SPP environment.
#------------------------------------------------------------------------------
#   This software was prepared by High Energy Astrophysic Science Archive
#   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
#   shall not, without prior written permission of the U.S. Government,
#   establish a claim to statutory copyright.  The Government and others acting
#   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
#   worldwide license for Government purposes to publish, distribute,
#   translate, copy, exhibit, and perform such material.
#------------------------------------------------------------------------------

include "fitsiospp.h"

procedure fsvers(vernum)

# Returns the current revision number of the FITSIO package.
# The revision number will be incremented whenever any modifications,
# bug fixes, or enhancements are made to the package

real    vernum          # o FITSIO version number

begin

call ftvers(vernum)
end
#-------------------------------------------------------------------
procedure fsarch(machid)

int     machid          # machine ID code

begin
call ftarch(machid)
end
#-------------------------------------------------------------------
procedure fsgsdt(dd,mm,yy,status)

# get the current date

int	dd		#O day of the month (1-31)
int	mm		#O month of the year (1-12)
int	yy		#O last 2 digits of the year (1992 = 92, 2001 = 01)
int     status          # o error status

begin
call ftgsdt (dd, mm, yy, status)
end
#-------------------------------------------------------------------
procedure fsgiou(iounit,status)

# Returns an unused I/O unit number which may then be used as input
# to the fsinit or fsopen procedures.

int     iounit          # o unused I/O unit number
int     status          # o error status

begin
call ftgiou(iounit,status)
end
#-------------------------------------------------------------------
procedure fsfiou(iounit,status)

# Returns an unused I/O unit number which may then be used as input
# to the fsinit or fsopen procedures.

int     iounit          # i I/O unit number
int     status          # o error status

begin
call ftfiou(iounit,status)
end
#-------------------------------------------------------------------
procedure fsgerr(errnum,text)

# Return a descriptive error message corresponding to the error number

int     errnum          # i error number
char    text[SZ_FERRTXT]       # i text string
%       character ftext*30

begin

call ftgerr(errnum,ftext)

call f77upk(ftext  ,text ,SZ_FERRTXT)
end
#-------------------------------------------------------------------
procedure fsgmsg(text)

# Return oldest error message from the FITSIO error stack

char    text[SZ_FCARD]       # o text string
%       character ftext*80

begin

call ftgmsg(ftext)

call f77upk(ftext  ,text ,SZ_FCARD)
end
#-------------------------------------------------------------------
procedure fspmsg(text)

# write a 80 character record to the FITSIO error stack

char    text[SZ_FCARD]     # i 80-char message
%       character ftext*80

begin

call f77pak(text,ftext,SZ_FCARD)

call ftpmsg(ftext)
end
#-------------------------------------------------------------------
procedure fscmsg

# clear the FITSIO error stack

begin


call ftcmsg
end
#-------------------------------------------------------------------
procedure fspkys(ounit,keywrd,strval,comm,status)

# write a character string value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkys(ounit,fkeywr,fstrva,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkls(ounit,keywrd,strval,comm,status)

# write a character string value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkls(ounit,fkeywr,fstrva,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsplsw(iunit,status)

# write keywords to warn users that longstring convention may be used

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftplsw(iunit,status)
end
#-------------------------------------------------------------------
procedure fspkyl(ounit,keywrd,logval,comm,status)

# write a logical value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyl(ounit,fkeywr,logval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkyj(ounit,keywrd,intval,comm,status)

# write an integer value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyj(ounit,fkeywr,intval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkyf(ounit,keywrd,rval,decim,comm,status)

# write a real*4 value to a header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyf(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkye(ounit,keywrd,rval,decim,comm,status)

# write a real*4 value to a header record in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkye(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkyg(ounit,keywrd,dval,decim,comm,status)

# write a double precision value to a header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyg(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkyd(ounit,keywrd,dval,decim,comm,status)

# write a double precision value to a header record in E format
# If it will fit, the value field will be 20 characters wide;
# otherwise it will be expanded to up to 35 characters, left
# justified.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyd(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkyt(iunit,keywrd,intval,dval,comm,status)

# concatinate a integer value with a double precision fraction
# and write it to the FITS header along with the comment string
# The value will be displayed in F28.16 format

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
double  dval            # i real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyt(iunit,fkeywr,intval,dval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkns(ounit,keywrd,nstart,nkey,strval,comm,status)

# write an array of character string values to header records

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
char    strval[SZ_FSTRVAL,ARB]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status
int	i
int	n1

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)
# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

n1=nstart
do i=1,nkey   {
 call f77pak(strval(1,i),fstrva,SZ_FSTRVAL)
 call ftpkys(ounit,fkeywr,n1,1,fstrva,fcomm,status)
 n1=n1+1
  }

end
#-------------------------------------------------------------------
procedure fspknl(ounit,keywrd,nstart,nkey,logval,comm,status)

# write an array of logical values to header records

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
bool    logval[ARB]     # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknl(ounit,fkeywr,nstart,nkey,logval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspknj(ounit,keywrd,nstart,nkey,intval,comm,status)

# write an array of integer values to header records

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknj(ounit,fkeywr,nstart,nkey,intval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspknf(ounit,keywrd,nstart,nkey,rval,decim,comm,status)

# write an array of real*4 values to header records in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknf(ounit,fkeywr,nstart,nkey,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkne(ounit,keywrd,nstart,nkey,rval,decim,comm,status)

# write an array of real*4 values to header records in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpkne(ounit,fkeywr,nstart,nkey,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspkng(ounit,keywrd,nstart,nkey,dval,decim,comm,status)

# write an array of real*8 values to header records in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpkng(ounit,fkeywr,nstart,nkey,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fspknd(ounit,keywrd,nstart,nkey,dval,decim,comm,status)

# write an array of real*8 values to header records in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

call ftpknd(ounit,fkeywr,nstart,nkey,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsirec(ounit,keyno,record,status)

# insert a character string card record to a header

int     ounit           # i output file pointer
int     keyno           # i number of the keyword to insert before
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftirec(ounit,keyno,frecor,status)
end
#-------------------------------------------------------------------
procedure fsikys(ounit,keywrd,strval,comm,status)

# insert a character string value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikys(ounit,fkeywr,fstrva,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikyl(ounit,keywrd,logval,comm,status)

# insert a logical value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikyl(ounit,fkeywr,logval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikyj(ounit,keywrd,intval,comm,status)

# insert an integer value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikyj(ounit,fkeywr,intval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikyf(ounit,keywrd,rval,decim,comm,status)

# insert a real*4 value to a header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikyf(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikye(ounit,keywrd,rval,decim,comm,status)

# insert a real*4 value to a header record in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikye(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikyg(ounit,keywrd,dval,decim,comm,status)

# insert a double precision value to a header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikyg(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsikyd(ounit,keywrd,dval,decim,comm,status)

# insert a double precision value to a header record in E format
# If it will fit, the value field will be 20 characters wide;
# otherwise it will be expanded to up to 35 characters, left
# justified.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftikyd(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmrec(ounit,nkey,record,status)

# modify the nth keyword in the CHU, by replacing it with the
# input 80 character string.

int     ounit           # i output file pointer
int     nkey            # i number of keyword to be modified
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftmrec(ounit,nkey,frecor,status)
end
#-------------------------------------------------------------------
procedure fsmcrd(ounit,keywrd,card,status)

# modify (overwrite) a given header record specified by keyword name.
# This can be used to overwrite the name of the keyword as well as
# the value and comment fields.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # i 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(card  ,fcard, SZ_FCARD)

call ftmcrd(ounit,fkeywr,fcard,status)
end
#-------------------------------------------------------------------
procedure fsucrd(ounit,keywrd,card,status)

# update a given header record specified by keyword name.
# new record is appended to header if it doesn't exist.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # i 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(card  ,fcard, SZ_FCARD)

call ftucrd(ounit,fkeywr,fcard,status)
end
#-------------------------------------------------------------------
procedure fsmcom(ounit,keywrd,comm,status)

# modify the comment string in a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    comm[SZ_FLONGCOMM]       # i keyword comment
%       character fcomm*72
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FLONGCOMM)

call ftmcom(ounit,fkeywr,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmnam(ounit,oldkey,newkey,status)

# modify the name of a header keyword

int     ounit           # i output file pointer
char    oldkey[SZ_FKEYWORD]     # i keyword name
%       character fokey*8
char    newkey[SZ_FKEYWORD]     # i keyword name
%       character fnkey*8
int     status          # o error status

begin

call f77pak(oldkey,fokey,SZ_FKEYWORD)
call f77pak(newkey,fnkey,SZ_FKEYWORD)

call ftmnam(ounit,fokey,fnkey,status)
end
#-------------------------------------------------------------------
procedure fsmkys(ounit,keywrd,strval,comm,status)

# modify a character string value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkys(ounit,fkeywr,fstrva,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkyl(ounit,keywrd,logval,comm,status)

# modify a logical value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyl(ounit,fkeywr,logval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkyj(ounit,keywrd,intval,comm,status)

# modify an integer value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyj(ounit,fkeywr,intval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkyf(ounit,keywrd,rval,decim,comm,status)

# modify a real*4 value header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyf(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkye(ounit,keywrd,rval,decim,comm,status)

# modify a real*4 value header record in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkye(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkyg(ounit,keywrd,dval,decim,comm,status)

# modify a double precision value header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyg(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsmkyd(ounit,keywrd,dval,decim,comm,status)

# modify a double precision value header record in E format
# If it will fit, the value field will be 20 characters wide;
# otherwise it will be expanded to up to 35 characters, left
# justified.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyd(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukys(ounit,keywrd,strval,comm,status)

# update a character string value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukys(ounit,fkeywr,fstrva,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukyl(ounit,keywrd,logval,comm,status)

# update a logical value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukyl(ounit,fkeywr,logval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukyj(ounit,keywrd,intval,comm,status)

# update an integer value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukyj(ounit,fkeywr,intval,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukyf(ounit,keywrd,rval,decim,comm,status)

# update a real*4 value header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukyf(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukye(ounit,keywrd,rval,decim,comm,status)

# update a real*4 value header record in E format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukye(ounit,fkeywr,rval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukyg(ounit,keywrd,dval,decim,comm,status)

# update a double precision value header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukyg(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsukyd(ounit,keywrd,dval,decim,comm,status)

# update a double precision value header record in E format
# If it will fit, the value field will be 20 characters wide;
# otherwise it will be expanded to up to 35 characters, left
# justified.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # i real*8 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftukyd(ounit,fkeywr,dval,decim,fcomm,status)
end
#-------------------------------------------------------------------
procedure fsdrec(iunit,keyno,status)

# delete a header keyword

int     iunit           # i input file pointer
int     keyno           # i position of the keyword to be deleted
int     status          # o error status

begin

call ftdkey(iunit,keyno,status)
end
#-------------------------------------------------------------------
procedure fsdkey(iunit,keywrd,status)

# delete a header keyword

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftdkey(iunit,fkeywr,status)
end
#-------------------------------------------------------------------
procedure fsprec(ounit,record,status)

# write a 80 character record to the FITS header

int     ounit           # i output file pointer
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftprec(ounit,frecor,status)
end
#-------------------------------------------------------------------
procedure fspcom(ounit,commnt,status)

# write a COMMENT record to the FITS header

int     ounit           # i output file pointer
char    commnt[SZ_FLONGCOMM]     # i comment keyword
%       character fcommn*72
int     status          # o error status

begin

call f77pak(commnt,fcommn,SZ_FLONGCOMM)

call ftpcom(ounit,fcommn,status)
end
#-------------------------------------------------------------------
procedure fsphis(ounit,histry,status)

# write a HISTORY record to the FITS header

int     ounit           # i output file pointer
char    histry[SZ_FLONGCOMM]     # i history keyword
%       character fhistr*72
int     status          # o error status

begin

call f77pak(histry,fhistr,SZ_FLONGCOMM)

call ftphis(ounit,fhistr,status)
end
#-------------------------------------------------------------------
procedure fspdat(ounit,status)

# write the current date to the DATE keyword in the ounit CHU

int     ounit           # i output file pointer
int     status          # o error status

begin

call ftpdat(ounit,status)
end
#-------------------------------------------------------------------
procedure fsgkys(iunit,keywrd,strval,comm,status)

# read a character string value and comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # o string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkys(iunit,fkeywr,fstrva,fcomm,status)

call f77upk(fstrva,strval,SZ_FSTRVAL)
call f77upk(fcomm ,comm  ,SZ_FCOMMENT)

end
#-------------------------------------------------------------------
procedure fsgkyl(iunit,keywrd,logval,comm,status)

# read a logical value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # o logical value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyl(iunit,fkeywr,logval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fsgkyj(iunit,keywrd,intval,comm,status)

# read an integer value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # o integer value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyj(iunit,fkeywr,intval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fsgkye(iunit,keywrd,rval,comm,status)

# read a real*4 value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # o real*4 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkye(iunit,fkeywr,rval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fsgkyd(iunit,keywrd,dval,comm,status)

# read a double precision value and comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # o real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyd(iunit,fkeywr,dval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fsgkyt(iunit,keywrd,intval,dval,comm,status)

# read an integer value and fractional parts of a keyword value
# and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # o integer value
double  dval            # o real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyt(iunit,fkeywr,intval,dval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fsgkns(iunit,keywrd,nstart,nmax,strval,nfound,status)

# read an array of character string values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
char    strval[SZ_FSTRVAL,ARB]     # o string value
%       character*70 fstrva
%       character*48 comm
%       character*8 keynam

int     nfound          # o no. of keywords found
int     status          # o error status
int	i
int	j

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

nfound=0
j=nstart

do i=1,nmax   {
  call ftkeyn(fkeywr,j,keynam,status)
  if (status > 0)
     go to 10

  call ftgkys(iunit,keynam,fstrva,comm,status)

  if (status <= 0) {
     nfound=i
     call f77upk(fstrva,strval(1,i),SZ_FSTRVAL)

  } else if (status == 202) {
#     ignore keyword not found error
      status=0
  }
  j=j+1
 }

10
   j=0
end
#-------------------------------------------------------------------
procedure fsgknl(iunit,keywrd,nstart,nmax,logval,nfound,status)

# read an array of logical values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
bool    logval[ARB]     # o logical values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknl(iunit,fkeywr,nstart,nmax,logval,nfound,status)
end
#-------------------------------------------------------------------
procedure fsgknj(iunit,keywrd,nstart,nmax,intval,nfound,status)

# read an array of integer values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
int     intval[ARB]     # o integer values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknj(iunit,fkeywr,nstart,nmax,intval,nfound,status)
end
#-------------------------------------------------------------------
procedure fsgkne(iunit,keywrd,nstart,nmax,rval,nfound,status)

# read an array of real*4 values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
real    rval[ARB]       # o real*4 values
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkne(iunit,fkeywr,nstart,nmax,rval,nfound,status)
end
#-------------------------------------------------------------------
procedure fsgknd(iunit,keywrd,nstart,nmax,dval,nfound,status)

# read an array of real*8 values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
double  dval[ARB]       # o real*8 value
int     nfound          # o no. of keywords found
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgknd(iunit,fkeywr,nstart,nmax,dval,nfound,status)
end
#-------------------------------------------------------------------
procedure fsgcrd(iunit,keywrd,card,status)

# Read the 80 character card image of a specified header keyword record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # o 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgcrd(iunit,fkeywr,fcard,status)

call f77upk(fcard  ,card ,SZ_FCARD)
end
#-------------------------------------------------------------------
procedure fsgkey(iunit,keywrd,value,comm,status)

# Read value and comment of a header keyword from the keyword buffer

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    value[SZ_FSTRVAL]           # o keyword value
%       character fvalue*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkey(iunit,fkeywr,fvalue,fcomm,status)

call f77upk(fvalue  ,value ,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FCOMMENT)

end
#-------------------------------------------------------------------
procedure fsgrec(iunit,nrec,record,status)

# Read the Nth 80-byte header record
# This routine is useful for reading the entire header, one
# record at a time.

int     iunit           # i input file pointer
int     nrec            # i number of keywords
char    record[SZ_FCARD]     # o 80-char header record
%       character frecor*80
int     status          # o error status

begin

call ftgrec(iunit,nrec,frecor,status)

call f77upk(frecor,record,SZ_FCARD)
end
#-------------------------------------------------------------------
procedure fsgkyn(iunit,nkey,keywrd,value,comm,status)

# Read the name, value, and comment of the NKEYth header record
# This routine is useful for reading the entire header, one
# record at a time.

int     iunit           # i input file pointer
int     nkey            # i number of keywords
char    keywrd[SZ_FKEYWORD]     # o keyword name
%       character fkeywr*8
char    value[SZ_FSTRVAL]      # o data value
%       character fvalue*70
char    comm[SZ_FLONGCOMM]       # o keyword comment
%       character fcomm*72
int     status          # o error status

begin

call ftgkyn(iunit,nkey,fkeywr,fvalue,fcomm,status)

call f77upk(fkeywr  ,keywrd ,SZ_FKEYWORD)
call f77upk(fvalue  ,value ,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FLONGCOMM)
end
#-------------------------------------------------------------------
procedure fspsvc(keyrec,value,comm,status)

# parse the header record to find value and comment strings

char    keyrec[SZ_FCARD]     # i header keyword string
%       character fkeyre*80
char    value[SZ_FSTRVAL]      # o data value
%       character fvalue*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keyrec,fkeyre,SZ_FCARD)

call ftpsvc(fkeyre,fvalue,fcomm,status)

call f77upk(fvalue ,value,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
#-------------------------------------------------------------------
procedure fskeyn(keywrd,nseq,keyout,status)

# Make a keyword name by concatinating the root name and a
# sequence number

char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nseq            # i keyword sequence no.
char    keyout[SZ_FKEYWORD]     # o output keyword
%       character fkeyou*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftkeyn(fkeywr,nseq,fkeyou,status)

call f77upk(fkeyou,keyout,SZ_FKEYWORD)
end
#-------------------------------------------------------------------
procedure fsnkey(nseq,keywrd,keyout,status)

# Make a keyword name by concatinating the root name and a
# sequence number

char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nseq            # i keyword sequence no.
char    keyout[SZ_FKEYWORD]     # o output keyword
%       character fkeyou*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftnkey(nseq,fkeywr,fkeyou,status)

call f77upk(fkeyou,keyout,SZ_FKEYWORD)
end
#-------------------------------------------------------------------
procedure fstkey(keywrd,status)

# test that keyword name contains only legal characters:
#   uppercase letters, numbers, hyphen, underscore, or space

char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call fttkey(fkeywr,status)
end
#-------------------------------------------------------------------
procedure fsphpr(ounit,simple,bitpix,naxis,naxes,
                 pcount,gcount,extend,status)

# write required primary header keywords

int     ounit           # i output file pointer
bool    simple          # i simple FITS file?
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     pcount          # i no. of group parameters
int     gcount          # i no. of groups
bool    extend          # i EXTEND keyword = TRUE?
int     status          # o error status

begin

call ftphpr(ounit,simple,bitpix,naxis,naxes,
                 pcount,gcount,extend,status)
end
#-------------------------------------------------------------------
procedure fsghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
                    pcount,gcount,extend,status)

# get the required primary header or image extension keywords

int     iunit           # i input file pointer
int     maxdim          # i max. number of dimensions
bool    simple          # o simple FITS file?
int     bitpix          # o bits per pixel
int     naxis           # o number of axes
int     naxes[ARB]      # o dimension of each axis
int     pcount          # o no. of group parameters
int     gcount          # o no. of groups
bool    extend          # o EXTEND keyword = TRUE?
int     status          # o error status

begin

call ftghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
                    pcount,gcount,extend,status)
end
#-------------------------------------------------------------------
procedure fsphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
                 tform,tunit,extnam,status)

# write required standard header keywords for an ASCII table extension

int     ounit           # i output file pointer
int     ncols           # i number of columns
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
int     tbcol[ARB]      # i starting column position
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftphtb(ounit,ncols,nrows,nfield,fttype,tbcol,
                 ftform,ftunit,fextna,status)
end
#-------------------------------------------------------------------
procedure fsitab(ounit,ncols,nrows,nfield,ttype,tbcol,
                 tform,tunit,extnam,status)

# insert an ASCII table extension

int     ounit           # i output file pointer
int     ncols           # i number of columns
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
int     tbcol[ARB]      # i starting column position
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftitab(ounit,ncols,nrows,nfield,fttype,tbcol,
                 ftform,ftunit,fextna,status)
end
#-------------------------------------------------------------------
procedure fsghtb(iunit,maxfld,ncols,nrows,nfield,ttype,
                    tbcol,tform,tunit,extnam,status)

# read required standard header keywords from an ASCII table extension

int     iunit           # i input file pointer
int     maxfld          # i max. number of fields to return
int     ncols           # o number of columns
int     nrows           # o number of rows
int     nfield          # o number of fields
char    ttype[SZ_FTTYPE,ARB]      # o column name
%       character*24 fttype(512)
int     tbcol[ARB]      # o starting column position
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     status          # o error status
int	i
int	n

begin

call ftghtb(iunit,maxfld,ncols,nrows,nfield,fttype,
                    tbcol,ftform,ftunit,fextna,status)

n=min(maxfld,nfield)
do i = 1, n
  { call f77upk(fttype(i) ,ttype(1,i),SZ_FTTYPE)
    call f77upk(ftform(i) ,tform(1,i),SZ_FTFORM)
    call f77upk(ftunit(i) ,tunit(1,i),SZ_FTUNIT)
  }

call f77upk(fextna ,extnam,SZ_FEXTNAME)

end
#-------------------------------------------------------------------
procedure fsphbn(ounit,nrows,nfield,ttype,tform,tunit,
                    extnam,pcount,status)

# write required standard header keywords for a binary table extension

int     ounit           # i output file pointer
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     pcount          # i size of 'heap'
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftphbn(ounit,nrows,nfield,fttype,ftform,ftunit,
                    fextna,pcount,status)
end
#-------------------------------------------------------------------
procedure fsibin(ounit,nrows,nfield,ttype,tform,tunit,
                    extnam,pcount,status)

# insert a binary table extension

int     ounit           # i output file pointer
int     nrows           # i number of rows
int     nfield          # i number of fields
char    ttype[SZ_FTTYPE,ARB]      # i column name
%       character*24 fttype(512)
char    tform[SZ_FTFORM,ARB]      # i column data format
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # i column units
%       character*24 ftunit(512)
char    extnam[SZ_FEXTNAME]     # i extension name
%       character fextna*24
int     pcount          # i size of 'heap'
int     status          # o error status
int	i

begin

do i = 1, nfield
  { call f77pak(ttype(1,i) ,fttype(i),SZ_FTTYPE)
    call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)
    call f77pak(tunit(1,i) ,ftunit(i),SZ_FTUNIT)
  }

call f77pak(extnam ,fextna,SZ_FEXTNAME)

call ftibin(ounit,nrows,nfield,fttype,ftform,ftunit,
                    fextna,pcount,status)
end
#-------------------------------------------------------------------
procedure fsghbn(iunit,maxfld,nrows,nfield,ttype,tform,
                   tunit,extnam,pcount,status)

# read required standard header keywords from a binary table extension

int     iunit           # i input file pointer
int     maxfld          # i max. number of fields
int     nrows           # o number of rows
int     nfield          # o number of fields
char    ttype[SZ_FTTYPE,ARB]      # o column name
%       character*24 fttype(512)
char    tform[SZ_FTFORM,ARB]      # o column datatype
%       character*16 ftform(512)
char    tunit[SZ_FTUNIT,ARB]      # o column units
%       character*16 ftunit(512)
char    extnam
%       character fextna*48
int     pcount          # o size of 'heap'
int     status          # o error status
int	i
int     n

begin

call ftghbn(iunit,maxfld,nrows,nfield,fttype,ftform,
                   ftunit,fextna,pcount,status)
n=min(maxfld,nfield)
do i = 1, n
  { call f77upk(fttype(i) ,ttype(1,i),SZ_FTTYPE)
    call f77upk(ftform(i) ,tform(1,i),SZ_FTFORM)
    call f77upk(ftunit(i) ,tunit(1,i),SZ_FTUNIT)
  }

call f77upk(fextna ,extnam,SZ_FEXTNAME)

end
#-------------------------------------------------------------------
procedure fsgabc(nfield,tform,space,rowlen,tbcol,status)

# Get ASCII table Beginning Columns
# determine the byte offset of the beginning of each field of a
# ASCII table, and the total width of the table

int     nfield          # i number of fields
char    tform[SZ_FTFORM,ARB]      # i column datatypes
%       character*16 ftform(512)
int     space           # i no. spaces between col
int     rowlen          # o length of a table row
int     tbcol[ARB]      # o starting column positions
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftgabc(nfield,ftform,space,rowlen,tbcol,status)
end
#-------------------------------------------------------------------
procedure fsbnfm(tform,dtype,rcount,width,status)

# 'Binary Format'
# parse the binary table column format to determine the data
# type and the repeat count (and string width, if it is an ASCII field)

char    tform[SZ_FTFORM]       # i column format
%       character*16 ftform
int     dtype           # o datatype code
int     rcount          # o vector column repeat count
int     width           # o width of character string
int     status          # o error status

begin

call f77pak(tform  ,ftform ,SZ_FTFORM)

call ftbnfm(ftform,dtype,rcount,width,status)
end
#-------------------------------------------------------------------
procedure fsinit(funit,fname,block,status)

# open a new FITS file with write access

int     funit           # i file I/O pointer
char    fname[ARB]      # i file name
%       character ffname*255
int     block           # i FITS blocking factor
int     status          # o error status

begin

call f77pak(fname ,ffname,255)

call ftinit(funit,ffname,block,status)
end
#-------------------------------------------------------------------
procedure fsopen(funit,fname,rwmode,block,status)

# open an existing FITS file with readonly or read/write access

int     funit           # i file I/O pointer
char    fname[ARB]      # i file name
%       character ffname*255
int     rwmode          # i file read/write mode
int     block           # i FITS blocking factor
int     status          # o error status

begin

call f77pak(fname ,ffname,255)

call ftopen(funit,ffname,rwmode,block,status)
end
#-------------------------------------------------------------------
procedure fsclos(iunit,status)

# close a FITS file that was previously opened with ftopen or ftinit

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftclos(iunit,status)
end
#-------------------------------------------------------------------
procedure fsdelt(iunit,status)

# close and delete a FITS file that was previously opened with ftopen or ftinit

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftdelt(iunit,status)
end
#-------------------------------------------------------------------
procedure fshdef(ounit,moreky,status)

# Header DEFinition
# define the size of the current header unit; this simply lets
# us determine where the data unit will start

int     ounit           # i output file pointer
int     moreky          # i reserve space for this many more keywords
int     status          # o error status

begin

call fthdef(ounit,moreky,status)
end
#-------------------------------------------------------------------
procedure fsghps(ounit,nexist,keyno,status)

# return the current position in the header

int     ounit           # i output file pointer
int     nexist          # o how many exist?
int     keyno           # o position of the last keyword that was read + 1
int     status          # o error status

begin

call ftghps(ounit,nexist,keyno,status)
end
#-------------------------------------------------------------------
procedure fsghsp(ounit,nexist,nmore,status)

# Get Header SPace
# return the number of additional keywords that will fit in the header

int     ounit           # i output file pointer
int     nexist          # o how many exist?
int     nmore           # o this many more will fit
int     status          # o error status

begin

call ftghsp(ounit,nexist,nmore,status)
end
#-------------------------------------------------------------------
procedure fsgthd(tmplat,card,hdtype,status)

# 'Get Template HeaDer'
# parse a template header line and create a formated
# 80-character string which is suitable for appending to a FITS header

char    tmplat[ARB]     # i template string
%       character ftmpla*100
char    card[SZ_FCARD]       # o 80-char header record
%       character fcard*80
int     hdtype          # o hdu type code
int     status          # o error status

begin

call f77pak(tmplat,ftmpla,100)

call ftgthd(ftmpla,fcard,hdtype,status)

call f77upk(fcard  ,card ,SZ_FCARD)
end
#-------------------------------------------------------------------
procedure fsrdef(ounit,status)

# Data DEFinition
# re-define the length of the data unit
# this simply redefines the start of the next HDU

int     ounit           # i output file pointer
int     status          # o error status

begin

call ftrdef(ounit,status)
end
#-------------------------------------------------------------------
procedure fsddef(ounit,bytlen,status)

# Data DEFinition
# re-define the length of the data unit
# this simply redefines the start of the next HDU

int     ounit           # i output file pointer
int     bytlen          # i length in bytes
int     status          # o error status

begin

call ftddef(ounit,bytlen,status)
end
#-------------------------------------------------------------------
procedure fspdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)

# Primary data DEFinition
# define the structure of the primary data unit or an IMAGE extension

int     ounit           # i output file pointer
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     pcount          # i number of group parame
int     gcount          # i number of groups
int     status          # o error status

begin

call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
end
#-------------------------------------------------------------------
procedure fsiimg(ounit,bitpix,naxis,naxes,status)

# insert an IMAGE extension

int     ounit           # i output file pointer
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftiimg(ounit,bitpix,naxis,naxes,status)
end
#-------------------------------------------------------------------
procedure fsadef(ounit,lenrow,nfield,tbcol,tform,nrows,status)

# Ascii table data DEFinition
# define the structure of the ASCII table data unit

int     ounit           # i output file pointer
int     lenrow          # o length of a table row
int     nfield          # i number of fields
int     tbcol[ARB]      # i beginning volumn
char    tform[SZ_FTFORM,ARB]      # i column datatype
%       character*16 ftform(512)
int     nrows           # i number of rows
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftadef(ounit,lenrow,nfield,tbcol,ftform,nrows,status)
end
#-------------------------------------------------------------------
procedure fsbdef(ounit,nfield,tform,pcount,nrows,status)

# Binary table data DEFinition
# define the structure of the binary table data unit

int     ounit           # i output file pointer
int     nfield          # i number of fields
char    tform[SZ_FTFORM,ARB]      # i column datatype
%       character*16 ftform(512)
int     pcount          # i number of group parame
int     nrows           # i number of rows
int     status          # o error status
int	i

begin

do i=1,nfield
  call f77pak(tform(1,i) ,ftform(i),SZ_FTFORM)

call ftbdef(ounit,nfield,tform,pcount,nrows,status)
end
#-------------------------------------------------------------------
procedure fspthp(ounit,heap,status)

# Define the starting address for the heap for a binary table.
# The default address is NAXIS1 * NAXIS2.  It is in units of
# bytes relative to the beginning of the regular binary table data.
# This routine also writes the appropriate THEAP keyword to the
# FITS header.

int     ounit           # i output file pointer
int     heap            # i heap starting address
int     status          # o error status

begin

call ftpthp(ounit,heap,status)
end
#-------------------------------------------------------------------
procedure fspscl(ounit,bscale,bzero,status)

# Primary SCaLing factor definition
# Define the scaling factor for the primary header data.
# This must be the first HDU of the FITS file.

int     ounit           # i output file pointer
double  bscale          # i scaling factor
double  bzero           # i scaling zeropoint
int     status          # o error status

begin

call ftpscl(ounit,bscale,bzero,status)
end
#-------------------------------------------------------------------
procedure fspnul(ounit,blank,status)

# Primary Null value definition
# Define the null value for an integer primary array.
# This must be the first HDU of the FITS file.

int     ounit           # i output file pointer
int     blank           # i value used to represent undefined values
int     status          # o error status

begin

call ftpnul(ounit,blank,status)
end
#-------------------------------------------------------------------
procedure fstscl(ounit,colnum,bscale,bzero,status)

# Table column SCaLing factor definition
# Define the scaling factor for a table column.

int     ounit           # i output file pointer
int     colnum          # i column number
double  bscale          # i scaling factor
double  bzero           # i scaling zeropoint
int     status          # o error status

begin

call fttscl(ounit,colnum,bscale,bzero,status)
end
#-------------------------------------------------------------------
procedure fstnul(ounit,colnum,inull,status)

# Table column NULl value definition
# Define the null value for an integer binary table column

int     ounit           # i output file pointer
int     colnum          # i column number
int     inull           # integer null value
int     status          # o error status

begin

call fttnul(ounit,colnum,inull,status)
end
#-------------------------------------------------------------------
procedure fssnul(ounit,colnum,nulval,status)

# ascii table Column NULl value definition
# Define the null value for an ASCII table column.

int     ounit           # i output file pointer
int     colnum          # i column number
char    nulval          # i value for undefined pixels
%       character*16 fnulva
int     status          # o error status

begin

call f77pak(nulval,fnulva,16)

call ftsnul(ounit,colnum,fnulva,status)
end
#-------------------------------------------------------------------
procedure fsgcno(iunit,exact,colnam,colnum,status)

# determine the column number corresponding to an input column name.
# this assumes that the first 16 characters uniquely define the name

int     iunit           # i input file pointer
bool    exact           # i require same case?
char    colnam[SZ_FTTYPE]     # column name
%       character fcolna*24
int     colnum          # o column number
int     status          # o error status

begin

call f77pak(colnam,fcolna,SZ_FTTYPE)

call ftgcno(iunit,exact,fcolna,colnum,status)
end
#-------------------------------------------------------------------
procedure fsgacl(iunit,colnum,ttype,tbcol,tunit,tform,
        tscal,tzero,tnull,tdisp,status)

# Get information about an Ascii table CoLumn
# returns the parameters which define the column

int     iunit           # i input file pointer
int     colnum          # i column number
char    ttype[SZ_FTTYPE]           # o column name
int     tbcol           # o starting column position in the row
char    tunit[SZ_FTUNIT]           # o physical units of the column
char    tform[SZ_FTFORM]           # o FITS data format of the column
double  tscal           # o scaling factor
double  tzero           # o scaling zero point
char    tnull[SZ_FTNULL]      # o string used to represent null values
char    tdisp[SZ_FTFORM]      # o Fortran display format
int     status          # o error status
%       character fttype*24, ftunit*24,ftform*16,ftnull*16,ftdisp*16

begin

call ftgacl(iunit,colnum,fttype,tbcol,ftunit,ftform,
        tscal,tzero,ftnull,ftdisp,status)

call f77upk(fttype,ttype,SZ_FTTYPE)
call f77upk(ftunit,tunit,SZ_FTUNIT)
call f77upk(ftform,tform,SZ_FTFORM)
call f77upk(ftnull,tnull,SZ_FTNULL)
call f77upk(ftdisp,tdisp,SZ_FTFORM)

end
#-------------------------------------------------------------------
procedure fsgbcl(iunit,colnum,ttype,tunit,dtype,rcount,
        tscal,tzero,tnull,tdisp,status)

# Get information about a Binary table CoLumn
# returns the parameters which define the column

int     iunit           # i input file pointer
int     colnum          # i column number
char    ttype[SZ_FTTYPE]           # o column name
char    tunit[SZ_FTUNIT]           # o physical units of the column
int     dtype           # o datatype code
int     rcount          # o repeat count for vector column
double  tscal           # o scaling factor
double  tzero           # o scaling zero point
int     tnull           # o integer used to represent null values
char    tdisp[SZ_FTFORM]      # o Fortran display format
int     status          # o error status
%       character fttype*24, ftunit*24,ftdisp*16

begin

call ftgbcl(iunit,colnum,fttype,ftunit,dtype,rcount,
        tscal,tzero,tnull,ftdisp,status)

call f77upk(fttype,ttype,SZ_FTTYPE)
call f77upk(ftunit,tunit,SZ_FTUNIT)
call f77upk(ftdisp,tdisp,SZ_FTFORM)

end
#-------------------------------------------------------------------
procedure fsptdm(ounit,colnum,naxis,naxes,status)

# write the TDIMnnn keyword

int     ounit           # i output file pointer
int     colnum          # i column number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftptdm(ounit,colnum,naxis,naxes,status)
end
#-------------------------------------------------------------------
procedure fsgtdm(iunit,colnum,maxdim,naxis,naxes,status)

# read the TDIMnnn keyword

int     iunit           # i input file pointer
int     colnum          # i column number
int     maxdim          # i maximum number of dimensions to return
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftgtdm(iunit,colnum,maxdim,naxis,naxes,status)
end
#-------------------------------------------------------------------
procedure fsmahd(iunit,extno,xtend,status)

# Move to Absolute Header Data unit
# move the i/o pointer to the specified HDU and initialize all
# the common block parameters which describe the extension

int     iunit           # i input file pointer
int     extno           # i extension number
int     xtend           # o type of extension
int     status          # o error status

begin

call ftmahd(iunit,extno,xtend,status)
end
#-------------------------------------------------------------------
procedure fsmrhd(iunit,extmov,xtend,status)

# Move Relative Header Data unit
# move the i/o pointer to the specified HDU and initialize all
# the common block parameters which describe the extension

int     iunit           # i input file pointer
int     extmov          # i relative extension number
int     xtend           # o type of extension
int     status          # o error status

begin

call ftmrhd(iunit,extmov,xtend,status)
end
#-------------------------------------------------------------------
procedure fsghdn(iunit,hdunum)

# return the number of the current header data unit.  The
# first HDU (the primary array) is number 1.

int     iunit           # i input file pointer
int     hdunum          # o returned number of the current HDU

begin

call ftghdn(iunit,hdunum)
end
#-------------------------------------------------------------------
procedure fscopy(iunit,ounit,moreky,status)

# copies the CHDU from IUNIT to the CHDU of OUNIT.
# This will also reserve space in the header for MOREKY keywords
# if MOREKY > 0.

int     iunit           # i input file pointer
int     ounit           # i output file pointer
int     moreky          # i how many more keywords
int     status          # o error status

begin

call ftcopy(iunit,ounit,moreky,status)
end
#-------------------------------------------------------------------
procedure fscpdt(iunit,ounit,status)

# copies the data from IUNIT to the CHDU of OUNIT.


int     iunit           # i input file pointer
int     ounit           # i output file pointer
int     status          # o error status

begin

call ftcpdt(iunit,ounit,status)
end
#-------------------------------------------------------------------
procedure fsirow(ounit,frow,nrows,status)

# insert rows in a table

int     ounit           # i output file pointer
int     frow            # insert rows after this row
int     nrows           # number of rows
int     status          # o error status

begin

call ftirow(ounit,frow,nrows,status)
end
#-------------------------------------------------------------------
procedure fsdrow(ounit,frow,nrows,status)

# delete rows in a table

int     ounit           # i output file pointer
int     frow            # first row to delete
int     nrows           # number of rows
int     status          # o error status

begin

call ftdrow(ounit,frow,nrows,status)
end
#-------------------------------------------------------------------
procedure fsicol(ounit,colnum,ttype,tform,status)

# insert column in a table

int     ounit           # i output file pointer
int     colnum          # i column to be inserted
char    ttype[SZ_FTTYPE]      # i column name
%       character*24 ftype
char    tform[SZ_FTFORM]      # i column data format
%       character*16 fform
int     status          # o error status

begin

call f77pak(ttype ,ftype,SZ_FTTYPE)
call f77pak(tform ,fform,SZ_FTFORM)

call fticol(ounit,colnum,ftype,fform,status)
end
#-------------------------------------------------------------------
procedure fsdcol(ounit,colnum,status)

# delete column in a table

int     ounit           # i output file pointer
int     colnum          # i column to be deleted
int     status          # o error status

begin

call ftdcol(ounit,colnum,status)
end
#-------------------------------------------------------------------
procedure fscrhd(iunit,status)

# 'CReate Header Data unit'
# create, initialize, and move the i/o pointer to a new extension at
# the end of the FITS file.

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftcrhd(iunit,status)
end
#-------------------------------------------------------------------
procedure fsdhdu(iunit,hdutyp,status)

# delete the CHDU

int     iunit           # i input file pointer
int     hdutyp          # o type of the new CHDU
int     status          # o error status

begin

call ftdhdu(iunit,hdutyp,status)
end
#-------------------------------------------------------------------
procedure fsghad(iunit,curadd,nxtadd)

# delete the CHDU

int     iunit           # i input file pointer
int     curadd          # o starting byte address of the CHDU
int     nxtadd          # o starting byte address of the next HDU

begin

call ftghad(iunit,curadd,nxtadd)
end
#-------------------------------------------------------------------
procedure fspdes(ounit,colnum,rownum,nelem,offset,status)

# write the descriptor values to a binary table.  This is only
# used for column which have TFORMn = 'P', i.e., for variable
# length arrays.

int     ounit           # i output file pointer
int     colnum          # i column number
int     rownum          # i row number
int     nelem           # i number of elements
int     offset          # i offset
int     status          # o error status

begin

call ftpdes(ounit,colnum,rownum,nelem,offset,status)
end
#-------------------------------------------------------------------
procedure fsgdes(iunit,colnum,rownum,nelem,offset,status)

# read the descriptor values from a binary table.  This is only
# used for column which have TFORMn = 'P', i.e., for variable
# length arrays.

int     iunit           # i input file pointer
int     colnum          # i column number
int     rownum          # i row number
int     nelem           # o number of elements
int     offset          # o offset
int     status          # o error status

begin

call ftgdes(iunit,colnum,rownum,nelem,offset,status)
end
#-------------------------------------------------------------------
procedure fspcls(ounit,colnum,frow,felem,nelem,sray,dim1,status)

# write an array of character string values to the  specified column of
# the table.
# The binary or ASCII table column being written to must have datatype 'A'

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
char    sray[dim1,ARB]       # i array of strings
int     dim1            # i size of 1st dimension of 2D character string array
%       character*256 fsray
int     status          # o error status
int	i
int	elem

begin

elem=felem
do i=1,nelem            {
  call f77pak(sray(1,i),fsray,dim1)
  call ftpcls(ounit,colnum,frow,elem,1,fsray,status)
  elem=elem+1
}
end
#-------------------------------------------------------------------
procedure fspcll(ounit,colnum,frow,felem,nelem,lray,status)

# write an array of logical values to the  specified column of the table.
# The binary table column being written to must have datatype 'L'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
bool    lray[ARB]       # i logical array
int     status          # o error status

begin

call ftpcll(ounit,colnum,frow,felem,nelem,lray,status)
end
#-------------------------------------------------------------------
procedure fspclb(ounit,colnum,frow,felem,nelem,array,status)

# write an array of unsigned byte data values to the
# specified column of the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpclb(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspcli(ounit,colnum,frow,felem,nelem,array,status)

# write an array of integer*2 data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftpcli(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspclj(ounit,colnum,frow,felem,nelem,array,status)

# write an array of integer data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpclj(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspcle(ounit,colnum,frow,felem,nelem,array,status)

# write an array of real data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftpcle(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspcld(ounit,colnum,frow,felem,nelem,array,status)

# write an array of double precision data values to the specified column
# of the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpcld(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspclc(ounit,colnum,frow,felem,nelem,array,status)

# write an array of single precision complex data values to the
# specified column of the table.
# The binary table column being written to must have datatype 'C'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftpclc(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspclm(ounit,colnum,frow,felem,nelem,array,status)

# write an array of double precision complex data values to the
# specified column of the table.
# The binary table column being written to must have datatype 'M'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpclm(ounit,colnum,frow,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspclu(ounit,colnum,frow,felem,nelem,status)

# set elements of a table to be undefined

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     status          # o error status

begin

call ftpclu(ounit,colnum,frow,felem,nelem,status)
end
#-------------------------------------------------------------------
procedure fspcnb(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of unsigned byte data values to the
# specified column of the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     nulval          # i value representing a null
int     status          # o error status

begin

call ftpcnb(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fspcni(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of integer*2 data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # i array of values
short   nulval          # i value representing a null
int     status          # o error status

begin

call ftpcni(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fspcnj(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of integer data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     nulval          # i value representing a null
int     status          # o error status

begin

call ftpcnj(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fspcne(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of real data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # r array of values
real    nulval          # r value representing a null
int     status          # o error status

begin

call ftpcne(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fspcnd(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of double precision data values to the specified column
# of the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
double  nulval          # d value representing a null
int     status          # o error status

begin

call ftpcnd(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsgcl(iunit,colnum,frow,felem,nelem,lray,status)

# read an array of logical values from a specified column of the table.
# The binary table column being read from must have datatype 'L'
# and no datatype conversion will be perform if it is not.
# This routine ignores any undefined values in the logical array.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
bool    lray[ARB]       # o logical array
int     status          # o error status

begin

call ftgcl(iunit,colnum,frow,felem,nelem,lray,status)
end
#-------------------------------------------------------------------
procedure fsgcvs(iunit,colnum,frow,felem,nelem,nulval,array,dim1,anynul,
                status)

# read an array of string values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=' ', in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
char    nulval[SZ_FTNULL] # i value for undefined pixels
%       character fnulva*16
char    array[dim1,ARB]      # o array of values
%       character farray*256
int	dim1		# i size of 1st dimension of 2D character string array
bool    anynul          # o any null values returned?
int     status          # o error status
int	i
int	elem
bool    null

begin

call f77pak(nulval,fnulva,SZ_FTNULL)

anynul=false
elem=felem
do i=1,nelem    {
  call ftgcvs(iunit,colnum,frow,elem,1,fnulva,farray,null,status)
  if (null)
     anynul=true

  call f77upk(farray,array(1,i),dim1)
  elem=elem+1
 }
end
#-------------------------------------------------------------------
procedure fsgcvb(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)

# read an array of byte values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     nulval          # i value for undefined pixels
int     array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvb(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcvi(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)

# read an array of I*2 values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
short   nulval          # i value for undefined pixels
short   array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvi(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcvj(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)

# read an array of I*4 values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     nulval          # i value for undefined pixels
int     array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvj(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcve(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)

# read an array of R*4 values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    nulval          # i value for undefined pixels
real    array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcve(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcvd(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)

# read an array of r*8 values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  nulval          # i value for undefined pixels
double  array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvd(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcvc(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)

# read an array of complex values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    nulval[2]       # i value for undefined pixels
real    array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvc(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcvm(iunit,colnum,frow,felem,nelem,nulval,array,
          anynul,status)

# read an array of double precision complex values from a specified
# column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  nulval[2]       # i value for undefined pixels
double  array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvm(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfs(iunit,colnum,frow,felem,nelem,array,dim1,
          flgval,anynul,status)

# read an array of string values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
char    array[dim1,ARB]      # o array of values
%       character farray*256
int	dim1		# i size of 1st dimension of 2D character string array
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status
int	i
int	elem
bool    null

begin

anynul=false
elem=felem
do i=1,nelem    {
  call ftgcvs(iunit,colnum,frow,elem,1,farray,flgval(i),null,status)
  if (null)
     anynul=true

  call f77upk(farray,array(1,i),dim1)
  elem=elem+1
 }
end
#-------------------------------------------------------------------
procedure fsgcfl(iunit,colnum,frow,felem,nelem,lray,
          flgval,anynul,status)

# read an array of logical values from a specified column of the table.
# The binary table column being read from must have datatype 'L'
# and no datatype conversion will be perform if it is not.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
bool    lray[ARB]       # o logical array
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfl(iunit,colnum,frow,felem,nelem,lray,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfb(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)

# read an array of byte values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfb(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfi(iunit,colnum,frow,felem,nelem,array,
         flgval,anynul,status)

# read an array of I*2 values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfi(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfj(iunit,colnum,frow,felem,nelem,array,
         flgval,anynul,status)

# read an array of I*4 values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]       # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfj(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfe(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)

# read an array of R*4 values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfe(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfd(iunit,colnum,frow,felem,nelem,array,
         flgval,anynul,status)

# read an array of r*8 values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfd(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfc(iunit,colnum,frow,felem,nelem,array,
         flgval,anynul,status)

# read an array of complex values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfc(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcfm(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)

# read an array of double precision complex values from a specified
# column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfm(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgcxi(iunit,colnum,frow,nrow,fbit,nbit,ivalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
short   ivalue[ARB]     # o short integer array
int     status          # o error status

begin

call ftgcxi(iunit,colnum,frow,nrow,fbit,nbit,ivalue,status)
end
#-------------------------------------------------------------------
procedure fsgcxj(iunit,colnum,frow,nrow,fbit,nbit,jvalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
int     jvalue[ARB]     # o integer array
int     status          # o error status

begin

call ftgcxj(iunit,colnum,frow,nrow,fbit,nbit,jvalue,status)
end
#-------------------------------------------------------------------
procedure fsgcxd(iunit,colnum,frow,nrow,fbit,nbit,dvalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
double  dvalue[ARB]     # o double integer array
int     status          # o error status

begin

call ftgcxd(iunit,colnum,frow,nrow,fbit,nbit,dvalue,status)
end
#-------------------------------------------------------------------
procedure fsgcx(iunit,colnum,frow,fbit,nbit,lray,status)

# read an array of logical values from a specified bit or byte
# column of the binary table.  A logical .true. value is returned
# if the corresponding bit is 1, and a logical .false. value is
# returned if the bit is 0.
# The binary table column being read from must have datatype 'B'
# or 'X'. This routine ignores any undefined values in the 'B' array.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     fbit            # i first bit
int     nbit            # i number of bits
bool    lray[ARB]       # o logical array
int     status          # o error status

begin

call ftgcx(iunit,colnum,frow,fbit,nbit,lray,status)
end
#-------------------------------------------------------------------
procedure fspclx(iunit,colnum,frow,fbit,nbit,lray,status)

# write an array of logical values to a specified bit or byte
# column of the binary table.   If the LRAY parameter is .true.,
# then the corresponding bit is set to 1, otherwise it is set
# to 0.
# The binary table column being written to must have datatype 'B'
# or 'X'.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     fbit            # i first bit
int     nbit            # i number of bits
bool    lray[ARB]       # i logical array
int     status          # o error status

begin

call ftpclx(iunit,colnum,frow,fbit,nbit,lray,status)
end
#-------------------------------------------------------------------
procedure fsgtbs(iunit,frow,fchar,nchars,svalue,status)

# read a consecutive string of characters from an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of characters
char    svalue[ARB]     # o string value
%       character fsvalu*256
int     status          # o error status
int	readfirst
int	writefirst
int	ntodo
int	itodo

begin

# since the string may be arbitrarily long, read it in pieces
readfirst=fchar
writefirst=1
ntodo=nchars
itodo=min(256,ntodo)

while (itodo > 0) {
  call ftgtbs(iunit,frow,readfirst,itodo,fsvalu,status)
  call fsupk(fsvalu,svalue[writefirst],itodo)
  writefirst=writefirst+itodo
  readfirst=readfirst+itodo
  ntodo=ntodo-itodo
  itodo=min(256,ntodo)
 }

end
#-------------------------------------------------------------------
procedure fsgtbb(iunit,frow,fchar,nchars,value,status)

# read a consecutive string of bytes from an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of bytes
int     value[ARB]      # o data value
int     status          # o error status

begin

call ftgtbb(iunit,frow,fchar,nchars,value,status)
end
#-------------------------------------------------------------------
procedure fsptbs(iunit,frow,fchar,nchars,svalue,status)

# write a consecutive string of characters to an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of characters
char    svalue[ARB]     # i string value
%       character fsvalu*256
int     status          # o error status
int	readfirst
int	writefirst
int	ntodo
int	itodo

begin

# since the string may be arbitrarily long, write it in pieces
readfirst=1
writefirst=fchar
ntodo=nchars
itodo=min(256,ntodo)

while (itodo > 0) {
  call f77pak(svalue[readfirst],fsvalu,itodo)
  call ftptbs(iunit,frow,writefirst,itodo,fsvalu,status)
  writefirst=writefirst+itodo
  readfirst=readfirst+itodo
  ntodo=ntodo-itodo
  itodo=min(256,ntodo)
 }

end
#-------------------------------------------------------------------
procedure fsptbb(iunit,frow,fchar,nchars,value,status)

# write a consecutive string of bytes to an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of bytes
int     value[ARB]      # i data value
int     status          # o error status

begin

call ftptbb(iunit,frow,fchar,nchars,value,status)
end
#-------------------------------------------------------------------
procedure fspprb(ounit,group,felem,nelem,array,status)

# Write an array of byte values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpprb(ounit,group,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fsppri(ounit,group,felem,nelem,array,status)

# Write an array of i*2 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftppri(ounit,group,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspprj(ounit,group,felem,nelem,array,status)

# Write an array of i*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpprj(ounit,group,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fsppre(ounit,group,felem,nelem,array,status)

# Write an array of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftppre(ounit,group,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fspprd(ounit,group,felem,nelem,array,status)

# Write an array of r*8 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpprd(ounit,group,felem,nelem,array,status)
end
#-------------------------------------------------------------------
procedure fsppnb(ounit,group,felem,nelem,array,nulval,status)

# Write an array of byte values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     nulval          # i value used for null pixels 
int     status          # o error status

begin

call ftppnb(ounit,group,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsppni(ounit,group,felem,nelem,array,nulval,status)

# Write an array of i*2 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # i array of values
short   nulval          # i value used for null pixels 
int     status          # o error status

begin

call ftppni(ounit,group,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsppnj(ounit,group,felem,nelem,array,nulval,status)

# Write an array of i*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     nulval          # i value used for null pixels 
int     status          # o error status

begin

call ftppnj(ounit,group,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsppne(ounit,group,felem,nelem,array,nulval,status)

# Write an array of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # r array of values
real    nulval          # r value used for null pixels 
int     status          # o error status

begin

call ftppne(ounit,group,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsppnd(ounit,group,felem,nelem,array,nulval,status)

# Write an array of r*8 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
double  nulval          # d value used for null pixels 
int     status          # o error status

begin

call ftppnd(ounit,group,felem,nelem,array,nulval,status)
end
#-------------------------------------------------------------------
procedure fsppru(ounit,group,felem,nelem,status)

# set elements of the primary array equal to the undefined value

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     status          # o error status

begin

call ftppru(ounit,group,felem,nelem,status)
end
#-------------------------------------------------------------------
procedure fspgpb(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpb(ounit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fspgpi(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpi(ounit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fspgpj(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpj(ounit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fspgpe(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpe(ounit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fspgpd(ounit,group,fparm,nparm,array,status)

# Write an array of group parmeters into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpgpd(ounit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsgpvb(iunit,group,felem,nelem,nulval,
                   array,anynul,status)

# Read an array of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     nulval          # i value for undefined pixel
int     array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpvb(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpvi(iunit,group,felem,nelem,nulval,
                   array,anynul,status)

# Read an array of i*2 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
short   nulval          # i value for undefined pixels
short   array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpvi(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpvj(iunit,group,felem,nelem,nulval,
                  array,anynul,status)

# Read an array of i*4 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     nulval          # i value for undefined pixels
int     array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpvj(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpve(iunit,group,felem,nelem,nulval,
                    array,anynul,status)

# Read an array of r*4 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
real    nulval          # i value for undefined pixels
real    array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpve(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpvd(iunit,group,felem,nelem,nulval,
                  array,anynul,status)

# Read an array of r*8 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
double  nulval          # i value for undefined pixels
double  array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpvd(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpfb(iunit,group,felem,nelem,
                   array,flgval,anynul,status)

# Read an array of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfb(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpfi(iunit,group,felem,nelem,
                    array,flgval,anynul,status)

# Read an array of I*2 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfi(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpfj(iunit,group,felem,nelem,
                   array,flgval,anynul,status)

# Read an array of I*4 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfj(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpfe(iunit,group,felem,nelem,
                   array,flgval,anynul,status)

# Read an array of r*4 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfe(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsgpfd(iunit,group,felem,nelem,
                   array,flgval,anynul,status)

# Read an array of r*8 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfd(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
#-------------------------------------------------------------------
procedure fsggpb(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # o array of values
int     status          # o error status

begin

call ftggpb(iunit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsggpi(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
short   array[ARB]      # o array of values
int     status          # o error status

begin

call ftggpi(iunit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsggpj(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftggpj(iunit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsggpe(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftggpe(iunit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsggpd(iunit,group,fparm,nparm,array,status)

# Read an array of group parameter values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     fparm           # i first parameter
int     nparm           # i number of parameters
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftggpd(iunit,group,fparm,nparm,array,status)
end
#-------------------------------------------------------------------
procedure fsp2db(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of byte values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2db(ounit,group,dim1,nx,ny,array,status)
end
#-------------------------------------------------------------------
procedure fsp2di(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of i*2 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2di(ounit,group,dim1,nx,ny,array,status)
end
#-------------------------------------------------------------------
procedure fsp2dj(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of i*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2dj(ounit,group,dim1,nx,ny,array,status)
end
#-------------------------------------------------------------------
procedure fsp2de(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2de(ounit,group,dim1,nx,ny,array,status)
end
#-------------------------------------------------------------------
procedure fsp2dd(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of r*8 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2dd(ounit,group,dim1,nx,ny,array,status)
end
#-------------------------------------------------------------------
procedure fsp3db(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of byte values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3db(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
#-------------------------------------------------------------------
procedure fsp3di(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of i*2 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3di(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
#-------------------------------------------------------------------
procedure fsp3dj(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of i*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3dj(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
#-------------------------------------------------------------------
procedure fsp3de(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3de(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
#-------------------------------------------------------------------
procedure fsp3dd(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of r*8 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3dd(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
#-------------------------------------------------------------------
procedure fsg2db(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
int     nulval          # i value for undefined pi
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2db(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg2di(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of i*2 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
short   nulval          # i value for undefined pi
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
short   array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2di(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg2dj(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of i*4 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
int     nulval          # i value for undefined pi
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2dj(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg2de(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of real values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
real    nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
real    array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2de(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg2dd(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of r*8 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
double  nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
double  array[ARB]      # i array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2dd(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg3db(ounit,group,nulval,dim1,dim2,nx,ny,nz,
   array,anyflg,status)

# Read a 3-d cube of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
int     nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3db(ounit,group,nulval,dim1,dim2,nx,ny,nz,
   array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg3di(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)

# Read a 3-d cube of i*2 values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
short   nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
short   array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3di(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg3dj(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                   array,anyflg,status)

# Read a 3-d cube of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
int     nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3dj(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg3de(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                   array,anyflg,status)

# Read a 3-d cube of real values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
real    nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
real    array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3de(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                   array,anyflg,status)

# Read a 3-d cube of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
double  nulval          # i value for undefined pi
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
double  array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fspssb(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of byte values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpssb(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
#-------------------------------------------------------------------
procedure fspssi(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of integer*2 values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftpssi(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
#-------------------------------------------------------------------
procedure fspssj(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of integer values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpssj(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
#-------------------------------------------------------------------
procedure fspsse(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of real values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftpsse(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
#-------------------------------------------------------------------
procedure fspssd(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of double precision values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpssd(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
#-------------------------------------------------------------------
procedure fsgsvb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 nulval,array,anyflg,status)
# Read a subsection of byte values from the primary array.

int     iunit           # i input file pointer
int     colnum           # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
int     nulval          # i value for undefined pi
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsvb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 nulval,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsvi(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                   nulval,array,anyflg,status)

# Read a subsection of Integer*2 values from the primary array.

int     iunit           # i input file pointer
int     colnum           # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
short   nulval          # i value for undefined pi
short   array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsvi(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 nulval,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsvj(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 nulval,array,anyflg,status)

# Read a subsection of integer values from the primary array.

int     iunit           # i input file pointer
int     colnum           # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
int     nulval          # i value for undefined pi
int     array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsvj(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  nulval,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsve(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  nulval,array,anyflg,status)

# Read a subsection of real values from the primary array.

int     iunit           # i input file pointer
int     colnum           # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
real    nulval          # i value for undefined pi
real    array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsve(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  nulval,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsvd(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  nulval,array,anyflg,status)

# Read a subsection of double precision values from the primary array.
int     iunit           # i input file pointer
int     colnum           # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
double  nulval          # i value for undefined pi
double  array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsvd(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                   nulval,array,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsfb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)
# Read a subsection of byte values from the primary array.

int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsfi(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                   array,flgval,anyflg,status)

# Read a subsection of Integer*2 values from the primary array.

int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
short   array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfi(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsfj(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)

# Read a subsection of integer values from the primary array.

int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfj(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  array,flgval,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsfe(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  array,flgval,anyflg,status)

# Read a subsection of real values from the primary array.

int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
real    array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfe(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  array,flgval,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsgsfd(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  array,flgval,anyflg,status)

# Read a subsection of double precision values from the primary array.
int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
double  array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfd(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                  array,flgval,anyflg,status)
end
#-------------------------------------------------------------------
procedure fsdtyp(value,dtype,status)

# determine datatype of a FITS value field
# This assumes value field conforms to FITS standards and may not
#    detect all invalid formats.
# value   c  input value field from FITS header record only,
#            (usually the value field is in columns 11-30 of record)
#            The value string is left justified.
# dtype   c  output type (C,L,I,F) for Character string, Logical,
#              Integer, Floating point, respectively

char    value[SZ_FSTRVAL]          # i data value
%       character*70 fvalue
char    dtype           # o datatype code
%       character*1 fdtype
int     status          # o error status
char    sdtype[1]
begin

call f77pak(value,fvalue,SZ_FSTRVAL)
call ftdtyp(fvalue,fdtype,status)
call f77upk(fdtype,sdtype,1)
dtype=sdtype[1]
end
#-------------------------------------------------------------------
procedure fsgcnn(iunit,exact,colnam,realnm,colnum,status)

# determine the column number corresponding to an input column name.

int     iunit           # i input file pointer
bool    exact           # i require same case?
char    colnam[SZ_FTTYPE]     # i column name template
%       character fcolna*24
char    realnm[SZ_FTTYPE]     # o column name
%       character frealn*24
int     colnum          # o column number
int     status          # o error status

begin

call f77pak(colnam,fcolna,SZ_FTTYPE)
call ftgcnn(iunit,exact,fcolna,frealn,colnum,status)
call f77upk(frealn,realnm,SZ_FTTYPE)
end
#-------------------------------------------------------------------
procedure fscmps(templ,strng,casesn,match,exact)

char    templ[SZ_FTTYPE]     # i column name template
%       character ftemp*24
char    strng[SZ_FTTYPE]     # i column name 
%       character fstrng*24
bool    casesn           # i require same case?
bool    match            # o do the strings match?
bool    exact            # o is it an exact match?

begin

call f77pak(templ,ftemp,SZ_FTTYPE)
call f77pak(strng,fstrng,SZ_FTTYPE)
call ftcmps(ftemp,fstrng,casesn,match,exact)
end
#-------------------------------------------------------------------
procedure fspcks(iunit,status)

int     iunit          
int     status          # o error status

begin

call ftpcks(iunit,status)
end
#-------------------------------------------------------------------
procedure fsucks(iunit,status)

int     iunit          
int     status          # o error status

begin

call ftucks(iunit,status)
end
#-------------------------------------------------------------------
procedure fsvcks(iunit,dataok,hduok,status)

int     iunit          
int     dataok
int     hduok
int     status          # o error status

begin

call ftvcks(iunit,dataok,hduok,status)
end
#-------------------------------------------------------------------
procedure fsgcks(iunit,datasum,hdusum,status)

int     iunit          
double  datasum
double  hdusum
int     status          # o error status

begin

call ftgcks(iunit,datasum,hdusum,status)
end
#-------------------------------------------------------------------
procedure fsesum(sum,comp,chksum)

double  sum          
bool    comp
char    chksum[16]
%       character fsum*16

begin

call ftesum(sum,comp,fsum)
call f77upk(fsum,chksum,16)
end
#-------------------------------------------------------------------
procedure fsdsum(chksum,comp,sum)

char    chksum[16]
bool    comp
double  sum          
%       character fsum*16

begin

call f77pak(chksum,fsum,16)
call ftdsum(fsum,comp,sum)
end
#-------------------------------------------------------------------
procedure fsgics(iunit,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,status)

int     iunit          
double  xrval,yrval,xrpix,yrpix,xinc,yinc,rot
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call ftgics(iunit,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,fcoord,status)
call f77upk(fcoord,coord,4)

end
#-------------------------------------------------------------------
procedure fsgtcs(iunit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,
           rot,coord,status)

int     iunit,xcol,ycol
double  xrval,yrval,xrpix,yrpix,xinc,yinc,rot
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call ftgtcs(iunit,xcol,ycol,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,
            fcoord,status)
call f77upk(fcoord,coord,4)

end
#-------------------------------------------------------------------
procedure fswldp(xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,
                 xpos,ypos,status) 

double  xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,xpos,ypos
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call f77pak(coord,fcoord,4)
call ftwldp(xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,fcoord,
             xpos,ypos,status)

end
#-------------------------------------------------------------------
procedure fsxypx(xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,coord,
                xpix,ypix,status)

double  xpix,ypix,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,xpos,ypos
char    coord[4]
%       character fcoord*4
int     status          # o error status

begin

call f77pak(coord,fcoord,4)
call ftxypx(xpos,ypos,xrval,yrval,xrpix,yrpix,xinc,yinc,rot,fcoord,
           xpix,ypix,status)

end
#-------------------------------------------------------------------
procedure fsgtcl(iunit,colnum,tcode,rpeat,wdth,status)

int     iunit,colnum,tcode,rpeat,wdth
int     status          # o error status

begin

call ftgtcl(iunit,colnum,tcode,rpeat,wdth,status)

end
#-------------------------------------------------------------------
procedure fsasfm(tform,code,width,decims,status)

char    tform[SZ_FTTYPE]
%       character ftform*24
int     code,width,decims
int     status          # o error status

begin

call f77pak(tform,ftform,4)
call ftasfm(ftform,code,width,decims,status)

end
