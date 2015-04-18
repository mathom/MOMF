       program MOMF
c
c
c      MOMFname.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c      First-version: 
c---------------------------------------------
c      Hans Kjeldsen - MOMF version 2.1
c      Aarhus University 1992 - Denmark
c---------------------------------------------
c  ----------------------------------
c      Hans Kjeldsen
c      Aarhus University 1990-1992
c      Denmark
c  ----------------------------------
c

c
c Defnr.
c
       character*60 name,value
	
c 
c This small program {MOMFname} is used to generate input to the
c main FIND-routine {MOMFfind}. This structure is used in order to
c give all FIND-routines in different version of MOMF the same 
c structure.
c
 
c 
c File with generated data: imname
c
       open(10,file='imname',status='unknown')
	
c
c File with input for other routines
c
       open(11,file='Dstars3',status='unknown')

c
c Date...
c
       call getenv('MOMF_DATAIN',value)
       lin = index(value, ' ') - 1

       open(12,file='input.SUM',status='old')
       read(12,'(a60)')name
       close(12)
       write(10,'(a60)')value(:lin)//'/'//name

       write(6,*)'Approx. seeing in pixels:'
       read(5,*)seeing
       write(10,'(f15.5)')seeing
       write(11,'(f15.5)')seeing

       write(6,*)'Detection-level (e.g. 8)'
       read(5,*)sigma
       write(10,'(f15.5)')sigma
c
c  enable scaling
c
       isflag = 1
       write(10,'(i5)') isflag

       close(10)
       close(11)

c
c End of program: MOMFname
c
 
       end
