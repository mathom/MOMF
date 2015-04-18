         program MOMF
c
c
c        Dnew.f
c
c        Hans Kjeldsen - MOMF version 3.0
c        Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c        First-version: DAOnew.f
c---------------------------------------------
c        Hans Kjeldsen - MOMF version 2.1
c        Aarhus University 1992 - Denmark
c---------------------------------------------
c

c
         parameter (NDIM=4096)
         character*80 name
         integer*2 image(1:NDIM,1:NDIM)
	 integer calcsize

c
c FORMAT-information...
c
 
         open(10,file='FORMAT',status='old')
 
         read(10,*)COL
         read(10,*)ROW
         read(10,*)pfix
         read(10,*)BIASCMIN
         read(10,*)BIASCMAX
         read(10,*)BIASRMIN
         read(10,*)BIASRMAX
         read(10,*)iscale
 
         close(10)
 
         mmx=nint(COL)
         mmy=nint(ROW)
         ncmin=nint(BIASCMIN)
         ncmax=nint(BIASCMAX)
         nrmin=nint(BIASRMIN)
         nrmax=nint(BIASRMAX)
       
c  this definition is for SGI, where it is given in words = 4 bytes
c  for direct, unformatted files
         Nrecl=calcsize(mmx,mmy)
 
         write(6,*)'Do you have any Bad-pixel image ? (1=yes,0=no)'
         read(5,*)nnn
         if (nnn) 11,11,12
11       continue
 
         write(6,*)'Making Bad-pixel image'
         do 112 i=1,mmx,1
            do 111 j=1,mmy,1
               image(i,j)=1
111         continue
112      continue
  
         go to 13
 
12       continue
 
         write(6,*)'What is the name of the Bad-pixel image ?'
         read(5,'(a80)')name
 
         open (10,file=name,access='direct',
     c          recl=Nrecl,status='old')
 
         write(6,*)'Reading Bad-pixel image'
         read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
         close(10)
 
13       continue
 
               if (ncmin) 125,125,120
120            if (ncmax) 125,125,121
121            do 123 ii=ncmin,ncmax,1
                  write(6,*)'BIAS-column:',ii
                  do 122 jj=1,mmy,1
                     image(ii,jj)=0
122               continue
123            continue
125            if (nrmin) 131,131,126
126            if (nrmax) 131,131,127
127            do 129 jj=nrmin,nrmax,1
                  write(6,*)'BIAS-row:   ',jj
                  do 128 ii=1,mmx,1
                     image(ii,jj)=0
128               continue
129            continue
131         continue
  
         open (10,file='im.ZERO',access='direct',
     c          recl=Nrecl,status='unknown')
 
         write(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
         close(10)
 
         write(6,*)'making: im.SUM (x,y) = 0'
         do 62 i=1,mmx,1
            do 61 j=1,mmy,1
               image(i,j)=0
61          continue
62       continue
  
         open (10,file='im.SUM',access='direct',
     c          recl=Nrecl,status='unknown')
 
         write(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
         close(10)
 
         write(6,*)'making: im.STAT (x,y) = 0'
         open (10,file='im.STAT',access='direct',
     c          recl=Nrecl,status='unknown')
 
         write(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
         close(10)
 
         write(6,*)'making: factor.SUM = 0.0'
         open (11,file='factor.SUM',status='unknown')
         write(11,*)0.0
         write(11,*)0.0
         close(11)
 
         end

	include '../../common/calcsize.f'
