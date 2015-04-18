         program MOMF
c
c
c        Dcoor2.f
c
c        Hans Kjeldsen - MOMF version 3.0
c        Aarhus University 1994 - Denmark
c     
c---------------------------------------------
c        First-version: DAOcoor2.f
c---------------------------------------------
c        Hans Kjeldsen - MOMF version 2.1
c        Aarhus University 1992 - Denmark
c---------------------------------------------
c
         parameter (NSTAR = 50000)

         dimension coo(1:4,1:NSTAR)
c
c Software structure same as for Dcoor.f - see comments in
c this sourcefile...
c

c
c FORMAT - information for frame
c

         open(10,file='FORMAT',status='old')
 
         read(10,*)COL
         read(10,*)ROW
         read(10,*)pfix
         read(10,*)BIASCMIN
         read(10,*)BIASCMAX
         read(10,*)BIASRMIN
         read(10,*)BIASRMAX
 
         close(10)

c
c Limits on offset (max from frame to frame..)
c

         write(6,*)'Maximum-size of offset (max(|dx|,|dy|)):'
         read(5,*)xmoff
 
         nxoff=int(xmoff+2.0)
         write(6,*)nxoff
 
c        nx01=nxoff+14
c        nx02=nxoff+13
         nx01=nxoff+3
         nx02=nxoff+2

c
c Bias-limits - coordinate-control
c
 
         if (BIASCMIN+BIASCMAX-COL) 301,301,302
301      mcol1=nx01+nint(BIASCMAX)
         mcol2=nint(COL)-nx02
         go to 303
302      mcol1=nx01
         mcol2=nint(BIASCMIN)-nx02
303      continue
 
         if (BIASRMIN+BIASRMAX-ROW) 311,311,312
311      mrow1=nx01+nint(BIASRMAX)
         mrow2=nint(ROW)-nx02
         go to 313
312      mrow1=nx01
         mrow2=nint(BIASRMIN)-nx02
313      continue
 
         write(6,*)'COL: ',mcol1,mcol2
         write(6,*)'ROW: ',mrow1,mrow2
 
         open(10,file='Dcoo.coo',status='old')
         open(11,file='PSFstars',status='unknown')
         open(23,file='fwhm.SUM',status='old')
 
         read(23,*)fxx,fxx
         close(23)
c
c FWHM / Seeing information for frame..
c
c

         fxx=(fxx*0.6005)**2
 
         read(10,'(a10)')line
         read(10,'(a10)')line
         read(10,'(a10)')line
 
         i=1
 
10       continue
         read(10,*,end=20)x,coo(1,i),coo(2,i),coo(3,i)
         coo(4,i)=10**(-0.4*coo(3,i))
         i=i+1
         if (i.gt.NSTAR) then
	   print *, 'Too many stars'
           stop
         endif
         go to 10
20       continue
 
         i=i-1
 
         write(6,*)'Number of stars: ',i
 
         do 50 ni=1,i,1
     
            if (coo(1,ni)-float(mcol1))50,50,31
31          if (coo(1,ni)-float(mcol2))32,50,50
32          if (coo(2,ni)-float(mrow1))50,50,33
33          if (coo(2,ni)-float(mrow2))34,50,50
34          if (coo(3,ni)+1.0)35,50,50
35          zz=1001.0*coo(4,ni)
            do 40 nj=1,i,1
               dist=-(coo(1,ni)-coo(1,nj))**2-(coo(2,ni)-coo(2,nj))**2
               dist=exp(dist/fxx)
               zz=zz-1000.0*dist*coo(4,nj)
40          continue
            zz=-zz
         write(11,666)zz,coo(4,ni),coo(1,ni),coo(2,ni),10.0+coo(3,ni),ni
666            format(2f16.3,2f10.2,f13.3,i10)
 
50       continue
 
         close(10)
         close(11)
 
         end
