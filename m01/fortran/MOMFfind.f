       program MOMF
c
c
c      MOMFfind.f
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
c
       parameter (NDIM = 4096)
       parameter (NSTARS = 50000)

       integer*2 image(1:NDIM,1:NDIM),iempty(1:NDIM,1:NDIM)
       common image, iempty
       character*80 name
       dimension xold1(NSTARS),yold1(NSTARS)
       integer calcsize
 
 
c----------------------------------------------------------------------
c      Basic operations........
c----------------------------------------------------------------------
 
       iem=0
       ima=0
       nfind=0

       coomin=0.0
c
c Reading data from basic file: imname
c
       open(10,file='imname',status='old')
 
       read(10,'(a80)')name
       read(10,*)seemax
 
       ndd=0
 
       read(10,*)sigma
       read(10,*)isflag
 
       write(6,'(a80,f15.5,f15.5,i5)')name,seemax,sigma,isflag
       close(10)
 
       write(6,*)'Input file: ',name

c
c Reading information on the frame-format
c

       open(10,file='FORMAT',status='old')
 
       read(10,*)COL
       read(10,*)ROW
       read(10,*)pfix
       do i=1,4
	 read(10,*) idum
       enddo
       read(10,*) iscale
       close(10)
c
c  set iscale to zero if second round of FIND
c
       iscale = iscale*isflag
 
       mmx=nint(COL)
       mmy=nint(ROW)

c
c Size of 2-byte frame in direct-mode (binary-format)
c
c   SGI convention defines record length given in words = 4 bytes
       Nrecl=calcsize(mmx,mmy)
 
       call rimage(name,Nrecl,image,mmx,mmy,iscale)
c
c         open (10,file=name,access='direct',
c    c          recl=Nrecl,status='old')
c
c         read(10,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
c         close(10)
    
          write(6,*)'Data read: ',name

c
c Open file for coordinate-output
c
       open(41,file='MOMFfind.coo',status='unknown')

c
c Init. reference-frame for control of the find algorithm
c
       do 10 i=1,mmx,1
          do 5 j=1,mmy,1
             iempty(i,j)=1
5         continue
10     continue
 
       write(6,*)'iempty(i,j)=1'
 
c----------------------------------------------------------------------
c      FIND ::
c----------------------------------------------------------------------
 
c
c Total direct FIND-routine. 3 step check
c

c
c Check I: local maximum
c
c
       do 2010 i=3,(mmx-3),1
          do 2005 j=3,(mmy-3),1

c Local check of maximum
c
	     do 101 id=(i-1),(i+1),1
	        do 100 jd=(j-1),(j+1),1
                   
		   if (image(i,j)-image(id,jd)) 2005,100,100

100             continue
101          continue

c
c Check I: OK
c
	     iem=iem+1

c
c Check II: Sigma detection
c
c Determination of SKY-param, mean and scatter
c

c set counters
c
	     cmu=0.0
	     csigma=0.0
	     ncc=0

	     do 201 id=max(1,(i-15)),min(mmx,(i+15)),1
	        do 200 jd=max(1,(j-15)),min(mmy,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 200,199,199

199                continue
		   cmu=cmu+image(id,jd)
		   ncc=ncc+1

200             continue
201          continue
 
	     cmu=cmu/float(ncc)

	     do 211 id=max(1,(i-15)),min(mmx,(i+15)),1
	        do 210 jd=max(1,(j-15)),min(mmy,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 210,209,209

209                continue
		   csigma=csigma-(image(id,jd)-cmu)**2

210             continue
211          continue
 
	     csigma=csigma/float(ncc)
c
c Init. counter for filter - Gaussian
c

	     do 320 jkk=1,2,1

c Double check - 320 continue
c
	     c2mu=0.0
	     c2sigma=0.0
	     c2c=0.0

	     do 301 id=max(1,(i-15)),min(mmx,(i+15)),1
	        do 300 jd=max(1,(j-15)),min(mmy,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 300,299,299

299                continue
		   xloc=exp(((image(id,jd)-cmu)**2)/csigma)
		   c2mu=c2mu+image(id,jd)*xloc
		   c2c=c2c+xloc

300             continue
301          continue
 
	     cmu=c2mu/c2c
	     c2c=0.0

	     do 311 id=max(1,(i-15)),min(mmx,(i+15)),1
	        do 310 jd=max(1,(j-15)),min(mmy,(j+15)),1
                   
		   rad=(float(id-i))**2+(float(jd-j))**2
		   if (rad-70.0) 310,309,309

309                continue
		   xloc=exp(((image(id,jd)-cmu)**2)/csigma)
		   c2c=c2c+xloc
		   c2sigma=c2sigma-xloc*(image(id,jd)-cmu)**2

310             continue
311          continue
 
	     csigma=c2sigma/c2c
c
c Double check....
c
320          continue
 
	     csigma=sqrt(-1.0*csigma)


c Sky mean and scatter calculated
c
c direct check II:
 
	     sum1=0.0
	     sum2=0.0

	     do 401 id=(i-1),(i+1),1
	        do 400 jd=(j-1),(j+1),1
                   
		   sum1=sum1+image(id,jd)

400             continue
401          continue

	     do 411 id=(i-2),(i+2),1
	        do 410 jd=(j-2),(j+2),1
                   
		   sum2=sum2+image(id,jd)

410             continue
411          continue

	     sum2=9.0*((sum2-sum1)/16.0)

	     sum1=(sum1-sum2)/3.0
	      
	     csigma=csigma*sigma

c
c Check II: sigma-check
c
	
	     if (csigma-sum1) 450,2005,2005
	      
450          continue
 
	     ima=ima+1

c
c End: check II
c

c coordinate and magnitude calculation
c & Check III:
c

	     Ssx=0.0
	     Ssy=0.0
	     Ssw=0.0

	     do 611 id=max(1,(i-5)),min(mmx,(i+5)),1
	        do 610 jd=max(1,(j-5)),min(mmy,(j+5)),1
                   
		   rad=-0.3*(float(id-i))**2-0.3*(float(jd-j))**2
		   rad=exp(rad)

		   Ssx=Ssx+(float(image(id,jd))-cmu)*rad*float(id)
		   Ssy=Ssy+(float(image(id,jd))-cmu)*rad*float(jd)
		   Ssw=Ssw+(float(image(id,jd))-cmu)*rad

610             continue
611          continue

	     if (Ssw) 2005,2005,612

612          continue
 
	     do 712 iimp=1,5,1

	     xcoo=Ssx/Ssw
	     ycoo=Ssy/Ssw

	     Ssx=0.0
	     Ssy=0.0
	     Ssw=0.0

	     do 711 id=max(1,(i-5)),min(mmx,(i+5)),1
	        do 710 jd=max(1,(j-5)),min(mmy,(j+5)),1
                   
		   rad=-0.3*(float(id)-xcoo)**2-0.3*(float(jd)-ycoo)**2
		   rad=exp(rad)

		   Ssx=Ssx+(float(image(id,jd))-cmu)*rad*float(id)
		   Ssy=Ssy+(float(image(id,jd))-cmu)*rad*float(jd)
		   Ssw=Ssw+(float(image(id,jd))-cmu)*rad

710             continue
711          continue

	     if (Ssw) 2005,2005,712

712          continue

	     xcoo=Ssx/Ssw
	     ycoo=Ssy/Ssw

	     xmag=20.0-2.5*log10(Ssw)

	     id=nint(xcoo)
	     jd=nint(ycoo)
	      
             if (iempty(id,jd)) 2005,2005,630
	      
630          iempty(id,jd)=0
	     xm0=2.5*log10(1.0+csigma/Ssw)
	     sky=cmu
	     ndd=(i-id)**2+(j-jd)**2

             do 5131 nw=1,nfind,1
                ddi = (xcoo-xold1(nw))**2 +(ycoo-yold1(nw))**2
                if (ddi-1.5) 20041,20041,5131
5131         continue

	     nfind=nfind+1

             xold1(nfind)=xcoo
             yold1(nfind)=ycoo
 
       write(6,5141)i,j,nfind,ima,iem,xcoo,ycoo,xmag,xm0,sky,ndd
       write(41,5141)i,j,nfind,ima,iem,xcoo,ycoo,xmag,xm0,sky,ndd
 5141     format(4i6,i6,2f8.2,2f9.4,f8.2,i6)
c   old format: 5141            format(4i6,i8,2f9.2,2f9.4,f10.2,i8)
 
          if (xmag.gt.20.0) goto 2004
	  if (coomin-xmag) 2003,2004,2004
 2003         coomin=xmag
 2004      continue

20041     continue

c 
 

2005      continue
2010   continue

       close(41)
 
c
c Making output for cont. MOMF-routines
c
 
          open (41,file='Dcoo.fit',access='direct',
     c          recl=Nrecl,status='unknown')
 
          write(41,rec=1) ((image(i,j),i=1,mmx),j=1,mmy)
          close(41)
 
       open(42,file='coomin',status='unknown')
       write(42,'(f12.5)')coomin
       close(42)
    
       end

       include '../../common/rimage.f'
       include '../../fits/fitsinput.f'
       include '../../common/calcsize.f'
