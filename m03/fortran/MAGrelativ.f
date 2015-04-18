       program MOMF
c
c
c      MAGrelativ.f
c
c      Hans Kjeldsen - MOMF version 3.0
c      Aarhus University 1994 - Denmark
c      07.02.1994
c
c      This version modified by SF 
c      26.08.1997
c
c---------------------------------------------------------
c

       dimension PHOT(1:5000,1:7)
       character*60 name
       dimension SIGE(2:7), SIGI(2:7), XMEA(2:7)
       dimension DV(2:7), NNN1(2:7), NNN2(2:7)
       dimension vv1(1:5000), vv2(1:5000), vv3(1:5000)
       dimension vv4(1:5000), vv5(1:5000), vv6(1:5000)
       integer rflag
 
c
c  Read file with information about reference stars
c  
       rflag = 0
       open(12,file='MOMF.ref',status='old',err=9)
       read(12,*) rflag
       print 888
888    format('Reference stars selected')
    9  continue

       open(10,file='starfile',status='old')
       open(20,file='starscatter',status='old')
       open(31,file='scatter',status='unknown')
       open(32,file='stat.V',status='unknown')
 
       n=1
10     continue
 
          read(10,'(a60)',end=200)name
          open(11,file=name,status='old')
      
          read(20,'(a60)')name
          open(21,file=name,status='unknown')
      
          ni=1
 
15        continue

             read(11,*,end=16) (PHOT(ni,j),j=1,7)
             ni=ni+1
 
          go to 15
 
16        continue
        
          close(11)
 
          ni=ni-1
 
          do 40 j=2,7,1
          
             nantal=0
             sum=0.0
             sum2=0.0
 
             do 18 i=1,ni,1
                if (PHOT(i,j)-99.9) 17,18,18
17              nantal=nantal+1
                sum=sum+PHOT(i,j)
18           continue
 
             if (nantal-1) 26,19,19
19              sum=sum/float(nantal)
             if (nantal-2) 25,20,20
20           do 202 i=1,ni,1
                if (PHOT(i,j)-99.9) 201,202,202
201             sum2=sum2+(PHOT(i,j)-sum)**2
202          continue
                sigma=sum2/float(nantal)
                if (sigma) 21,21,22
21                 sigma=0.0
22                 sigma=sqrt(sigma)
                   go to 27
25              sigma=99.9999
                go to 27
26              sigma=99.9999
                sum=99.9999
27           continue
    
             nantal2=0
             scat=0.0
             scat2=0.0
 
             do 31 i=2,ni,1
                if (PHOT(i,j)-99.9) 28,31,31
28              if (PHOT((i-1),j)-99.9) 29,31,31
29              nantal2=nantal2+1
                scat=scat+(PHOT(i,j)-PHOT((i-1),j))
                scat2=scat2+(PHOT(i,j)-PHOT((i-1),j))**2
31           continue
 
             if (nantal2-2) 35,32,32
32              scat=scat/float(nantal2)
                scat2=scat2/float(nantal2)
                sigmaint=scat2-scat**2
                if (sigmaint) 33,33,34
33                 sigmaint=0.0
                   go to 36
34                 sigmaint=sqrt(sigmaint/2.0)
                   dval=sigma/sigmaint
                   go to 37
35              sigmaint=99.9999
36              dval=99.9999
37           continue
 
             SIGE(j)=sigma
             SIGI(j)=sigmaint
             XMEA(j)=sum
             DV(j)=dval
             NNN1(j)=nantal
             NNN2(j)=nantal2
 
40        continue
 
c         write(6,777)n,1,XMEA(2),NNN1(2),SIGE(2),NNN2(2),SIGI(2),DV(2)
c         write(6,778)2,XMEA(3),NNN1(3),SIGE(3),NNN2(3),SIGI(3),DV(3)
c         write(6,778)3,XMEA(4),NNN1(4),SIGE(4),NNN2(4),SIGI(4),DV(4)
c         write(6,778)4,XMEA(5),NNN1(5),SIGE(5),NNN2(5),SIGI(5),DV(5)
c         write(6,778)5,XMEA(6),NNN1(6),SIGE(6),NNN2(6),SIGI(6),DV(6)
c         write(6,778)6,XMEA(7),NNN1(7),SIGE(7),NNN2(7),SIGI(7),DV(7)
c         write(6,'(a30)')' '
          write(31,777)n,1,XMEA(2),NNN1(2),SIGE(2),NNN2(2),SIGI(2),DV(2)
          write(31,778)2,XMEA(3),NNN1(3),SIGE(3),NNN2(3),SIGI(3),DV(3)
          write(31,778)3,XMEA(4),NNN1(4),SIGE(4),NNN2(4),SIGI(4),DV(4)
          write(31,778)4,XMEA(5),NNN1(5),SIGE(5),NNN2(5),SIGI(5),DV(5)
          write(31,778)5,XMEA(6),NNN1(6),SIGE(6),NNN2(6),SIGI(6),DV(6)
          write(31,778)6,XMEA(7),NNN1(7),SIGE(7),NNN2(7),SIGI(7),DV(7)
          write(31,'(a30)')' '
777          format(i7,i6,f11.5,i7,f11.5,i7,2f11.5)
778          format(i13,f11.5,i7,f11.5,i7,2f11.5)
c
c  If no reference stars are chosen, suppress the multitude of faint
c  stars by not using the variance, but a higher power of sigma
c  when calculating weights
c
          npow = 3
          if (rflag.gt.0) npow=1
          if (n.eq.1) print 899, npow
899       format('Power used in weight:',i5)
          if (rflag) 7780,7780,7781
 7780     vv1(n)=0.0000001*float(max(0,(1+NNN1(2)-ni)))/(SIGE(2)**npow)
          vv2(n)=0.0000001*float(max(0,(1+NNN1(3)-ni)))/(SIGE(3)**npow)
          vv3(n)=0.0000001*float(max(0,(1+NNN1(4)-ni)))/(SIGE(4)**npow)
          vv4(n)=0.0000001*float(max(0,(1+NNN1(5)-ni)))/(SIGE(5)**npow)
          vv5(n)=0.0000001*float(max(0,(1+NNN1(6)-ni)))/(SIGE(6)**npow)
          vv6(n)=0.0000001*float(max(0,(1+NNN1(7)-ni)))/(SIGE(7)**npow)
          go to 7782

 7781     vv1(n)=0.0001*float(max(0,(1+NNN1(2)-ni)))/(SIGE(2))
          vv2(n)=0.0001*float(max(0,(1+NNN1(3)-ni)))/(SIGE(3))
          vv3(n)=0.0001*float(max(0,(1+NNN1(4)-ni)))/(SIGE(4))
          vv4(n)=0.0001*float(max(0,(1+NNN1(5)-ni)))/(SIGE(5))
          vv5(n)=0.0001*float(max(0,(1+NNN1(6)-ni)))/(SIGE(6))
          vv6(n)=0.0001*float(max(0,(1+NNN1(7)-ni)))/(SIGE(7))

 7782     do 59 i=1,ni,1

             x1=PHOT(i,1)
             x2=99.9999
             x3=99.9999
             x4=99.9999
             x5=99.9999
             x6=99.9999
             x7=99.9999
             x8=0.0
             x9=0.0
 
             if (PHOT(i,2)-99.9) 47,48,48
47           x2=(PHOT(i,2)-XMEA(2))/SIGI(2)
             x8=x8+x2
             x9=x9+1.0
48           if (PHOT(i,3)-99.9) 49,50,50
49           x3=(PHOT(i,3)-XMEA(3))/SIGI(3)
             x8=x8+x3
             x9=x9+1.0
50           if (PHOT(i,4)-99.9) 51,52,52
51           x4=(PHOT(i,4)-XMEA(4))/SIGI(4)
             x8=x8+x4
             x9=x9+1.0
52           if (PHOT(i,5)-99.9) 53,54,54
53           x5=(PHOT(i,5)-XMEA(5))/SIGI(5)
             x8=x8+x5
             x9=x9+1.0
54           if (PHOT(i,6)-99.9) 55,56,56
55           x6=(PHOT(i,6)-XMEA(6))/SIGI(6)
             x8=x8+x6
             x9=x9+1.0
56           if (PHOT(i,7)-99.9) 57,58,58
57           x7=(PHOT(i,7)-XMEA(7))/SIGI(7)
             x8=x8+x7
             x9=x9+1.0
58           continue
 
             if (x9-0.5) 64,64,65
64           x8=99.9999
             go to 66
65           x8=x8/x9
66           continue
             write(21,779)nint(x1),x2,x3,x4,x5,x6,x7,x8
779             format(i9,7f10.5)
          
59        continue
 
          close(21)
          close(22)
 
          n=n+1
 
       go to 10
 
200    continue
 
       n=n-1
c
c  if reference stars specified do not modify weights
c
       if (rflag.gt.0) goto 255 
       do 250 j=1,3,1
 
          sum1=0.0
          sum2=0.0
          sum3=0.0
          sum4=0.0
          sum5=0.0
          sum6=0.0
 
          do 210 i=1,n,1
 
             sum1=sum1+vv1(i)
             sum2=sum2+vv2(i)
             sum3=sum3+vv3(i)
             sum4=sum4+vv4(i)
             sum5=sum5+vv5(i)
             sum6=sum6+vv6(i)
 
210       continue
 
          sum1=sum1/6.0
          sum2=sum2/6.0
          sum3=sum3/6.0
          sum4=sum4/6.0
          sum5=sum5/6.0
          sum6=sum6/6.0
 
          do 240 i=1,n,1
 
             if (vv1(i)-sum1)212,212,211
211          vv1(i)=sum1
212          if (vv2(i)-sum2)214,214,213
213          vv2(i)=sum2
214          if (vv3(i)-sum3)216,216,215
215          vv3(i)=sum3
216          if (vv4(i)-sum4)218,218,217
217          vv4(i)=sum4
218          if (vv5(i)-sum5)220,220,219
219          vv5(i)=sum5
220          if (vv6(i)-sum6)240,240,221
221          vv6(i)=sum6
 
240       continue
 
250    continue
 
255    do 260 i=1,n,1
 
       write(32,783)i,vv1(i),vv2(i),vv3(i),vv4(i),vv5(i),vv6(i)
783       format(i10,6f16.7)
 
260    continue
 
       close(10)
       close(20)
       close(31)
       close(32)
 
       end
