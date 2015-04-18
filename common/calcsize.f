       integer function calcsize(mmx,mmy)

c       for SGI
c        calcsize = 2*mmx*mmy/4+1
c       for others
       calcsize = 2*mmx*mmy
c        calcsize = 2
c       write (*,*) 'calcsize',calcsize,mmx,mmy
       return
       end
