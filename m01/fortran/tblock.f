	program tbl

        parameter (NDIM=4096)
        integer*2 image(1:NDIM,1:NDIM)
        character*80 name
	integer calcsize

	mmy=512
	mmx=512
        Nrecl = calcsize(mmx,mmy)
        name='/users/tmp/srf/teide/reduce/im16_26.b'
        iscale = 0
        call rimage(name,Nrecl,image,mmx,mmy,iscale)
        write(6,*)'Data read: ',name

	end

       include '../../common/rimage.f'
       include '../../fits/fitsinput.f'
       include '../../common/calcsize.f'
