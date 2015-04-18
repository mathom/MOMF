	program tfit

	parameter (NDIM=2048)
	integer*2 image(1:NDIM,1:NDIM), image2(-10:10,-10:10)
	character*80 name
c
	integer nx,ny

	common image,image2

	name = '/users/tmp/srf/hr1217/a0300380.fits'

	call readheader(name,nx,ny)
	print 100,nx,ny
  100	format('Image dimensions',2i6)

	call readimage(name)
	print 200,(image(i,1),i=1,10)
  200	format('Image: ',10i6)


	end

	include 'fitsinput.f'
