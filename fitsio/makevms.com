$ ! Command file to build the FITSIO library on a VAX or Alpha VMS system
$ !   written by William Pence, January 1997
$ ! 
$ if (F$GETSYI("ARCH_NAME") .eqs. "VAX") then goto VAX
$ ! 
$ !  This section builds FITSIO on an Alpha OpenVMS system
$ ! 
$    write sys$output "Building Alpha OpenVMS version of FITSIO..."
$    set verify
$    fortran fitsfort.f
$    fortran fitsio.f
$    fortran fitsalphavms.f
$    cc vmsieee.c
$    lib/create fitsio fitsio,fitsfort,fitsalphavms,vmsieee
$    set noverify
$    exit
$ ! 
$ VAX:
$ ! 
$ !  This section builds FITSIO on a VAX/VMS system
$ ! 
$    write sys$output "Building VAX/VMS version of FITSIO..."
$    set verify
$    fortran fitsfort.f
$    fortran fitsio.f
$    fortran fitsvax.f
$    macro vmsieeed.mar
$    lib/create fitsio fitsio,fitsfort,fitsvax,vmsieeed
$    set noverify
$    exit
