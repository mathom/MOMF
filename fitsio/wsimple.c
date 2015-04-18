/*        program wsimpl

C       A simple program to write a primary array and a binary table extension.
C       This is designed to illustrate the basic sequence of FITSIO calls
C       needed to write a new FITS file.
*/

#include "cfitsio.h"

int MAIN_;

main ()
{
  int iunit, status, bitpix, naxis, naxes[99], pcount, gcount;
  int group, fpixel, nelem, i, jvals[3];
  int ivalue[20], rowlen, nrows, tfield, vardat, tbcol[2], colnum, frow, felem;
  int FitsStrBufLen;
  float evals[3];
  int simple, extend;
  char errtxt[FITS_CLEN_ERRMSG];
  static char aform[][FITS_CLEN_HDEFKWDS] =
  {"I6", "F8.2","A8"};
  static char tform[][FITS_CLEN_HDEFKWDS] =
  {"J", "E", "8A"};
  static char ttype[][FITS_CLEN_HDEFKWDS] =
  {"IntVal", "RealVal","StringVal"};
  static char tunit[][FITS_CLEN_HDEFKWDS] =
  {"cm", "kg", " "};
  static char svalues[][7] =
  {"Vega", "Sirius", "Orion"};
  static char *extnam = "SIMPLETEST";

  status = 0;
  iunit = 15;

  /*C       open the new FITS file*/
  FCINIT(iunit, "csimple.fit", 2880, &status);
    FCGERR(status,errtxt);
    printf ("fcinit status = %d : %s\n", status, errtxt);

  /* C       write a 2D array 10 x 2 pixels in size*/
  simple = 1;
  bitpix = 32;
  naxis = 2;
  naxes[0] = 10;
  naxes[1] = 2;
  pcount = 0;
  gcount = 1;
  extend = 1;
  jvals[0] = 66;
  jvals[1] = 67;
  jvals[2] = 68;
  evals[0] = 1000.;
  evals[1] = 1001.;
  evals[2] = 1002.;

  /*C       write the required primary array keywords */
  FCPHPR (iunit, simple, bitpix, naxis, naxes, pcount, gcount, extend, &status);
    FCGERR(status,errtxt);
    printf ("fcphpr status = %d : %s\n", status, errtxt);

  /* C      write an additional integer keyword and a comment and history
   keyword: */
  FCPKYJ (iunit, "jkey", -35, "The value is -35", &status);
  FCPCOM (iunit, "This was written by FCPCOM", &status);
  FCPHIS (iunit, "This was written by FCPHIS", &status);

  /*C       set the values for the primary array*/
  nelem = naxes[0] * naxes[1];
  for (i = 0; i < nelem; i++)
    ivalue[i] = i +1 + 64;

  /*C       define the primary array structure */
  FCPDEF (iunit, bitpix, naxis, naxes, pcount, gcount, &status);
    FCGERR(status,errtxt);
    printf ("fcpdef status = %d : %s\n", status, errtxt);

  /*C       write the primary array of data */
  group = 1;
  fpixel = 1;
  FCPPRJ (iunit, group, fpixel, nelem, ivalue, &status);
  /*
C       now create an ASCII table extension
C       first, create a new empty extension
*/
  FCCRHD (iunit, &status);
    FCGERR(status,errtxt);
    printf ("fccrhd status = %d : %s\n", status, errtxt);

  /* C  table will have 3 rows and 3 columns */
  nrows = 3;
  tfield = 3;
  rowlen = 22;
  tbcol[0]=1;
  tbcol[1]=7;
  tbcol[2]=15;

  /* C initialize the FITSIO parameters defining the structure of the table */
  FCADEF (iunit, rowlen, tfield, tbcol, aform, nrows, &status);
    FCGERR(status,errtxt);
    printf ("fcadef status = %d : %s\n", status, errtxt);


  /* C       write the required keywords to the ASCII table extension */
  FCPHTB (iunit, rowlen, nrows, tfield, ttype, tbcol, aform, tunit, extnam, &status);
    FCGERR(status,errtxt);
    printf ("fcphtb status = %d : %s\n", status, errtxt);

  frow = 1;
  felem = 1;
  nelem = 3;

  /* C       write the integer column (column 1) */
  colnum = 1;
  FCPCLJ (iunit, colnum, frow, felem, nelem, jvals, &status);
    FCGERR(status,errtxt);
    printf ("fcpclj status = %d : %s\n", status, errtxt);

  /*C       write the real column (column 2) */
  colnum = 2;
  FCPCLE(iunit,colnum,frow,felem,nelem,evals,&status);
    FCGERR(status,errtxt);
    printf ("fcpcle status = %d : %s\n", status, errtxt);

  /*C       write the string column (column 3) */
  colnum = 3;
  frow = 1;
  felem = 1;
  nelem = 3;

  FitsStrBufLen = 6;
  FCPCLS(iunit,colnum,frow,felem,nelem,svalues,&status);
    FCGERR(status,errtxt);
    printf ("fcpcls status = %d : %s\n", status, errtxt);

  /*
C       now create a binary table extension
C       first, create a new empty extension
*/
  FCCRHD (iunit, &status);
    FCGERR(status,errtxt);
    printf ("fccrhd status = %d : %s\n", status, errtxt);

  /* C       there are no variable length arrays so the size of the heap=0 */
  vardat = 0;

  /* C       write the required keywords to the binary table extension */
  FCPHBN (iunit, nrows, tfield, ttype, tform, tunit, extnam, vardat, &status);
    FCGERR(status,errtxt);
    printf ("fcphbn status = %d : %s\n", status, errtxt);

  /* C initialize the FITSIO parameters defining the structure of the table */
  FCBDEF (iunit, tfield, tform, vardat, nrows, &status);
    FCGERR(status,errtxt);
    printf ("fcbdef status = %d : %s\n", status, errtxt);

  jvals[0] = 69;
  jvals[1] = 70;
  jvals[2] = 71;
  evals[0] = 1003.;
  evals[1] = 1004.;
  evals[2] = 1005.;

  /* C       write the integer column (column 1) */
  colnum = 1;
  FCPCLJ (iunit, colnum, frow, felem, nelem, jvals, &status);
    FCGERR(status,errtxt);
    printf ("fcpclj status = %d : %s\n", status, errtxt);

  /*C       write the real column (column 2) */
  colnum = 2;
  FCPCLE(iunit,colnum,frow,felem,nelem,evals,&status);
    FCGERR(status,errtxt);
    printf ("fcpcle status = %d : %s\n", status, errtxt);

  /*C       write the string column (column 3) */
  colnum = 3;
  FCPCLS(iunit,colnum,frow,felem,nelem,svalues,&status);
    FCGERR(status,errtxt);
    printf ("fcpcls status = %d : %s\n", status, errtxt);

  /*C       now close the table and quit */
  FCCLOS (iunit, &status);
    FCGERR(status,errtxt);
    printf ("fcclos status = %d : %s\n", status, errtxt);

  if (status <= 0)
    {
      puts ("*** Program completed successfully ***");
    }
  else
    {

    /*C      get the error text description */
      FCGERR(status,errtxt);
      puts ("*** ERROR - program did not run successfully ***");
      printf ("status = %d : %s\n", status, errtxt);

      if (status == 105)
      {
        puts ("  ");
        puts ("The file 'csimple.fit' probably already exists.");
        puts ("Delete the FITS file and try again.");
      }
    }
}
