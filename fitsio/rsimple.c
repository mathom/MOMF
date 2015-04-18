/*      program rsimpl

C       A simple program to read the FITS primary array and a binary table 
C       extension that was created by the WSIMPLE program.
C       This is designed to illustrate the basic sequence of FITSIO calls
C       needed to read a FITS file.
*/

#ifndef vms
#include <malloc.h>
#endif
#include "cfitsio.h"

int MAIN_;

main ()
{
  int iunit,status,bitpix,naxis,naxes[99],pcount,gcount,naxx;
  int group,fpixel,nelem,i,j,jvals[3],rwstat,bksize;
  int ivalue[20],colnum,frow,felem, nelmt;
  int jvalue[20],*kvalue,Min,Max;
  int hdutyp,inull;
  char labels2[3][10];
  char comment[73];
  int FitsStrBufLen;
  float evals[3],enull;
  int simple,extend,anyflg;
  char errtxt[FITS_CLEN_ERRMSG];
  char **labels;

        status=0;
        iunit=15;
/*C     open the existing FITS file with readonly access  */
        rwstat=0;
        FCOPEN(iunit, "csimple.fit", rwstat, &bksize, &status);
          FCGERR(status,errtxt);
          printf ("fcopen status = %d : %s\n", status, errtxt);

/*C     read the required primary array keywords  */
        FCGHPR(iunit,99,&simple,&bitpix,&naxis,naxes,&pcount,
               &gcount,&extend,&status);
          FCGERR(status,errtxt);
          printf ("fcghpr status = %d : %s\n", status, errtxt);

/*C     read a keyword */
        FCGKYJ(iunit,"NAXIS",&naxx,comment, &status);

/*C     read the 2D array, one row at at time 
        first, test to make sure this is a 2D array */
        if (naxis != 2)
            puts("ERROR: This program only works on a 2D FITS array");

        puts(" ");
        printf("Primary array dimensions = %d , %d\n",naxes[0],naxes[1]);
        nelmt = naxes[0]*naxes[1];

/*C     read and print each row of the 2D array */
        group=0;
        fpixel=1;
        for (i = 0; i < naxes[1]; i++)
          { 
            FCGPVJ(iunit,group,fpixel,naxes[0],0,ivalue,&anyflg,&status);
            printf(" Row: %d\n",i);
            for (j = 0; j < naxes[0]; j++)
              printf(" %d",ivalue[j]);

            puts("\n");
            fpixel=fpixel+naxes[0];
           }

/*C     test the 2D read routine */
/*C     allocate memory for the integer array */
        kvalue = (int *) malloc (sizeof(int) * nelmt);
        FCG2DJ(iunit,group,0,naxes[0],naxes[0],naxes[1],kvalue,&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcg2dj status = %d : %s\n", status, errtxt);
  
/*C     compute min and max of values */
        Max = -100000000;
        Min = 1000000000;
        for (j = 0; j < nelmt; j++)
        {
           if (Max < kvalue[j]) Max = kvalue[j];
           if (Min > kvalue[j]) Min = kvalue[j];
        }
        printf ("Min = %d\n", Min);
        printf ("Max = %d\n", Max);

        puts("\n");

/*C     now move to the next ASCII table extension */
        FCMAHD(iunit,2,&hdutyp,&status);
          FCGERR(status,errtxt);
          printf ("fcmahd status = %d : %s\n", status, errtxt);

/*C     read the integers from the 1st column */
        colnum=1;
        frow=1;
        felem=1;
        nelem=3;
        inull=0;
        FCGCVJ(iunit,colnum,frow,felem,nelem,inull,jvals,&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcvj status = %d : %s\n", status, errtxt);

        puts(" Column 1");
        for (i = 0; i < nelem; i++)
              printf(" %d",jvals[i]);
        puts("\n");

/*C     test that it read the correct value */
        if (jvals[0] != 66)
            puts("ERROR: 1st value should = 66");

/*C     read the reals from the 2nd column */
        colnum=2;
        enull=0.;
        FCGCVE(iunit,colnum,frow,felem,nelem,enull,evals,&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcve status = %d : %s\n", status, errtxt);

        puts(" Column 2");
        for (i = 0; i < nelem; i++)
              printf(" %8.2f",evals[i]);
        puts("\n");

/*C     test that it read the correct value */
        if (evals[0] != 1000.)
            puts("ERROR: 1st value should = 1000.");

/*C     read the strings from the 3nd column */
        colnum=3;

/*C     must allocate space for the strings  */
        labels = (char **) malloc ( nelem * sizeof(char *));
	FitsStrBufLen = 20;
        labels[0] = (char *) malloc( (FitsStrBufLen + 1) 
				     * nelem * sizeof(char));
        for (i=1;i< nelem;i++)
           labels[i]= labels[0] + i * (FitsStrBufLen + 1);

        
        FCGCVS(iunit,colnum,frow,felem,nelem," ",labels[0],&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcvs status = %d : %s\n", status, errtxt);

        puts(" Column 3");
        for (i = 0; i < nelem; i++)
              printf(" %s",labels[i]);
        puts("\n");

/*C     now move to the next binary table extension */
        FCMAHD(iunit,3,&hdutyp,&status);
          FCGERR(status,errtxt);
          printf ("fcmahd status = %d : %s\n", status, errtxt);

/*C      read the integers from the 1st column */
        colnum=1;
        FCGCVJ(iunit,colnum,frow,felem,nelem,inull,jvals,&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcvj status = %d : %s\n", status, errtxt);

        puts(" Column 1");
        for (i = 0; i < nelem; i++)
              printf(" %d",jvals[i]);
        puts("\n");

/*C     test that it read the correct value */
        if (jvals[0] != 69)
            puts("ERROR: 1st value should = 69");

/*C     read the reals from the 2nd column */
        colnum=2;
        enull=0.;
        FCGCVE(iunit,colnum,frow,felem,nelem,enull,evals,&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcve status = %d : %s\n", status, errtxt);

        puts(" Column 2");
        for (i = 0; i < nelem; i++)
              printf(" %8.2f",evals[i]);
        puts("\n");

/*C     test that it read the correct value */
        if (evals[0] != 1003.)
            puts("ERROR: 1st value should = 1003.");

/*C     read the strings from the 3nd column */
        colnum=3;

/*C     note: sufficient space was already allocated for "labels" */
        FCGCVS(iunit,colnum,frow,felem,nelem," ",labels[0],&anyflg,&status);
          FCGERR(status,errtxt);
          printf ("fcgcvs status = %d : %s\n", status, errtxt);

        puts(" Column 3");
        for (i = 0; i < nelem; i++)
              printf(" %s",labels[i]);
        puts("\n");

/*C     now close the table and quit */
        FCCLOS(iunit,&status);
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
     }
}
