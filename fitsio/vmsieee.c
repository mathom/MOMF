#include <stdio.h>
/* void vax2alpha_d(longval1, longval2, dval) */
void ieevpd(longval, dval, nvals)
long *longval, *nvals;
double *dval;
{  
/* Melissa H. Gibby, CSC */
/* to convert d_float r*8 values created on the VAX to IEEE values on the Alpha
   there are several steps as outlined below:
   1. Put *longword1 and *longword2 into R1.lval and R2.lval.
   2. Obtain the sign bit which is now in bit 15 of R1 and put it into bit
      31 of the resultant long (called L1). 
   3. Get the exponent from R1 and put into L1. The exponent is in bits (14:7)
      of R1. Add 894 (in decimal) to get the value that IEEE expects for 
      the exponent. It should be put into bits (30:20) of L1.
   4. Get the 1st part of the mantissa from R1. It is in bits (6:0) of R1.
      Put it into bits (19:13) of L1.
   5. Get the 2nd part of the mantissa from R1. It is in bits (31:19) of R1. Put
      it into bits (12:0) of L1.
   6. Get the 3rd part of the mantissa from R1. It is in bits (18:16) of R1. Put
      it into bits (31:29) of L2.
   7. Start working with the 2nd long word sent to this subroutine (R2). 
   8. Put bits (31:3) of R2 into bits(28:0) of L2.
*/

long ii;
unsigned long blank;
unsigned char cv;
unsigned short sv;

union unite{
  unsigned char cval[4];
  unsigned short sval[2];
  unsigned long lval;
  float fval;
};
union dunite{
  unsigned long lval[2];
  double dval;
};
union unite R1, R2;   /* the input words */
union unite L1, L2;   /* the output words to form the dble */

union unite N;
union dunite F;

/* printf("ieevpd(longval, dval, *nvals): %u %u %d\n",longval,dval, *nvals); */
for (ii=0;ii < *nvals; ii++, dval++)
{
  /* check for 0 */
  if (*longval == 0 && longval[1] ==0)
    *dval = 0.0;
  else
  {
    R1.lval = *longval;
    R2.lval = longval[1];
    longval += 2;     /* increment pointer to next double word */

    sv = R2.sval[1];
    R2.sval[1] = R2.sval[0];
    R2.sval[0] = sv;

    /* Obtain the sign bit which is now in bit 15 of R1 and put it into bit
      31 of the resultant long (called L1).    */
    L1.lval = 0L;
    blank = 0L;
    blank = R1.lval & 0x00008000;
    L1.lval = blank << 16;
 
    /* Get the exponent from R1 and put into L1. The exponent is in bits (14:7)
      of R1. Add 894 (in decimal) to get the value that the Solarus expects for 
      the exponent. It should be put into bits (30:20) of L1.  */
    blank = 0L;
    blank = (R1.lval >> 7) & 0xff; 
    blank = blank + 894; /* to account for the 11 bit exp used in IEEE instead 
                           of 8 bits used in d_float Vax notation */

    blank = blank << 20;
    L1.lval |= blank;	/* put the value into L1 */

    /* Get the 1st part of the mantissa from R1. It is in bits (6:0) of R1.
      Put it into bits (19:13) of L1. */
    blank = 0L;
    blank = R1.lval & 0x7f;
    blank <<= 13;
    L1.lval |= blank;

    /* Get the 2nd part of the mantissa from R1. It is in bits (31:19) of R1. 
       Put it into bits (12:0) of L1. */
    blank = 0L;
    blank = (R1.lval >> 19) & 0x1fff;
    L1.lval |= blank;

    /* Get the 3rd part of the mantissa from R1. It is in bits (18:16) of R1. 
       Put it into bits (31:29) of L2. */
    blank = 0L;
    blank = (R1.lval << 13) & 0xe0000000;
    L2.lval = 0L;   /* starting to fill L2 */
    L2.lval |= blank;

    /* Put bits (31:3) of R2 into bits(28:0) of L2. */
    blank = 0L;
    blank = (R2.lval >> 3) & 0x1fffffff;
    L2.lval |= blank;

    /* reverse the bytes in the output words */
    cv = L1.cval[3];
    L1.cval[3] = L1.cval[0];
    L1.cval[0] = cv;
    cv = L1.cval[2];
    L1.cval[2] = L1.cval[1];
    L1.cval[1] = cv;

    cv = L2.cval[3];
    L2.cval[3] = L2.cval[0];
    L2.cval[0] = cv;
    cv = L2.cval[2];
    L2.cval[2] = L2.cval[1];
    L2.cval[1] = cv;

    /* initialize F */
    F.lval[0] = L1.lval;
    F.lval[1] = L2.lval;

    *dval = F.dval;
    }
  }
  return;
}

/* void alpha2vax_d(longval1, longval2, dval) */
void ieevud(longval, dval, nvals)
long *longval, *nvals;
double *dval;
{
/* Melissa H. Gibby, CSC */
/* this routine will take values read in a FITS file (which are in IEEE format)
   and convert them to VAX d-float format. The following steps are necessary:
   1. Reverse the bytes in each input word to form R1 and R2.
   2. Put R2(28:0) into L2(31:3).  This is the 4th part of the mantissa.
   3. Put R2(31:29) into L1(18:16). This is the 3rd part of the mantissa.
   4. Put R1(12:0) into L1(31:19). This is the 2nd part of the mantissa.
   5. Put R1(19:13) into L1(6:0). This is the 1st part of the mantissa. 
   6. Get bits R1(30:20); This is the IEEE exponent. Subtract 894. Put into
      L1(14:7).
   7. Put R1(31) into L1(15). This is the sign bit.
   8. Reverse the 32 bit words in L2.
 */
long ii;
unsigned long blank;
unsigned char cv; 
unsigned short sv;

union unite{
  unsigned char cval[4];
  unsigned short sval[2];
  unsigned long lval;
};   

union dunite{
  unsigned long lval[2];
  double dval;
};

union unite R1, R2;   /* the input words */
union unite L1, L2;   /* the output words to form the dble */

union dunite F;

/* printf("ieevud(longval, dval, *nvals): %u %u %d\n",longval,dval,*nvals); */
for (ii=0;ii < *nvals; ii++, dval++)
{
  /* check for 0 */
  if (*longval == 0 && longval[1] ==0)
    *dval = 0.0;
  else
  {
    R1.lval = *longval;
    R2.lval = longval[1];
    longval += 2;     /* increment pointer to next double word */

    /*  Reverse the bytes in each input word to form R1 and R2.  */
    cv = R1.cval[3];
    R1.cval[3] = R1.cval[0];
    R1.cval[0] = cv;
    cv = R1.cval[2];
    R1.cval[2] = R1.cval[1];
    R1.cval[1] = cv;

    cv = R2.cval[3];
    R2.cval[3] = R2.cval[0];
    R2.cval[0] = cv;
    cv = R2.cval[2];
    R2.cval[2] = R2.cval[1];
    R2.cval[1] = cv;

    /* initialize L1 and L2 */
    L1.lval = 0L;
    L2.lval = 0L;

    /* Put R2(28:0) into L2(31:3).  This is the 4th part of the mantissa. */
    blank = 0L;
    blank = (R2.lval << 3) & 0xfffffff8;
    L2.lval |= blank;

    /* Put R2(31:29) into L1(18:16). This is the 3rd part of the mantissa. */
    blank = 0L;
    blank = (R2.lval >> 13) & 0x70000;
    L1.lval |= blank;

    /* Put R1(12:0) into L1(31:19). This is the 2nd part of the mantissa. */
    blank = 0L;
    blank = (R1.lval << 19) & 0xfff80000;
    L1.lval |= blank;

    /* Put R1(19:13) into L1(6:0). This is the 1st part of the mantissa. */
    blank = 0L;
    blank = (R1.lval >> 13) & 0x7f;
    L1.lval |= blank;

    /* Get bits R1(30:20); This is the IEEE exponent. Subtract 894. Put into
      L1(14:7). */
    blank = 0L;
    blank = (R1.lval >> 20) & 0x7ff;
    blank = blank - 894;
    L1.lval |= (blank << 7);

    /* Put R1(31) into L1(15). This is the sign bit. */
    blank = 0L;
    blank = (R1.lval >> 16) & 0x8000;
    L1.lval |= blank;

    /* Reverse the 32 bit words in L2. */
    sv = L2.sval[1];
    L2.sval[1] = L2.sval[0];
    L2.sval[0] = sv;

    /* load up the result */
    F.lval[0] = L1.lval;
    F.lval[1] = L2.lval;

    *dval = F.dval;
    }
  }
  return;
}




