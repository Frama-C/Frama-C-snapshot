/* run.config
   GCC:
   DONTRUN:
*/
/* IDCT: a fixed point IDCT implementation.
 * Copyright (C) 2001  Renaud Pacalet
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Renaud Pacalet
 * Departement Comunications et Electronique,
 * Ecole Nationale Superieure des Telecommunications
 * 46, Rue Barrault 75634 Paris Cedex 13
 * Tel : +33 1 45 81 78 08
 * Fax : +33 1 45 80 40 36
 * Email : pacalet@enst.fr
 *
 * The following code implements a 2-steps IDCT. The
 * computations are done in finite accuracy, controlled by the 3 macros
 * NBC1, NBI, NBC2. It behaves exactly the same as our hardware
 * distributed arithmetics based architecture. The default values for
 * NBC1, NBI and NBC2 (14, 14 and 14) are a kind of best choice if you
 * need to pass the IEEE 1180-1990 requirements but want the different
 * dynamics to be as small as possible. You can play with them but be
 * aware that accuracy strongly depends on them.
 *
 * The input of the function should be a 8x8 matrix of integers in the
 * range of -2048 ... 2047 (2's complement coded on 12 bits).
 * The output will be a 8x8 matrix of integers in the
 * range -256 ... 255. */





#include "share/math.h"


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TH M_PI/16.0
#define NBC1 14
/* Number of bits used to represent the first pass hard-coded cosines
 * matrix. */
#define NBI 14
/* Number of bits kept on partial results after first pass. This will
 * be the word length of the transposition RAM. */
#define NBC2 14
/* Number of bits used to represent the second pass hard-coded cosines
 * matrix (usually the same as NBC1 but...). */

void idct (long m1[8][8], long m2[8][8])
/* m1 is the input 8x8 matrix of DCT coefficients. m2 will hold the
 * IDCT result. */

  {
  long i, j, k, tmp1[8][8], tmp2[8][8];
/* Loops indexes and temporary matrices. */
  double ftmp1, ftmp2;
/* Temporary variables used for rounding purpose when computing the
 * hard-coded cosines matrices. */
  static int init = 1;
/* A simple flag that tells it's the first time the function is called.
 * When init is true we will compute the hard-coded cosines matrices for
 * pass one and pass two, then reset init and compute the IDCT. If not
 * we will only compute the IDCT. */
  static long mc1[8][8], mc2[8][8];
/* hard-coded cosines matrices. */
  
  if (init) {
/* If init (it's the first time the function is called), let's compute
 * the hard-coded cosines matrices for pass one and pass two. */
    for (i = 0; i < 8; i++)
      for (j = 0; j < 8; j++)
        {
        ftmp1 = ((j == 0) ? 0.5 / sqrt (2.0) : 0.5) *
          cos ((2.0 * i + 1.0) * j * TH);
        ftmp2 = ftmp1;
/* The well known formula. The max absolute value for ftmp1 and ftmp2 is 0.5. */
        ftmp1 *= (1 << NBC1);
/* Multiply the cosine coefficient by 2^NBC1. The max absolute value for
 * ftmp1 is 2^(NBC1-1). */
        if (ftmp1 < 0)
          ftmp1 -= 0.5;
        else
          ftmp1 += 0.5;
/* For symetrical rounding. */
        mc1[i][j] = ftmp1;
/* The rounding itself. mc1
 * contains now the cosines reprensented in 2's complement form, fixed
 * point on NBC1 bits. */
        ftmp2 *= (1 << NBC2);
/* Multiply the cosine coefficient by 2^NBC2. The max absolute value for
 * ftmp2 is 2^(NBC2-1). */
        if (ftmp2 < 0)
          ftmp2 -= 0.5;
        else
          ftmp2 += 0.5;
/* For symetrical rounding. */
        mc2[i][j] = ftmp2;
/* The rounding itself. mc2
 * contains now the cosines reprensented in 2's complement form, fixed
 * point on NBC2 bits. */
        }
      init = 0;
/* Reset the init flag. On the next call mc1 and mc2 will not be
 * computed anymore. */
      }

/* Then the first pass. */
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      for(k = 0, tmp1[i][j] = 0; k < 8; k++)
        tmp1[i][j] += mc1[i][k] * m1[k][j];
/* The [i,j] coefficient of the matrix product MC1*M1. */
      tmp1[i][j] >>= (NBC1 + 10 - NBI);
/* In order to keep NBI bits only. The DCT coefficients of m1 are
 * integers, 2's complement coded on 12 bits. The result should be
 * reprensented on NBC1 + 12 + 3 bits (sum of 8 partial products, each
 * of them beeing reprensented on NBC1 + 12 bits). A dynamic study can
 * prove that m1 beeing a DCT output tmp1[i][j] can be represented on
 * NBC1 + 11 bits only.
 * So as we have a NBC1 + 11 long integer and want to
 * keep NBI bits only we first drop NBC1 + 10 - NBI bits. We will drop
 * the last bit after rounding. */
      tmp1[i][j] += 1;
/* For rounding purpose. */
      tmp1[i][j] >>= 1;
/* Final rounding. tmp1[i][j] is now represented on NBI bits. */
      if (tmp1[i][j] < -(1 << (NBI - 1)))
        tmp2[j][i] = -(1 << (NBI - 1));
      else if (tmp1[i][j] >= (1 << (NBI - 1)))
        tmp2[j][i] = (1 << (NBI - 1)) - 1;
      else
        tmp2[j][i] = tmp1[i][j];
/* Saturation and transposition at the same time. Why saturation? I
 * wrote above that it can be proved that tmp1[i][j] can be represented
 * on NBI bits after truncation and rounding but it assumed that m1 was
 * a DCT output, which is usually not the case because of quantization
 * and inverse quantization. So saturation is needed. */
      }

/* Then the second pass. Looks like the first one. */
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      for(k = 0, tmp1[i][j] = 0; k < 8; k++)
        tmp1[i][j] += mc2[i][k] * tmp2[k][j];
/* The [i,j] coefficient of the matrix product MC2*TMP2, that is,
 * MC2*t(TMP1) = MC2*t(MC1*M1) = MC2*tM1*tMC1. */
      tmp1[i][j] >>= (NBC2 + NBI - 12);
/* In order to keep 9 bits only. The coefficients of tmp2 are fixed
 * point, 2's complement coded on NBI bits. The result should be
 * reprensented on NBC2 + NBI + 3 bits (sum of 8 partial products, each
 * of them beeing reprensented on NBC2 + NBI bits). A dynamic study can
 * prove that m1 beeing a DCT output tmp2[i][j] can be represented on
 * NBC2 + NBI - 2 bits only (I wrote a paper on this study once; If
 * you're interested...). So as we have a NBC2 + NBI - 2 long integer
 * and want to keep 9 bits only we first drop NBC2 + NBI - 12 bits. We
 * will drop the last bit after rounding. */
      tmp1[i][j] += 1;
/* For rounding purpose. */
      tmp1[i][j] >>= 1;
/* Final rounding. tmp2[i][j] is now represented on 9 bits. */
      if (tmp1[i][j] < -256)
        m2[j][i] = -256;
      else if (tmp1[i][j] > 255)
        m2[j][i] = 255;
      else
        m2[j][i] = tmp1[i][j];
/* Saturation and transposition at the same time. Why saturation? I
 * wrote above that it can be proved that tmp2[i][j] can be represented
 * on 9 bits after truncation and rounding but it assumed that m1 was
 * a DCT output, which is usually not the case because of quantization
 * and inverse quantization. So saturation is needed. The last
 * transposition leads to M2 = t(MC2*tM1*tMC1) = MC1*M1*tMC2, that is,
 * the IDCT formula of M1. */
      }
  }
