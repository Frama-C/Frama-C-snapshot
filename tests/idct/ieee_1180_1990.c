/* run.config
   GCC:
   OPT: -float-normal -no-warn-signed-overflow -val -deps -out -input tests/idct/idct.c share/libc/stdio.c share/math.c -journal-disable -remove-redundant-alarms -memexec-all -then -report -report-print-properties
*/
/* IEEE_1180_1990: a testbed for IDCT accuracy
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
 * The following program checks a IDCT C-code against the IEEE
 * 1180-1990 Standard Specification for the Implementation of 8x8
 * Inverse Discrete Cosine Transform */



#include "share/libc/stdio.h"
#include "share/math.h"



//@ assigns \nothing;
void exit (int x);

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define IEEE_1180_1990_TH M_PI/16.0
#define IEEE_1180_1990_ABS(a) ((a < 0) ? -a : a)

extern void idct (long m1[8][8], long m2[8][8]);

static long M1[8][8];

typedef struct {
  long pmse[8][8];
  long pme[8][8];
  } IEEE_1180_1990_stat_set;

/* The random generator of the IEEE 1180/1990 standard */
long IEEE_1180_1990_rand(long L, long H)

  {
  static long randx = 1;
  static double z = (double)0x7fffffff;
  long i, j;
  double x;

  randx = (randx * 1103515245) + 12345;
  i = randx & 0x7ffffffe;
  x = ((double)i) / z;
  x *= (L + H + 1);
  j = x;
  return(j - L);
  }

/* Generates random blocks with values between min and max */
static void IEEE_1180_1990_mkbk(long min, long max)

  {
  long i, j;

  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      M1[i][j] = IEEE_1180_1990_rand(-min, max);
  }

/* Floating point DCT */
void IEEE_1180_1990_dctf(long m1[8][8], long m2[8][8])

  {
  long i, j, k;
  double tmp1[8][8], tmp2[8][8];
  static double mcos[8][8];
  static int init = 1;

  if(init) {
    for(i = 0; i < 8; i++)
      for(j = 0; j < 8; j++)
        mcos[i][j] = ((j == 0) ? 0.5 / sqrt(2.0) : 0.5) *
          cos((2.0 * i + 1.0) * j * IEEE_1180_1990_TH);
    init = 0;
    }
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      tmp1[i][j] = 0.0;
      for(k = 0; k < 8; k++)
        tmp1[i][j] = tmp1[i][j] + mcos[k][i] * m1[k][j];
      }
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      tmp2[i][j] = 0.0;
      for(k = 0; k < 8; k++)
        tmp2[i][j] = tmp2[i][j] + tmp1[i][k] * mcos[k][j];
      if(tmp2[i][j] < -2048.0)
        m2[i][j] = -2048;
      else if(tmp2[i][j] > 2047.0)
        m2[i][j] = 2047;
      else if(tmp2[i][j] > 0.0)
        m2[i][j] = tmp2[i][j] + 0.5;
      else
        m2[i][j] = tmp2[i][j] - 0.5;
      }
  }

/* Floating point IDCT */
void IEEE_1180_1990_idctf(long m1[8][8], long m2[8][8])

  {
  long i, j, k;
  double tmp1[8][8], tmp2[8][8];
  static double mcos[8][8];
  static int init = 1;

  if(init) {
    for(i = 0; i < 8; i++)
      for(j = 0; j < 8; j++)
        mcos[i][j] = ((j == 0) ? 0.5 / sqrt(2.0) : 0.5) *
          cos((2.0 * i + 1.0) * j * IEEE_1180_1990_TH);
    init = 0;
    }

  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      tmp1[i][j] = 0.0;
      for(k = 0; k < 8; k++)
        tmp1[i][j] = tmp1[i][j] + mcos[i][k] * m1[k][j];
      }
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      {
      tmp2[i][j] = 0.0;
      for(k = 0; k < 8; k++)
        tmp2[i][j] = tmp2[i][j] + tmp1[i][k] * mcos[j][k];
      if(tmp2[i][j] < -256.0)
        m2[i][j] = -256;
      else if(tmp2[i][j] > 255.0)
        m2[i][j] = 255;
      else if(tmp2[i][j] > 0.0)
        m2[i][j] = tmp2[i][j] + 0.5;
      else
        m2[i][j] = tmp2[i][j] - 0.5;
      }
  }

int main()

  {
  IEEE_1180_1990_stat_set res[6];
  long i, j, k, m1[8][8], m2[8][8], m3[8][8], m4[8][8], succ, omse, ome, err;

  succ = 1;
/*@ loop pragma UNROLL 7; */
  for(i = 0; i < 6; i++)
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        res[i].pmse[j][k] = 0;
        res[i].pme[j][k] = 0;
        }
  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      M1[i][j] = 0;
  idct(M1, m2);

  for(i = 0; i < 8; i++)
    for(j = 0; j < 8; j++)
      if(m2[i][j] != 0)
        succ = 0;
  if(succ != 1)
    {
    printf("For all-zero input, the proposed IDCT shall generate all-zero ");
    printf("output.\n");
      ;
    }
  /*fprintf(stderr, "------------------------------------------------->\n");*/

/* loop pragma UNROLL 0 */
  for(i = 0; i < 10000; i++)
    {
    if((i + 1) % 200 == 0)
      {
        /*
      fprintf(stderr, "*");
      fflush(stderr);
      */
      }
    IEEE_1180_1990_mkbk(-256, 255);
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[0].pme[j][k] = res[0].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 1, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[0].pmse[j][k] = res[0].pmse[j][k] + err;
        }
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        M1[j][k] = - M1[j][k];
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[3].pme[j][k] = res[3].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 4, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[3].pmse[j][k] = res[3].pmse[j][k] + err;
        }
    IEEE_1180_1990_mkbk(-5, 5);
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[1].pme[j][k] = res[1].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 2, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[1].pmse[j][k] = res[1].pmse[j][k] + err;
        }
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        M1[j][k] = - M1[j][k];
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[4].pme[j][k] = res[4].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 5, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[4].pmse[j][k] = res[4].pmse[j][k] + err;
        }
    IEEE_1180_1990_mkbk(-300, 300);
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[2].pme[j][k] = res[2].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 3, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[2].pmse[j][k] = res[2].pmse[j][k] + err;
        }
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        M1[j][k] = - M1[j][k];
    IEEE_1180_1990_dctf(M1, m2);
    IEEE_1180_1990_idctf(m2, m3);
    idct(m2, m4);
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        err = m4[j][k] - m3[j][k];
        res[5].pme[j][k] = res[5].pme[j][k] + err;
        if(IEEE_1180_1990_ABS(err) > 1)
          {
            /*
          printf("For any pixel location, the peak error (ppe) shall not ");
          printf("exceed 1 in magnitude.\n");
          printf("  (%ld in set 6, block %ld, line %ld, column %ld).\n", err, i, j,
                 k);
                 */
          succ = 0;
          }
        err = err * err;
        res[5].pmse[j][k] = res[5].pmse[j][k] + err;
        }
    }
  for(i = 0; i < 6; i++)
    {
    omse = 0;
    ome = 0;
    for(j = 0; j < 8; j++)
      for(k = 0; k < 8; k++)
        {
        omse = omse + res[i].pmse[j][k];
        if(res[i].pmse[j][k] > 600)
          {
            /*
          printf("For any pixel location, the mean square error (pmse) shall ");
          printf("not exceed 0.06.\n");
          printf("  (%0.5f in set %ld, line %ld, column %ld).\n",
                 res[i].pmse[j][k] / 10000.0, i, j, k);
          */
          succ = 0;
          }
        ome = ome + res[i].pme[j][k];
        if(IEEE_1180_1990_ABS(res[i].pme[j][k]) > 150)
          {
            /*
          printf("For any pixel location, the mean error (pme) shall ");
          printf("not exceed 0.015 in magnitude.\n");
          printf("  (%0.5f in set %ld, line %ld, column %ld).\n",
                 res[i].pme[j][k] / 10000.0, i, j, k);
                 */
          succ = 0;
          }
        }
    if(omse > 12800)
      {
        /*
      printf("Overall, the mean square error (omse) shall ");
      printf("not exceed 0.02 in magnitude.\n");
      printf("  (%0.5f in set %ld).\n", omse / 640000.0, i);
      */
      succ = 0;
      }
    if(IEEE_1180_1990_ABS(ome) > 960)
      {
        /*
      printf("Overall, the mean error (ome) shall ");
      printf("not exceed 0.0015 in magnitude.\n");
      printf("  (%0.5f in set %ld).\n", ome / 640000.0, i);
      */
      succ = 0;
      }
    }
  /* fprintf(stderr, "\n"); */
  if(succ == 1)
    {
      /*
    fprintf(stderr, "Your IDCT meets the IEEE Std 1180-1990 accuracy ");
    fprintf(stderr, "requirements.\n");
    */
    exit(0);
    }
  else
    {
      /*
    fprintf(stderr, "Your IDCT does not meet the IEEE Std 1180-1990 accuracy ");
    fprintf(stderr, "requirements.\n");
    */
    exit(1);
    }
  }
