/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    CEA (Commissariat à l'Énergie Atomique)                             */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#include "caml/mlvalues.h"
#include "caml/bigarray.h"
///#include <stdio.h>
#include <assert.h>

#ifdef ARCH_SIXTYFOUR
static const uint64 incr = 0x0808080808080808U;
static const uint64 end_of_chain = 0xFEFEFEFEFEFEFEFEU;

value init_fat(value fat, value ml_size)
{
  int64 pat0, pat_end, size;
  register int64 pat;
  register int64 *p1 ;
  register int64 *p2 ;


  size = Int_val(ml_size);
#ifndef Caml_ba_data_val
  fat = (value)Data_bigarray_val(fat);
#else
  fat = (value)Caml_ba_data_val(fat);
#endif
  *((char*)&pat0)       = 1;
  *(((char*)&pat0) + 1) = 2;
  *(((char*)&pat0) + 2) = 3;
  *(((char*)&pat0) + 3) = 4;
  *(((char*)&pat0) + 4) = 5;
  *(((char*)&pat0) + 5) = 6;
  *(((char*)&pat0) + 6) = 7;
  *(((char*)&pat0) + 7) = 8;

  *((char*)&pat_end)       = 249;
  *(((char*)&pat_end) + 1) = 250;
  *(((char*)&pat_end) + 2) = 251;
  *(((char*)&pat_end) + 3) = 252;
  *(((char*)&pat_end) + 4) = 253;
  *(((char*)&pat_end) + 5) = 254;
  *(((char*)&pat_end) + 6) = 255;
  *(((char*)&pat_end) + 7) = 0;

  p1 = (int64*)fat;
  p2 = p1 + 512 / 8;

  do {
    int i = 31;
    pat = pat0;
    do {
      *p1 = pat;
      *(p1 + 256 / 8) = end_of_chain;
      p1 += 1;
      *p2 = pat;
      *(p2 + 256 / 8) = end_of_chain;
      p2 += 1;
      pat += incr;
      i--;
    } while (i != 0);
    *p1 = pat_end;
    *(p1 + 256 / 8) = end_of_chain;
    p1 += 1 + (512 + 256) / 8;

    *p2 = pat_end;
    *(p2 + 256 / 8) = end_of_chain;
    p2 += 1 + (512 + 256) / 8;

    size -= 2;
  }
  while (size != 0);
  return Val_unit;
}

#else

static const unsigned int incr = 0x04040404U;
static const unsigned int end_of_chain = 0xFEFEFEFEU;

value init_fat(value fat, value ml_size)
{
  int pat0, pat_end, size;
  register int pat1;
  register int pat2;
  register int *p1 ;
  register int *p2 ;

  size = Int_val(ml_size);
#ifndef Caml_ba_data_val
  fat = (value)Data_bigarray_val(fat);
#else
  fat = (value)Caml_ba_data_val(fat);
#endif

  *((char*)&pat0)       = 1;
  *(((char*)&pat0) + 1) = 2;
  *(((char*)&pat0) + 2) = 3;
  *(((char*)&pat0) + 3) = 4;

  *((char*)&pat_end)       = 253;
  *(((char*)&pat_end) + 1) = 254;
  *(((char*)&pat_end) + 2) = 255;
  *(((char*)&pat_end) + 3) = 0;

  p1 = (int*)fat;
  p2 = p1 + 512 / 4;
  do {
    int i = 31;
    pat1 = pat0;
    pat2 = pat0 + incr;
    do {
      *p1 = pat1;
      *(p1 + 256 / 4) = end_of_chain;
      *(p1 + 1) = pat2;
      *(p1 + 1 + 256 / 4) = end_of_chain;
      p1 += 2;
      *p2 = pat1;
      *(p2 + 256 / 4) = end_of_chain;
      *(p2 + 1) = pat2;
      *(p2 + 1 + 256 / 4) = end_of_chain;
      p2 += 2;
      pat2 += 2*incr;
      pat1 += 2*incr;
      i--;
    } while (i != 0);
    pat2 = pat_end;
    *p1 = pat1;
    *(p1 + 256 / 4) = end_of_chain;
    *(p1 + 1) = pat2;
    *(p1 + 1 + 256 / 4) = end_of_chain;
    p1 += 2 + (512 + 256) / 4;

    *p2 = pat1;
    *(p2 + 256 / 4) = end_of_chain;
    *(p2 + 1) = pat_end;
    *(p2 + 1 + 256 / 4) = end_of_chain;
    p2 += 2 + (512 + 256) / 4;

    size -= 2;
  }
  while (size != 0);
  return Val_unit;
}
#endif

extern value caml_weak_none;
#define FREE_ENTRY 255
#define FAT_ENTRIES_SIZE 512
#define ENTRIES_START 256
#define END_OF_CHAIN 254

value collect_Nones(value t, value mlsize)
{
  unsigned  char *fat;
  value *data;
  value weak_none_sentinel = caml_weak_none;
  int size = Int_val(mlsize);
  int begin_chain;
  int chain, next;
  unsigned char tmp, current_free;
#ifndef Caml_ba_data_val
  fat = (unsigned char*)Data_bigarray_val(*(value*)t);
#else
  fat = (unsigned char*)Caml_ba_data_val(*(value*)t);
#endif
  data = 1 + *(1+(value**) t);

  goto nextbuckx;

 unloadedcompare:
  if (data[next] != weak_none_sentinel)
    goto unloadednext;

 transfer:
  tmp = fat[next];
  fat[next] = current_free;
  current_free = next;
  next = tmp;
  if (tmp > 253)
    goto nextchain;
  if (data[next] == weak_none_sentinel)
    goto transfer;

  // loadednext:
  fat[chain] = tmp;

 unloadednext:
  chain = next;
  next = fat[next];
  if (next <= 253)
    goto unloadedcompare;

 nextchain:
  begin_chain ++;
  fat[chain] = END_OF_CHAIN;
  next = begin_chain;
  if (begin_chain & 255)
    goto unloadednext;

  size --;
  fat[FREE_ENTRY] = current_free;
  if (size == 0)
    goto end;

  fat += 512;
  data += 254;
 nextbuckx:
  current_free = fat[FREE_ENTRY];
  begin_chain = ENTRIES_START;
  next = ENTRIES_START;
  goto unloadednext;
 end:
  return Val_unit;
}

#if defined(__i386__) 
#define GETCOUNTER(low,high)						\
  __asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
#else
#if  defined(__x86_64__)
#define GETCOUNTER(low,high)						\
{ \
     unsigned int __a,__d; \
     asm volatile("rdtsc" : "=a" (__a), "=d" (__d)); \
     low = ((unsigned long)__a) | (((unsigned long)__d)<<32); \
     high = 0; \
}
#else
#define GETCOUNTER(low,high)						\
  { low = 0; high = 0; }
#endif
#endif




value getperfcount1024(value dum)
{
  unsigned long l,h,acc;
  GETCOUNTER(l,h);
  acc = (l >> 10) | (h << 22);
  return (acc | 1);
}

value getperfcount(value dum)
{
  unsigned long l, h;
  GETCOUNTER(l,h);
  return (l | 1);
}
