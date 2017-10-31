/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
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

/*! ***********************************************************************
 * \file  e_acsl_bits.h
 * \brief Bit-level manipulations and endianness checks.
 *
 * Should be included after e_acsl_printf.h and e_acsl_string.h headers.
 *
 * FIXME: Present implementation is built for little-endian byte order.  That
 * is, the implementation assumes that least significant bytes are stored at
 * the highest memory addresses. In future support for big-endian/PDP byte
 * orders should also be provided.
 *
 * CAUTION: As per above FIXME notice, all examples, macros and functions
 * assume little-endian byte order.
***************************************************************************/

#ifndef E_ACSL_BITS_H
#define E_ACSL_BITS_H

#include <stdint.h>
#include <stddef.h>

/* Check if we have little-endian and abort the execution otherwise. */
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#  error "Big-endian byte order is unsupported"
#elif __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
#  error "PDP-endian byte order is unsupported"
#elif __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#  error "Unknown byte order"
#endif

/* Bit-level manipulations {{{ */

/* 64-bit type with all bits set to ones */
#define ONE UINT64_MAX

/* 64-bit type with all bits set to zeroes */
#define ZERO (~ONE)

/* Set a given bit in a number to '1' (least-significant bit is at index zero).
 * Example:
 *  int x = 0;    // x => 0000 0000 ...
 *  bitset(0, x)  // x => 1000 0000 ...
 *  bitset(7, x)  // x => 1000 0001 ... */
#define setbit(_bit,_number) (_number |= 1 << _bit)

/* Same as bitset but the `_bit` bit is cleared (i.e., set of zero) */
#define clearbit(_bit, _number) (_number &= ~(1 << _bit))

/* Evaluate to a non-zero value if a given bit in a number is set to 1.
 *  int x = 1;     // x => 1000 0000 ...
 *  checkbit(0, x) // 1
 *  checkbit(1, x) // 0 */
#define checkbit(_bit, _number) ((_number >> _bit) & 1)

/* Toggle a given bit.
 * Example:
 *  int x = 4;        // x => 0010 0000 ...
 *  togglebit(3, x);  // x => 0000 0000 ...
 *  togglebit(3, x);  // x => 0010 0000 ... */
#define togglebit(_bit, _number) (_number ^= 1 << _bit)

/* Set a given bit to a specified value (e.g., 0 or 1). */
#define changebit(_bit, _val, _number) \
  (_number ^= (-_val ^ _number) & (1 << _bit))

/* Set up to 64 bits from left to right to ones.
 * Example:
 *  int x = 0;        // x => 00000000 00000000 ...
 *  setbits64(11, x)  //   => 11111111 11100000 ...
 *  setbits64(64, x)  //   => behaviour undefined */
#define setbits64(_bits, _number)   (_number |= ~(ONE << _bits))

/* Set up to 64 bits from left to right to ones skiping `_skip` leftmost bits
 * Example:
 *  int x = 0;          // x => 00000000 00000000 ...
 *  setbits64(11, x, 2) //   => 00111111 11111000 ...
 *  setbits64(64, x, 2) //   => behaviour undefined */
#define setbits64_skip(_bits, _number, _skip) \
  (_number |= ~(ONE << _bits) << _skip)

/* Evaluate to 1 if up to 64 bits from left to right in `_number` are set:
 * Example:
 *  int x = 31;         // x => 11111000 00000000 ...
 *  checkbits64(4, x)   //   => 1
 *  checkbits64(5, x)   //   => 1
 *  checkbits64(6, x)   //   => 0
 *  checkbits64(64, x)  //   => behaviour undefined */
#define checkbits64(_bits, _number) \
  ((_number & ~(ONE << _bits)) ==  (~(ONE << _bits)))

/* Same as checkbits64 but with skipping `_skip` leftmost bits
 * Example:
 *  int x = 124;               // x => 00111100 00000000 ...
 *  checkbits64_skip(3,  x, 2) // => 1
 *  checkbits64_skip(4,  x, 2) // => 1
 *  checkbits64_skip(5,  x, 2) // => 0
 *  checkbits64_skip(3,  x, 1) // => 0
 *  checkbits64_skip(64, x, 0) // => behaviour undefined */
#define checkbits64_skip(_bits, _number, _skip) \
  ((_number & ~(ONE << _bits) << _skip) == (~(ONE << _bits) << _skip))

/* Same as `setbits64' but clear the bits (set to zeroes). */
#define clearbits64(_bits, _number) (_number &= ONE << _bits)

/* Set `_bits' bits from right to the left starting from a 64-bit boundary.
 * Example:
 *  long x = 0;            // x => ... 00000000 00000000 00000000 00000000
 *  setbits64_right(10, x) // x => ... 00000000 00000000 00000011 11111111 */
#define setbits64_right(_bits, _number)   (_number |= ~(ONE >> _bits))

/* Same as setbits64_right but clears bits (sets to zeroes) */
#define clearbits64_right(_bits, _number) (_number &= ONE >> _bits)

/* Set `size' bits starting from an address given by `ptr' to ones.
 * Example:
 *  char a[4];
 *  memset(a,0,4);    // => 00000000 00000000 00000000 00000000
 *  setbits(&a, 11);  // => 11111111 11100000 00000000 00000000 */
static inline void setbits(size_t size, void *ptr) {
  size_t i;
  int64_t *lp = (int64_t*)ptr;
  for (i = 0; i < size/64; i++)
    *(lp+i) |= ONE;
  setbits64(size%64, *(lp+i));
}

/* Same as `setbits' but clear the bits (set to zeroes). */
static inline void clearbits(size_t size, void *ptr) {
  size_t i;
  int64_t *lp = (int64_t*)ptr;
  for (i = 0; i < size/64; i++)
    *(lp+i) &= ZERO;
  clearbits64(size%64, *(lp+i));
}

/* Same as `setbits' but clear the bits (set to zeroes). */
static inline int checkbits(size_t size, void *ptr) {
  size_t i;
  int64_t *lp = (int64_t*)ptr;
  for (i = 0; i < size/64; i++) {
    if (*(lp+i) != ONE)
      return 0;
  }
  return checkbits64(size%64, *(lp+i));
}

/* Same as `setbits' but set the bits from right to left
 * Example:
 *  char a[4];
 *  memset(a,0,4);         // => 00000000 00000000 00000000 00000000
 *  setbits_right(&a, 11); // => 00000000 00000000 00000111 11111111 */
static inline void setbits_right(size_t size, void *ptr) {
  size_t i = 0;
  int64_t *lp = (int64_t*)ptr - 1;
  for (i = 0; i < size/64; i++)
    *(lp-i) |= ONE;
  setbits64_right(size%64, *(lp-i));
}

/* Same as `setbits_right' but clear the bits (set to zeroes). */
static inline void clearbits_right(size_t size, void *ptr) {
  size_t i = 0;
  int64_t *lp = (int64_t*)ptr - 1;
  for (i = 0; i < size/64; i++)
    *(lp-i) &= ZERO;
  clearbits64_right(size%64, *(lp-i));
}
/* }}} */
#endif
