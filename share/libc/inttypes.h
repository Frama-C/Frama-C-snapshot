/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

#ifndef __FC_INTTYPES
#define __FC_INTTYPES
#include "features.h"
__PUSH_FC_STDLIB
/* ISO C: 7.8 */
#include "__fc_machdep.h"
#include "stdint.h"
#include "errno.h"

/* ISO C: 7.8.1 */

/* Macros for printing format specifiers.  */

/* Decimal notation.  */
# define PRId8		__PRI8_PREFIX "d"
# define PRId16		__PRI16_PREFIX "d"
# define PRId32		__PRI32_PREFIX "d"
# define PRId64		__PRI64_PREFIX "d"

# define PRIdLEAST8	__PRI8_PREFIX "d"
# define PRIdLEAST16	__PRI16_PREFIX "d"
# define PRIdLEAST32	__PRI32_PREFIX "d"
# define PRIdLEAST64	__PRI64_PREFIX "d"

# define PRIdFAST8	__PRI8_PREFIX "d"
# define PRIdFAST16	__PRIFAST16_PREFIX "d"
# define PRIdFAST32	__PRI32_PREFIX "d"
# define PRIdFAST64	__PRI64_PREFIX "d"


# define PRIi8		__PRI8_PREFIX "i"
# define PRIi16		__PRI16_PREFIX "i"
# define PRIi32		__PRI32_PREFIX "i"
# define PRIi64		__PRI64_PREFIX "i"

# define PRIiLEAST8	__PRI8_PREFIX "i"
# define PRIiLEAST16	__PRI16_PREFIX "i"
# define PRIiLEAST32	__PRI32_PREFIX "i"
# define PRIiLEAST64	__PRI64_PREFIX "i"

# define PRIiFAST8	__PRI8_PREFIX "i"
# define PRIiFAST16	__PRIFAST16_PREFIX "i"
# define PRIiFAST32	__PRI32_PREFIX "i"
# define PRIiFAST64	__PRI64_PREFIX "i"

/* Octal notation.  */
# define PRIo8		__PRI8_PREFIX "o"
# define PRIo16		__PRI16_PREFIX "o"
# define PRIo32		__PRI32_PREFIX "o"
# define PRIo64		__PRI64_PREFIX "o"

# define PRIoLEAST8	__PRI8_PREFIX "o"
# define PRIoLEAST16	__PRI16_PREFIX "o"
# define PRIoLEAST32	__PRI32_PREFIX "o"
# define PRIoLEAST64	__PRI64_PREFIX "o"

# define PRIoFAST8	__PRI8_PREFIX "o"
# define PRIoFAST16	__PRIFAST16_PREFIX "o"
# define PRIoFAST32	__PRI32_PREFIX "o"
 # define PRIoFAST64	__PRI64_PREFIX "o"

/* Unsigned integers.  */
# define PRIu8		__PRI8_PREFIX "u"
# define PRIu16		__PRI16_PREFIX "u"
# define PRIu32		__PRI32_PREFIX "u"
# define PRIu64		__PRI64_PREFIX "u"

# define PRIuLEAST8	__PRI8_PREFIX "u"
# define PRIuLEAST16	__PRI16_PREFIX "u"
# define PRIuLEAST32	__PRI32_PREFIX "u"
# define PRIuLEAST64	__PRI64_PREFIX "u"

# define PRIuFAST8	__PRI8_PREFIX "u"
# define PRIuFAST16	__PRIFAST16_PREFIX "u"
# define PRIuFAST32	__PRI32_PREFIX "u"
# define PRIuFAST64	__PRI64_PREFIX "u"

/* lowercase hexadecimal notation.  */
# define PRIx8		__PRI8_PREFIX "x"
# define PRIx16		__PRI16_PREFIX "x"
# define PRIx32		__PRI32_PREFIX "x"
# define PRIx64		__PRI64_PREFIX "x"

# define PRIxLEAST8	__PRI8_PREFIX "x"
# define PRIxLEAST16	__PRI16_PREFIX "x"
# define PRIxLEAST32	__PRI32_PREFIX "x"
# define PRIxLEAST64	__PRI64_PREFIX "x"

# define PRIxFAST8	__PRI8_PREFIX "x"
# define PRIxFAST16	__PRIFAST16_PREFIX "x"
# define PRIxFAST32	__PRI32_PREFIX "x"
# define PRIxFAST64	__PRI64_PREFIX "x"

/* UPPERCASE hexadecimal notation.  */
# define PRIX8		__PRI8_PREFIX "X"
# define PRIX16		__PRI16_PREFIX "X"
# define PRIX32		__PRI32_PREFIX "X"
# define PRIX64		__PRI64_PREFIX "X"

# define PRIXLEAST8	__PRI8_PREFIX "X"
# define PRIXLEAST16	__PRI16_PREFIX "X"
# define PRIXLEAST32	__PRI32_PREFIX "X"
# define PRIXLEAST64	__PRI64_PREFIX "X"

# define PRIXFAST8	__PRI8_PREFIX "X"
# define PRIXFAST16	__PRIFAST16_PREFIX "X"
# define PRIXFAST32	__PRI32_PREFIX "X"
# define PRIXFAST64	__PRI64_PREFIX "X"


/* Macros for printing `intmax_t' and `uintmax_t'.  */
# define PRIdMAX	__PRIMAX_PREFIX "d"
# define PRIiMAX	__PRIMAX_PREFIX "i"
# define PRIoMAX	__PRIMAX_PREFIX "o"
# define PRIuMAX	__PRIMAX_PREFIX "u"
# define PRIxMAX	__PRIMAX_PREFIX "x"
# define PRIXMAX	__PRIMAX_PREFIX "X"


/* Macros for printing `intptr_t' and `uintptr_t'.  */
# define PRIdPTR	__PRIPTR_PREFIX "d"
# define PRIiPTR	__PRIPTR_PREFIX "i"
# define PRIoPTR	__PRIPTR_PREFIX "o"
# define PRIuPTR	__PRIPTR_PREFIX "u"
# define PRIxPTR	__PRIPTR_PREFIX "x"
# define PRIXPTR	__PRIPTR_PREFIX "X"

/* Macros for scanning format specifiers.  */

/* Signed decimal notation.  */
# define SCNd8		__PRI8_PREFIX "d"
# define SCNd16		__PRI16_PREFIX "d"
# define SCNd32		__PRI32_PREFIX "d"
# define SCNd64		__PRI64_PREFIX "d"

# define SCNdLEAST8	__PRI8_PREFIX "d"
# define SCNdLEAST16	__PRI16_PREFIX "d"
# define SCNdLEAST32	__PRI32_PREFIX "d"
# define SCNdLEAST64	__PRI64_PREFIX "d"

# define SCNdFAST8	__PRI8_PREFIX "d"
# define SCNdFAST16	__PRIFAST16_PREFIX "d"
# define SCNdFAST32	__PRI32_PREFIX "d"
# define SCNdFAST64	__PRI64_PREFIX "d"

/* Signed decimal notation.  */
# define SCNi8		__PRI8_PREFIX "i"
# define SCNi16		__PRI16_PREFIX "i"
# define SCNi32		__PRI32_PREFIX "i"
# define SCNi64		__PRI64_PREFIX "i"

# define SCNiLEAST8	__PRI8_PREFIX "i"
# define SCNiLEAST16	__PRI16_PREFIX "i"
# define SCNiLEAST32	__PRI32_PREFIX "i"
# define SCNiLEAST64	__PRI64_PREFIX "i"

# define SCNiFAST8	__PRI8_PREFIX "i"
# define SCNiFAST16	__PRIFAST16_PREFIX "i"
# define SCNiFAST32	__PRI32_PREFIX "i"
# define SCNiFAST64	__PRI64_PREFIX "i"

/* Unsigned decimal notation.  */
# define SCNu8		__PRI8_PREFIX "u"
# define SCNu16		__PRI16_PREFIX "u"
# define SCNu32		__PRI32_PREFIX "u"
# define SCNu64		__PRI64_PREFIX "u"

# define SCNuLEAST8	__PRI8_PREFIX "u"
# define SCNuLEAST16	__PRI16_PREFIX "u"
# define SCNuLEAST32	__PRI32_PREFIX "u"
# define SCNuLEAST64	__PRI64_PREFIX "u"

# define SCNuFAST8	__PRI8_PREFIX "u"
# define SCNuFAST16	__PRIFAST16_PREFIX "u"
# define SCNuFAST32	__PRI32_PREFIX "u"
# define SCNuFAST64	__PRI64_PREFIX "u"

/* Octal notation.  */
# define SCNo8		__PRI8_PREFIX "o"
# define SCNo16		__PRI16_PREFIX "o"
# define SCNo32		__PRI32_PREFIX "o"
# define SCNo64		__PRI64_PREFIX "o"

# define SCNoLEAST8	__PRI8_PREFIX "o"
# define SCNoLEAST16	__PRI16_PREFIX "o"
# define SCNoLEAST32	__PRI32_PREFIX "o"
# define SCNoLEAST64	__PRI64_PREFIX "o"

# define SCNoFAST8	__PRI8_PREFIX "o"
# define SCNoFAST16	__PRIFAST16_PREFIX "o"
# define SCNoFAST32	__PRI32_PREFIX "o"
# define SCNoFAST64	__PRI64_PREFIX "o"

/* Hexadecimal notation.  */
# define SCNx8		__PRI8_PREFIX "x"
# define SCNx16		__PRI16_PREFIX "x"
# define SCNx32		__PRI32_PREFIX "x"
# define SCNx64		__PRI64_PREFIX "x"

# define SCNxLEAST8	__PRI8_PREFIX "x"
# define SCNxLEAST16	__PRI16_PREFIX "x"
# define SCNxLEAST32	__PRI32_PREFIX "x"
# define SCNxLEAST64	__PRI64_PREFIX "x"

# define SCNxFAST8	__PRI8_PREFIX "x"
# define SCNxFAST16	__PRIFAST16_PREFIX "x"
# define SCNxFAST32	__PRI32_PREFIX "x"
# define SCNxFAST64	__PRI64_PREFIX "x"


/* Macros for scanning `intmax_t' and `uintmax_t'.  */
# define SCNdMAX	__PRIMAX_PREFIX "d"
# define SCNiMAX	__PRIMAX_PREFIX "i"
# define SCNoMAX	__PRIMAX_PREFIX "o"
# define SCNuMAX	__PRIMAX_PREFIX "u"
# define SCNxMAX	__PRIMAX_PREFIX "x"

/* Macros for scaning `intptr_t' and `uintptr_t'.  */
# define SCNdPTR	__PRIPTR_PREFIX "d"
# define SCNiPTR	__PRIPTR_PREFIX "i"
# define SCNoPTR	__PRIPTR_PREFIX "o"
# define SCNuPTR	__PRIPTR_PREFIX "u"
# define SCNxPTR	__PRIPTR_PREFIX "x"

__BEGIN_DECLS

typedef struct __fc_imaxdiv_t
  {
    intmax_t quot;		/* Quotient.  */
    intmax_t rem;		/* Remainder.  */
  } imaxdiv_t;

/* ISO C: 7.8.2 */
/*@ 
  requires abs_representable: (intmax_t)(-c) != c ;
  assigns \result \from c ; 
*/
extern intmax_t imaxabs(intmax_t c);

/*@
  requires no_div_by_zero: denom != 0;
  requires no_overflow: denom != -1 || (intmax_t)(-numer) != numer;
  assigns \result \from numer, denom ;
  ensures correct_div: \result.quot == numer / denom;
  ensures correct_mod: \result.rem == numer % denom;
*/
extern imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom);

#include "__fc_define_wchar_t.h"
/*@ assigns \result \from nptr[..], base ;
  assigns endptr[..] \from nptr[..], base ;
  assigns __fc_errno \from nptr[..], base ;
*/
extern intmax_t strtoimax(const char * restrict nptr,
     char ** restrict endptr, int base);
extern uintmax_t strtoumax(const char * restrict nptr,
     char ** restrict endptr, int base);
extern intmax_t wcstoimax(const wchar_t * restrict nptr,
     wchar_t ** restrict endptr, int base);
extern uintmax_t wcstoumax(const wchar_t * restrict nptr,
     wchar_t ** restrict endptr, int base);

__END_DECLS

__POP_FC_STDLIB
#endif

