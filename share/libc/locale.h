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

#ifndef __FC_LOCALE
#define __FC_LOCALE
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

/* Structure giving information about numeric and monetary notation.  */
struct lconv
{
  /* Numeric (non-monetary) information.  */

  char *decimal_point;		/* Decimal point character.  */
  char *thousands_sep;		/* Thousands separator.  */
  /* Each element is the number of digits in each group;
     elements with higher indices are farther left.
     An element with value CHAR_MAX means that no further grouping is done.
     An element with value 0 means that the previous element is used
     for all groups farther left.  */
  char *grouping;

  /* Monetary information.  */

  /* First three chars are a currency symbol from ISO 4217.
     Fourth char is the separator.  Fifth char is '\0'.  */
  char *int_curr_symbol;
  char *currency_symbol;	/* Local currency symbol.  */
  char *mon_decimal_point;	/* Decimal point character.  */
  char *mon_thousands_sep;	/* Thousands separator.  */
  char *mon_grouping;		/* Like `grouping' element (above).  */
  char *positive_sign;		/* Sign for positive values.  */
  char *negative_sign;		/* Sign for negative values.  */
  char int_frac_digits;		/* Int'l fractional digits.  */
  char frac_digits;		/* Local fractional digits.  */
  /* 1 if currency_symbol precedes a positive value, 0 if succeeds.  */
  char p_cs_precedes;
  /* 1 iff a space separates currency_symbol from a positive value.  */
  char p_sep_by_space;
  /* 1 if currency_symbol precedes a negative value, 0 if succeeds.  */
  char n_cs_precedes;
  /* 1 iff a space separates currency_symbol from a negative value.  */
  char n_sep_by_space;
  /* Positive and negative sign positions:
     0 Parentheses surround the quantity and currency_symbol.
     1 The sign string precedes the quantity and currency_symbol.
     2 The sign string follows the quantity and currency_symbol.
     3 The sign string immediately precedes the currency_symbol.
     4 The sign string immediately follows the currency_symbol.  */
  char p_sign_posn;
  char n_sign_posn;
  /* 1 if int_curr_symbol precedes a positive value, 0 if succeeds.  */
  char int_p_cs_precedes;
  /* 1 iff a space separates int_curr_symbol from a positive value.  */
  char int_p_sep_by_space;
  /* 1 if int_curr_symbol precedes a negative value, 0 if succeeds.  */
  char int_n_cs_precedes;
  /* 1 iff a space separates int_curr_symbol from a negative value.  */
  char int_n_sep_by_space;
  /* Positive and negative sign positions:
     0 Parentheses surround the quantity and int_curr_symbol.
     1 The sign string precedes the quantity and int_curr_symbol.
     2 The sign string follows the quantity and int_curr_symbol.
     3 The sign string immediately precedes the int_curr_symbol.
     4 The sign string immediately follows the int_curr_symbol.  */
  char int_p_sign_posn;
  char int_n_sign_posn;
};

#include "__fc_define_null.h"
#include "__fc_string_axiomatic.h"

#define LC_CTYPE           0
#define LC_NUMERIC         1
#define LC_TIME            2
#define LC_COLLATE         3
#define LC_MONETARY        4
#define LC_MESSAGES        5
#define LC_ALL             6
#define LC_PAPER           7
#define LC_NAME            8
#define LC_ADDRESS         9
#define LC_TELEPHONE      10
#define LC_MEASUREMENT    11
#define LC_IDENTIFICATION 12

# define LC_CTYPE_MASK           (1 << LC_CTYPE)
# define LC_NUMERIC_MASK         (1 << LC_NUMERIC)
# define LC_TIME_MASK            (1 << LC_TIME)
# define LC_COLLATE_MASK         (1 << LC_COLLATE)
# define LC_MONETARY_MASK        (1 << LC_MONETARY)
# define LC_MESSAGES_MASK        (1 << LC_MESSAGES)
# define LC_PAPER_MASK           (1 << LC_PAPER)
# define LC_NAME_MASK            (1 << LC_NAME)
# define LC_ADDRESS_MASK         (1 << LC_ADDRESS)
# define LC_TELEPHONE_MASK       (1 << LC_TELEPHONE)
# define LC_MEASUREMENT_MASK     (1 << LC_MEASUREMENT)
# define LC_IDENTIFICATION_MASK  (1 << LC_IDENTIFICATION)
# define LC_ALL_MASK             (LC_CTYPE_MASK                 \
                                  | LC_NUMERIC_MASK             \
                                  | LC_TIME_MASK                \
                                  | LC_COLLATE_MASK             \
                                  | LC_MONETARY_MASK            \
                                  | LC_MESSAGES_MASK            \
                                  | LC_PAPER_MASK               \
                                  | LC_NAME_MASK                \
                                  | LC_ADDRESS_MASK             \
                                  | LC_TELEPHONE_MASK           \
                                  | LC_MEASUREMENT_MASK         \
                                  | LC_IDENTIFICATION_MASK      \
                                  )

extern struct lconv* __frama_c_locale;
extern char*__frama_c_locale_names[];

/*@ 
  requires locale == \null || valid_read_string(locale);
  assigns __frama_c_locale \from category, locale[..];
  assigns \result \from __frama_c_locale,category, locale[..];
  ensures \result==\null
   || (\valid(\result) 
      && \exists ℤ i ; \result == __frama_c_locale_names[i]) ;
*/
extern char *setlocale(int category, const char *locale);

/*@ assigns \nothing;
  ensures \result == __frama_c_locale;
 */
extern struct lconv *localeconv(void);

__END_DECLS

__POP_FC_STDLIB
#endif
