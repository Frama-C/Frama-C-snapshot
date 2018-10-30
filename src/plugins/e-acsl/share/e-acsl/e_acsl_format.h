/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2018                                               */
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
 * \file  e_acsl_format.h
 * \brief Validating format strings with respect to arguments and their types
 *
 * Detection of format string vulnerabilities and other violations
 * related to improper use of formats in printf-like functions are addressed
 * as follows. Each call to a format function `f` (e.g. printf)
 * is replaced by a call to an analysis function `f'`. The signature of `f'` is
 * similar to that of `f'` except it has an additional argument. This argument
 * is a literal string where each character describes the type of a variadic
 * argument in the original call to `f`.
 *
 * For instance:
 *  `printf("Str=%s, Int=%d ", str, num);`
 * is replaced by
 *  `__e_acsl_builtin_printf("sd", "Str=%s, Int=%d", str, num);`
 * Note the first argument "sd". It indicates that `printf` was invoked
 * with two variadic arguments of types `char*` (specified via 's')
 * and `int` (`d`). Such single-character types are further called
 * |abbreviated| types. See ::abbr2str function for details.
 *
 * Execution of __e_acsl_builtin_printf checks that
 *  - format string is a NUL-terminated C string
 *  - all directives in the format string are well-formed (as per C99 standard)
 *  - each formatting directive has a corresponding variadic argument.
 *    Excessive arguments (for which there are no directives) are allowed
 *    but otherwise ignored.
 *  - the types of variadic arguments provided via a call match the types
 *    expected by the respective format directives. This check includes checking
 *    for signedness. For instance,
 *      __e_acsl_builtin_printf("d", "%u", n);
 *    will abort because the formatting directive expects its argument to be an
 *    unsigned integer, whereas `n` is a signed integer (indicated by "d") in
 *    the first argument to `__e_acsl_builtin_printf`. Bear in mind though that
 *    char, short, and float types are the subjects to default promotions. That
 *    is, `char` and `short` are promoted to `int` and `float` is promoted to
 *    double. Frama-C enforces such promotions by adding explicit casts.
 *  - variadic arguments corresponding to `%s` conversion specifiers describe
 *    valid C strings (NUL-terminated arrays of characters belonging to program
 *    allocation)
 *  - variadic arguments corresponding to `%n` conversion specifiers describe
 *    valid integer pointers
 * Execution of __e_acsl_builtin_dprintf additionally checks that
 *  - the file descriptor designated for writing is open
 * Execution of __e_acsl_builtin_fprintf additionally checks that
 *  - the stream designated for writing is valid
 * Execution of __e_acsl_builtin_sprintf and  __e_acsl_builtin_sprintf
 * additionally check that
 *  - memory buffers designated for writing are allocated, writable and provide
 *    sufficient space for storing the results
***************************************************************************/

#ifndef E_ACSL_FORMAT_H
#define E_ACSL_FORMAT_H

#ifndef E_ACSL_VALIDATE_FORMAT_STRINGS
# define E_ACSL_FORMAT_VALIDITY_DESC "disabled"
#else
# define E_ACSL_FORMAT_VALIDITY_DESC "enabled"

#include <stdlib.h>
#include <features.h>
#include <stdint.h>
#include <stdio.h>
#include <wctype.h>
#include <fcntl.h>

#define FMT_ERROR "Format error: "
#define INT_ERROR "Internal error: "

/* Check whether a value is unsigned or not.
   Watch out for integer promotions. */
#define UNSIGNED(n) (n >= 0 && ~n >= 0)

/* Abbreviated types {{{ */

/* An abbreviated type is a character that describes a given primitive or a
   pointer type that may be expected by a formatting directive of a
   function such as `printf`. Correspondance between abbreviated and actual
   types is given via macro ::abbreviated_types.

   Abbreviated type values are ad-hoc with one convention:
   any given signed integral (or integral pointer type) is given by a lower-case
   letter, whereas an unsigned type is given by the same but upper-case letter.
   For instance, 'd' corresponds to `int` and 'D' corresponds to `unsigned int`.
   Similarly, 'i' corresponds to `int*` and `I` corresponds to `unsigned int*`.

   NOTE: Abbreviated types are generated by the instrumentation engine
   (see definition of the `get_printf_argument_str` in `functions.ml`).
   It is therefore important that the types as they are used by this file
   and by `functions.ml` are in sync */

#define abbreviated_types \
  typedef_abbreviated('c', char, IChar) \
  typedef_abbreviated('C', unsigned char, IUChar) \
  typedef_abbreviated('h', short, IShort) \
  typedef_abbreviated('H', unsigned short, IUShort) \
  typedef_abbreviated('d', int, IInt) \
  typedef_abbreviated('D', unsigned int, IUInt) \
  typedef_abbreviated('l', long, ILong) \
  typedef_abbreviated('L', unsigned long, IULong) \
  typedef_abbreviated('r', long long, ILongLong) \
  typedef_abbreviated('R', unsigned long long, IULongLong) \
  typedef_abbreviated('f', float, FFloat) \
  typedef_abbreviated('e', double, FDouble) \
  typedef_abbreviated('E', long double, FLongDouble) \
  typedef_abbreviated('s', char*, PChar) \
  typedef_abbreviated('S', unsigned char*, PUChar) \
  typedef_abbreviated('q', short*, PShort) \
  typedef_abbreviated('Q', unsigned short*, PUShort) \
  typedef_abbreviated('i', int*, PInt) \
  typedef_abbreviated('I', unsigned int*, PUInt) \
  typedef_abbreviated('z', long*, PLong) \
  typedef_abbreviated('Z', unsigned long*, PULong) \
  typedef_abbreviated('w', long long*, PLongLong) \
  typedef_abbreviated('W', unsigned long long*, PULongLong) \
  typedef_abbreviated('p', void*, PVoid)

/* Define abbreviated types as enum so they can be referred to as aliases */
typedef enum {
#define typedef_abbreviated(type,val,alias) alias = type,
  abbreviated_types
#undef typedef_abbreviated
} abbrev_t;

/** \brief Return a C-string representation of a given abbreviated type */
static const char* abbr2str(abbrev_t type) {
  switch(type) {
#define typedef_abbreviated(type,val,alias) case type: return #val;
  abbreviated_types
#undef typedef_abbreviated
  }
  return '\0';
}

/** \brief Return a byte-size of a given abbreviated type */
static int abbr2size(abbrev_t type) {
  switch(type) {
#define typedef_abbreviated(type,val,alias) case type: return sizeof(val);
  abbreviated_types
#undef typedef_abbreviated
  }
  return 0;
}

/** \brief Some format directives expect typedefs rather than actual types.
    For instance '%jd' expects `intmax_t`. The instrumentation engine "unrolls"
    types, that is,  we will be given an actual primitive type instead (say
    long). While we cannot reason about typedefs dynamically we infer types
    corresponding to typedefs based on their size and sign. The following
    function establishes some loose correspondence between sizes and signs and
    abbreviated integer types corresponding to these sizes and signs */
static abbrev_t size2abbri(int size, int sign) {
  if (size <= sizeof(int))
    return sign ? IInt : IUInt;
  else if (size == sizeof(long int))
    return sign ? ILong : IULong;
  else if (size == sizeof(long long int))
    return sign ? ILongLong : IULongLong;
  vabort(INT_ERROR "integral type corresponding to size %d unknown", size);
  return '\0';
}

/** \brief Same as above but for floating point types */
static abbrev_t size2abbrf(int size) {
  if (size == sizeof(float))
    return FFloat;
  else if (size == sizeof(double))
    return FDouble;
  else if (size == sizeof(long double))
    return FLongDouble;
  vabort
    (INT_ERROR "floating point type corresponding to size %d unknown", size);
  return '\0';
}

/* Partial mapping of primitive abreviated type to a pointer of the same type,
   e.g., 'd' (int) -> 's' (*) */
static char abbr2ptr(char c) {
  switch(c) {
    case IInt: return PInt;
    case IUInt: return PUInt;
    case ILong: return PLong;
    case IULong: return PULong;
    case ILongLong: return PLongLong;
    case IULongLong: return PULongLong;
    default:
      vabort(INT_ERROR "Unexpected abbreviated type %c\n", c);
  }
  return '\0';
}
/* }}} */

/* Format string character classes {{{ */

/* Length modifier characters */
const char *length_chars = "hljztL";

/* Flag characters */
const char *flag_chars = "-+ #0'";

/* Conversion specifier characters. '%' is treated specially */
const char *specifier_chars = "diouxXfFeEgGaAcspn";

/* Period character */
const char *period_chars = ".";

/* Character map allowing to quickly identify the class of a character (above).
   Do not use this map directly, use one of the below macros. */
char format_char_map[256];

#define specifier_id  's'
#define length_id     'l'
#define flag_id       'f'
#define period_id     'r'

#define is_specifier_char(_c) (format_char_map[(int)_c] == specifier_id)
#define is_flag_char(_c) (format_char_map[(int)_c] == flag_id)
#define is_length_char(_c) (format_char_map[(int)_c] == length_id)
#define is_period_char(_c) (format_char_map[(int)_c] == period_id)

static void set_format_char_map() {
  int init_idx = (int)'\0';
  int init_char = 'i';
  if (format_char_map[init_idx] != init_char) {
    memset(format_char_map, 0, 256);
    int i;
    for(i = 0; i < strlen(specifier_chars); i++)
      format_char_map[(int)specifier_chars[i]] = specifier_id;
    for(i = 0; i < strlen(length_chars); i++)
      format_char_map[(int)length_chars[i]] = length_id;
    for(i = 0; i < strlen(flag_chars); i++)
      format_char_map[(int)flag_chars[i]] = flag_id;
    for(i = 0; i < strlen(period_chars); i++)
      format_char_map[(int)period_chars[i]] = period_id;
    format_char_map[init_idx] = init_char;
  }
}
/* }}} */

/* Argument specification {{{ */
typedef struct {
  char *format; /* Pointer to the complete format string */
  char directive [16]; /* Directive string */
  struct arg {
    int index;  /* Argument index this specification refers to */
    _Bool expl; /* Set to 1 if the argument has been numbered via $ */
  } arg;
  struct flags { /* Flags */
    _Bool specified; /* set if any of the below flags are set */
    _Bool minus;  /* - */
    _Bool plus;   /* + */
    _Bool space;  /* ' ' */
    _Bool hash;   /* # */
    _Bool zero;   /* 0 */
    _Bool apostroph; /* ' */
  } flags;
  int field_width; /* Field width, INT_MIN if not given, -1 if '*' */
  int precision; /* Format precision, INT_MIN if not given, -1 if '*' */
  struct length { /* Length Modifier */
    int bytes; /* Number of bytes inferred from length modifier */
    char mod;  /* Modifier character (such as 'l' or 'h') */
    _Bool extended; /* Set if modifier is repeated (e.g., 'll' or 'hh') */
  } length;
  char specifier; /* Format specifier character */
} format_directive;
/* }}} */

/* Debug print {{{ */
static void print_directive(format_directive *dir, char *rem) {
  rtl_printf("  Format:      %s\n", dir->format);
  rtl_printf("  Specifier:   %c\n", dir->specifier);
  rtl_printf("  Arg:         \n");
  rtl_printf("    Index:    $d\n", dir->arg.index);
  rtl_printf("    Explicit: $d\n", dir->arg.expl);
  rtl_printf("  Precision:   .%d\n", dir->precision);
  rtl_printf("  Field width: %d\n", dir->field_width);
  rtl_printf("  Length: \n");
  rtl_printf("    Modifier: %c \n", dir->length.mod);
  rtl_printf("    Bytes:    %d \n", dir->length.bytes);
  rtl_printf("    Extended: %d \n", dir->length.extended);
  rtl_printf("  Flags:       <");
#define print_format_flag(_f,_s) if (dir->flags._f) rtl_printf(_s)
  print_format_flag(plus,"+");
  print_format_flag(minus, "-");
  print_format_flag(space, " ");
  print_format_flag(hash,"#");
  print_format_flag(apostroph,"'");
  print_format_flag(zero,"0");
#undef print_format_flag
  rtl_printf(">\n");
  rtl_printf("  Remainder: \"%s\"\n", rem);
}
/* }}} */

/* Fetch format argument number {{{
   Most format specifications allow for argument numbers to be specified. E.g.,
     printf("%2$*1$d", width, num);
   The above will first print num and then width. Even though this is not in
   C99, we allow that as many `printf` implementations have this extension.
   The feature comes from the Single UNIX Specification.

   This function assumes that *(fmt-1) is '%' */
static char *fetch_format_argno(char *fmt, format_directive *dir,
    format_directive *prev, int ind) {
  int argno = 0;
  char *ret = fmt;

  /* If *fmt is a non-zero digit then it is either an argument number
     (if followed by '$') or a field width. */
  if (*ret != '0') {
    while (isdigit(*ret)) {
      argno = argno * 10 + (*ret - '0');
      ret++;
    }
  } else {
    /* The only case when zero is found is when it is a flag, just return. */
    return ret;
  }

  /* If '$' is found then it was an argument number, save its index */
  if (*ret == '$') {
    dir->arg.index = argno - 1;
    dir->arg.expl = 1;
    ret++;
  /* ... do nothing otherwise but revert back to original format as some
     characters may have been fetched */
  } else {
    dir->arg.index = ind;
    dir->arg.expl = 0;
    ret = fmt;
  }

  /* make sure that numbered and non-numbered directives are not mixed */
  if (prev && prev->arg.expl != dir->arg.expl)
    vabort(FMT_ERROR
      "\"%s\":  numbered and non-numbered directives cannot be mixed\n",
      dir->format);

  return ret;
}
/* }}} */

/* Fetch format flags {{{ */
/* Assumes that `fmt` is a format string returned by ::fetch_format_argno */
static char *fetch_format_flags(char *fmt, format_directive *dir) {
#define set_format_flag(_f) \
  if (!dir->flags._f) \
    { dir->flags._f = 1; } \
  else  \
    { vabort(FMT_ERROR "flag %s has already been set", #_f); }

  while (is_flag_char(*fmt)) {
    dir->flags.specified = 1;
    switch(*fmt) {
      case '#':
        set_format_flag(hash);
        break;
      case '-':
        set_format_flag(minus);
        break;
      case '+':
        set_format_flag(plus);
        break;
      case ' ':
        set_format_flag(space);
        break;
      case '\'':
        set_format_flag(apostroph);
        break;
      case '0':
        set_format_flag(zero);
        break;
    }
    fmt++;
  }
#undef set_format_flag
  return fmt;
}
/* }}} */

/* Fetch field width {{{ */
/* Assumes that `fmt` is a format string returned by ::fetch_format_flags */
static char *fetch_format_field_width(char *fmt, format_directive *dir) {
  dir->field_width = INT_MIN;
  /* Field width is either an asterisk ... */
  int len = 0;
  if (*fmt == '*') {
    dir->field_width = -1;
    fmt++;
  } else { /* ... or a positive decimal integer */
    if (isdigit(*fmt)) {
      if (*fmt == '0') {
        vabort(FMT_ERROR
          "field width in format cannot start with zero (%s)\n", dir->format);
      };
      while (isdigit(*fmt)) {
        len = len * 10 + (*fmt - '0');
        fmt++;
      }
      dir->field_width = len;
    }
  }
  return fmt;
} /* }}} */

/* Fetch format precision {{{ */
/* Assumes that `fmt` is a format string returned by
   ::fetch_format_field_width */
static char *fetch_format_precision(char *fmt, format_directive *dir) {
  /* Precision is given in the form '.' optionally followed by either
    '*' or a digit string. */
  dir->precision = INT_MIN;
  if (is_period_char(*fmt)) {
    fmt++;
    if (*fmt == '*') {
      dir->precision = -1;
      fmt++;
    } else {
      dir->precision = 0;
      while (isdigit(*fmt)) {
        dir->precision = dir->precision * 10 + (*fmt - '0');
        fmt++;
      }
    }
  }
  return fmt;
} /* }}} */

/* Fetch format length modifier and format specifier {{{ */
/* Assumes that `fmt` is a format string returned by ::fetch_format_precision */
static char *fetch_format_length(char *fmt, format_directive *dir) {
  dir->length.bytes = 0;
  dir->length.mod = '\0';
  dir->length.extended = 0;

  switch(*fmt) {
    case 'h': {
      if (*(fmt + 1) == 'h') {
        dir->length.bytes = sizeof(char);
        dir->length.extended = 1;
        fmt++;
      } else
        dir->length.bytes = sizeof(short);
      break;
    }
    case 'l': {
      if (*(fmt + 1) == 'l') {
        dir->length.bytes = sizeof(long long);
        dir->length.extended = 1;
        fmt++;
      } else
        dir->length.bytes = sizeof(long);
      break;
    }
    case 'j':
      dir->length.bytes = sizeof(intmax_t);
      break;
    case 'z':
      dir->length.bytes = sizeof(size_t);
      break;
    case 't':
      dir->length.bytes = sizeof(ptrdiff_t);
      break;
    case 'L':
      dir->length.bytes = sizeof(long double);
      break;
  }

  /* Make sure that the length modifier (if there is one) belongs
     to a right character class */
  vassert(dir->length.mod == '\0' || is_length_char(dir->length.mod),
    INT_ERROR "Bad length modifier: '%c'\n", dir->length.mod);

  if (dir->length.bytes) {
    dir->length.mod = *fmt;
    fmt++;
  }

  /* fmt now points to a conversion specifier, get it. */
  dir->specifier = *fmt;

  /* Detect specifying length modifiers twice. This is purely for better error
     reporting. Even if there is no this check, the one below detects it,
     but with a different error message. */
  if ( dir->specifier )

  /* which has been fetched at the start matches the one we have arrived at */
  if (!is_specifier_char(dir->specifier)) {
    if (dir->specifier == '%')
      vabort(FMT_ERROR "in directive '%s'."
          "the complete conversion specification for '%%' is '%%%%'\n",
          dir->format);
    else
      vabort(FMT_ERROR "illegal format specifier '%c'\n", dir->specifier);
  }
  return ++fmt;
} /* }}} */

/* Parse format string {{{ */
/* Parse format string into a NULL-terminated array of directives */
static format_directive ** get_format_directives(char *fmt) {
  /* Count the number of formatting directives in the format string
     by counting '%' occurrences. Yes, it may give more specifications than
     needed (e.g., "%%") but allocating space for a few extra pointers does not
     hurt considering that in order to do that properly we need to parse format
     string twice. */

  int sz = charcount(fmt, '%') + 1;
  format_directive ** directives =
    private_malloc(sizeof(format_directive *) * sz);
  char *format_string = fmt;

  /* Nullify all pointers to make sure there is no leftover rubbish */
  int i;
  for (i = 0; i < sz; i++)
    directives[i] = NULL;

  /* Set character map so format characters can be recognized */
  set_format_char_map();
  char ch;
  i = 0;
  format_directive *prev = NULL;
  while ((ch = *fmt++) != '\0') {
    if (ch == '%') {
      /* Do not take into account '%%' specification. It has no corresponding
         arguments, flags or any other attributes. */
      if (*fmt == '%') {
        ++fmt;
        continue;
      }
      /* Allocate space for `format_directive` to hold the result of parsing */
      format_directive *dir = private_calloc(1, sizeof(format_directive));
      /* Parse format string */
      dir->format = format_string;
      char *fmt_start  = fmt - 1;
      fmt = fetch_format_argno(fmt, dir, prev, i);
      fmt = fetch_format_flags(fmt, dir);
      fmt = fetch_format_field_width(fmt, dir);
      fmt = fetch_format_precision(fmt, dir);
      fmt = fetch_format_length(fmt, dir);
      /* Save format string in the dir struct. Rather ugly but the RTL
        printf library has no `snprintf` or extensions allowing to print
        `N` characters. */
      ptrdiff_t fmt_len = fmt - fmt_start;
      ptrdiff_t max_len = sizeof(dir->directive) - 1;
      int len = max_len > fmt_len ? fmt_len : max_len;
      strncpy(dir->directive, fmt_start, len);
      dir->directive[len+1] = '\0';
      /* Save the directive */
      directives[i++] = dir;
      prev = dir;
      /* print_directive(dir, fmt); */
    }
  }
  return directives;
}

static void release_directives(const char *fmt, format_directive ** dirs) {
  int formats = charcount(fmt, '%') + 1;
  int i;
  for (i = 0; i < formats; i++)
    private_free(dirs[i]);
  private_free(dirs);
}
/* }}} */

/* Format string validation (well-formedness) {{{ */
static inline void validate_application(format_directive *dir, char *allowed,
  char* kind, char *desc) {
  vassert(strchr(allowed, dir->specifier) != '\0', FMT_ERROR
    "wrong application of %s [%s] to format specifier [%c]\n",
    desc, kind, dir->specifier);
}

/** \brief Check that a given format specifier are used with right flags,
    precision, field width, and length modifier. */
static void validate_applications(format_directive *dir) {
  /* ==== Flags ==== */
  char *desc = "flag";

  /* ' flag separates thousands by commas. It is applicable only to
     i, d, u, f, F, g, or G conversion specifiers. For other specifiers
     its behaviour is undefined. */
  if (dir->flags.apostroph)
    validate_application(dir, "idufFgG", "\\", desc);

  /* # flag converts a value to an alternative form. It is applicable only to
     x, X, a, A, e, E, f, F, g, and G conversion specifiers. */
  if (dir->flags.hash)
    validate_application(dir, "oxXaAeEfFgG", "#", desc);

  /* 0 flag pads values with zeroes. It is applicable only to
     d, i, o, u, x, X, a, A, e, E, f, F, g, and G conversion specifiers */
  if (dir->flags.zero)
    validate_application(dir, "diouxXaAeEfFgG", "0", desc);

  /* No flags should be used if 'n' specifier is given */
  if (dir->flags.specified && dir->specifier == 'n')
    vabort(FMT_ERROR "one of more flags with [n] specifier", NULL);

  /* ==== Precision ==== */
  desc = "precision";

  /* Precision gives the minimum number of digits to appear for the
     d, i, o, u, x, and X conversion specifiers; the number of digits
     to appear after the period character for the a, A, e, E, f, and F
     conversion specifiers; the maximum number of significant digits for the
     g and G conversion specifiers; or the maximum number of bytes to be
     printed from a string in the s and S conversion specifiers. */
  if (dir->precision != INT_MIN)
    validate_application(dir, "diouxXaAeEfFgGs", ".", desc);

  /* ==== Field width ==== */
  desc = "field width";

  if (dir->specifier == 'n' && dir->field_width != INT_MIN)
    vabort(FMT_ERROR "field width used with [n] specifier", NULL);

  /* ==== Length modifiers ==== */
  desc = "length modifier";

  /* Most length modifiers (all except 'L') apply to d, i, o, u, x, or X
     conversion specifiers. 'L' applies to a, A, e, E, f, F, g, or G. */
  if (dir->length.mod != '\0') {
    /* Make sure the length specifier is one of the allowed ones */
    vassert(is_length_char(dir->length.mod),
      FMT_ERROR "bad length specifier [%c]\n", dir->length.mod);

    /* Conver length modifier to a string */
    char lm_kind[3];
    int i = 0;
    lm_kind[i++] = dir->length.mod;
    if (dir->length.extended)
      lm_kind[i++] = dir->length.mod;
    lm_kind[i++] = '\0';

    switch (dir->length.mod) {
      case 'l':
        if (!dir->length.extended) {
          validate_application(dir, "diouxXncsfFeEgGaA", lm_kind, desc);
          break;
        }
      /* No need to look whether 'h' is extended, both 'h' and 'hh' are only
        applicable to [diouxXn] */
      case 'h':
      case 'j':
      case 'z':
      case 't':
        validate_application(dir, "diouxXn", lm_kind, desc);
        break;
      case 'L':
        validate_application(dir, "aAeEfFgG", lm_kind, desc);
        break;
      default:
        vabort(INT_ERROR "unexpected length modifier %c\n", lm_kind);
    }
  }
}
/* }}} */

/* Format validation (arguments) {{{ */

/** \brief d, i, o, u, x and X format specifiers expect either `int` or
   `unsigned int`. This, however, can be changed using length modifiers. For
   instance, "%d" expects `int`, "%ld" expects `long`. The following function
   computes an integer type that a format expects using length modifiers and
   format specifiers. */
static abbrev_t infer_integral_abbr(format_directive *dir, int sgn) {
  switch(dir->length.mod) {
    case 'l': /* expects long long or long */
      return (dir->length.extended) ?
        (sgn ? ILongLong : IULongLong) : (sgn ? ILong : IULong);
    case 'h': /* short/char: promoted to int */
      return IInt;
    case 'j': /* intmax_t */
      return size2abbri(sizeof(intmax_t), sgn);
    case 'z': /* size_t */
      return size2abbri(sizeof(size_t), sgn);
    case 't': /* ptrdiff_t */
      return size2abbri(sizeof(ptrdiff_t), sgn);
    case '\0':
      return (sgn ? IInt : IUInt);
  }
  vabort(INT_ERROR "unexpected length modifier: '%c'\n", dir->length.mod);
  return '\0';
}

/** \brief Same as above but for 'n' conversion specifier. */
static abbrev_t infer_n_abbr(format_directive *dir) {
  char c;
  switch(dir->length.mod) {
    case 'h':
      return dir->length.extended ? PChar : PShort;
    case 'l':
      return dir->length.extended ? PLongLong : PLong;
    case 'j': /* intmax_t: signed type */
      c = size2abbri(sizeof(intmax_t), 1);
      return abbr2ptr(c);
    case 'z': /* size_t: unsigned type */
      c = size2abbri(sizeof(size_t), 0);
      return abbr2ptr(c);
    case 't': /* ptrdiff_t: signed type */
      c = size2abbri(sizeof(ptrdiff_t), 1);
      return abbr2ptr(c);
    case '\0':
      return PInt;
    default:
      vabort(INT_ERROR "unexpected length modifier '%c'\n", dir->length.mod);
  }
  return '\0';
}

/** \brief Check that a given abbreviated type (`expected_t`) matches the
   abbreviated type in the format description (`fmtdesc`) and abort the
   execution if it is not so.

   This function expects that the index given via the format directive
   `dir` is less than or equal to the length of `fmtdesc`, i.e., there is an
   actual argument that corresponds to `dir`. */
static void validate_format_type
  (abbrev_t expected_t, const char *fmtdesc,
   format_directive *dir, const char *func)
{
  abbrev_t actual_t = fmtdesc[dir->arg.index];
  if (actual_t != expected_t) {
    vabort("%s: directive %d ('%s') expects argument of type '%s'"
      " but the corresponding argument has type '%s'\n",
      func, dir->arg.index + 1, dir->directive,
      abbr2str(expected_t), abbr2str(actual_t));
  }
}

/** \brief Validate that a C string used with 's' modifier belongs to allocation
     and has a NUL-terminator. Takes precision into account.

   @param s - address of the string
   @param dir - formatting directive
   @param func - name of the function (e.g., printf)
   @param wide - if set to a true value then the string should be treated as
    a wide string (wchar_t*)  */
static long validate_format_string_argument
  (char *s, format_directive *dir, const char *func, int wide)
{
  int limit = (dir->precision >= 0) ? dir->precision : -1;
  long size =
    (wide) ? valid_nwstring((wchar_t*)s, limit, 0) : valid_nstring(s, limit, 0);
  switch(size) {
    case -1:
      vabort
        ("%s: attempt to access unallocated memory via directive %d ('%s')\n",
        func, dir->arg.index + 1, dir->directive);
    case -2:
      vabort(INT_ERROR
        "%s: writeable check unexpectedly failed in directive %d ('%s')\n",
        func, dir->arg.index + 1, dir->directive);
    case -3:
      vabort("%s: attempt to access partially unallocated memory "
        "via directive %d ('%s')\n",
        func, dir->arg.index + 1, dir->directive);
    case -4:
      vabort("%s: unterminated string in directive %d ('%s')\n",
        func, dir->arg.index + 1, dir->directive);
  }
  return size;
}

/** \brief Check that a buffer of a given length overlaps with the memory space
     of a formatting directive argument. */
static void validate_overlapping_buffer(char *buffer, size_t buf_sz, void *arg,
    size_t arg_sz, const char *func, format_directive *dir) {
  if (buffer) {
    if (!disjoint_spaces((uintptr_t)buffer, buf_sz, (uintptr_t)arg, arg_sz))
      vabort("%s: output buffer overlaps with argument %d (%s)\n",
          func, dir->arg.index + 1, dir->directive);
  }
}

/** \brief Main format validation function that parses the format string and
   validates each format directive against the provided format description.
   If any of the requirements are violated the the program is aborted.

   @param fmtdesc - format description, a C string of abbreviated types
    that describes the types of actual argument a call to a formatting function
    has been made with
   @param fmt - format string
   @param ap - va_list of arguments to a formatting function
   @param func - symbolic name of a formatting function used
   @param buffer - buffer to write (in case of sprintf/snprintf, NULL otherwise)
   @param buffer - buffer limit */
static void validate_format
  (const char *fmtdesc, const char *fmt,
  va_list ap, const char *func, char *buffer, size_t buf_size)
{

  /* Check that format string is valid first */
  if (valid_string((char*)fmt, 0) < 0)
    vabort("%s: invalid format string (unallocated or unterminated)\n", func);

  /* Parse format string and generate format directives */
  format_directive ** dirs = get_format_directives((char*)fmt);
  format_directive ** dirs_c = dirs; /* extra alias for passing it to `free` */

  /* Track addresses of variadic arguments */
  int arglen = strlen(fmtdesc); /* number of variadic arguments */
  void *args[arglen];
  int i;
  for (int i = 0; i < arglen; i++)
    args[i] = va_arg(ap, void*);
  va_end(ap);

  /* Validate each generated directive */
  while (*dirs) {
    format_directive *dir = *dirs;
    validate_applications(dir); /* Check that the directive is well formed */
    int argno = dir->arg.index;

    /* Make sure there is a sufficient number of arguments provided: for each
       format directive in the format string there should be a corresponding
       argument except for literal directive `%%` */
    if (argno >= arglen)
      vabort("%s: directive %d (%s) in format \"%s\" has no argument\n",
        func, dir->arg.index + 1, dir->directive, dir->format);

/* Shortcut for `validate_format_type` function */
#define validate_type(_t) validate_format_type(_t, fmtdesc, dir, func)

    uintptr_t addr = (uintptr_t)args[argno]; /* Address of the argument */
    char expected_t; /* Placeholder for the type expected by the directive */

    switch(dir->specifier) {
      case 'd': /* signed integer */
      case 'i':
        expected_t = infer_integral_abbr(dir, 1);
        validate_type(expected_t);
        break;
      case 'o': /* unsigned integer */
      case 'u':
      case 'x': case 'X':
        expected_t = infer_integral_abbr(dir, 0);
        validate_type(expected_t);
        break;
      case 'f': case 'F': /* double */
      case 'e': case 'E':
      case 'g': case 'G':
      case 'a': case 'A':
        /* All floating point modifiers (aAeEfFgG) expect doubles except for
           the case when 'L' length modifier is given in which case it expects
           long double. Any other length modifier leads to an undefined
            behaviour. Checking that does not happen is done in
           ::validate_applications */
        expected_t = (dir->length.mod == 'L') ? FLongDouble : FDouble;
        validate_type(expected_t);
        break;
      case 'c': /* character */
        /* On all occasions 'c' expects an `int`. This is because `char` is
           always promoted. However, in case `l` length modifier is specified
           (i.e., "%lc") then it expects an argument be of type `wint_t` and
           can either be signed or unsigned. So let's compute it! */
        if (dir->length.mod == 'l') {
          wint_t wi = 1;
          int sign = UNSIGNED(wi);
          expected_t = size2abbri(sizeof(wint_t), !sign);
        } else
          expected_t = IInt;
        validate_type(expected_t);
        break;
      case 's': { /* character string */
        int wide = (dir->length.mod == 'l');
        if (wide) { /* same as with %lc, compute sign of wchar_t */
          wchar_t wi = 1;
          int sign = UNSIGNED(wi);
          expected_t = size2abbri(sizeof(wint_t), !sign);
          expected_t = abbr2ptr(expected_t);
        } else
          expected_t = PChar;
        validate_type(expected_t);
        /* Check that a string is valid */
        int asz = validate_format_string_argument((char*)addr, dir, func, wide);
        validate_overlapping_buffer
          (buffer, buf_size, (void*)addr, asz, func, dir);
        break;
      }
      case 'p':
        validate_type(PVoid);
        if (!allocated(addr, 1, addr))
          vabort("%s: argument %d of directive %s not allocated\n", func,
            argno + 1, dir->directive);
        validate_overlapping_buffer
          (buffer, buf_size, (void*)addr, 1, func, dir);
        break;
      case 'n': {
        expected_t = infer_n_abbr(dir);
        validate_type(expected_t);
        /* 'n' modifier writes the number of bytes corresponding to characters
           written by a function so far to a pointer of an integral type. Make
           sure that the provided pointer corresponds to writeable memory. */
        int size = dir->length.bytes == 0 ?  sizeof(int) : dir->length.bytes;
        if (!writeable(addr, size, addr))
          vabort("%s: argument %d of directive %s not allocated or writeable\n",
            func, argno, dir->directive);
        validate_overlapping_buffer
          (buffer, buf_size, (void*)addr, size, func, dir);
        break;
      }
      default:
        vabort(INT_ERROR "Unexpected format specifier '%c'\n", dir->specifier);
    }
    dirs++;
#undef validate_type
  }
  release_directives(fmt, dirs_c);
}
/* }}} */

/* Printf and friends {{{ */
int builtin_printf(const char *fmtdesc, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  validate_format(fmtdesc, fmt, ap, "printf", NULL, 0);
  va_start(ap, fmt);
  return vprintf(fmt, ap);
}

int builtin_fprintf(const char *fmtdesc, FILE *stream, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  /* First check that stream belongs to allocated space */
  if (!allocated((uintptr_t)stream, 1, (uintptr_t)stream))
    vabort("fprintf: attempt to write to an invalid stream\n", NULL);
  /* Check if stream is a valid stream and that is open */
  int fd = fileno(stream);
  if (fd == -1)
    vabort("fprintf: attempt to write to an invalid stream\n", NULL);
  /* Since we have a file descriptor check if it is open.
     This may not be really necessary since `fileno` should be able to tell us
     that. Does not hurt though. That's a fast check.
     It should be noted that `fileno` is a POSIX function and not in the C99
     standard. */
  if (fcntl(fd, F_GETFD) == -1)
    vabort("fprintf: attempt to write to a closed stream\n", NULL);

  validate_format(fmtdesc, fmt, ap, "fprintf", NULL, 0);
  va_start(ap, fmt);
  return vfprintf(stream, fmt, ap);
}

int builtin_dprintf(const char *fmtdesc, int fd, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  /* Make sure that the designated file descriptor is open */
  if (fcntl(fd, F_GETFD) == -1)
    vabort("dprintf: attempt to write to a closed file descriptor %d\n", fd);
  validate_format(fmtdesc, fmt, ap, "dprintf", NULL, 0);
  va_start(ap, fmt);
  return vdprintf(fd, fmt, ap);
}

int builtin_sprintf(const char *fmtdesc, char *buffer, const char *fmt, ...) {
  va_list ap;
  /* Make sure that the buffer has sufficient space to store the result of the
     function. Luckily this can be accomplished via `snprintf(buf, n, mfmt,...)`
     that can take `NULL` and `0` as its first two arguments (nothing is
     printed) but still returns the number of characters that would have been
     printed if both `buf` and `n` were sufficiently large. This behaviour is
     C99-compliant and described in par. 7.19.6.5 of the C99 standard */
  va_start(ap, fmt);
  int len = vsnprintf(NULL, 0, fmt, ap);
  if (!writeable((uintptr_t)buffer, len + 1, (uintptr_t)buffer))
    vabort("sprintf: output buffer is unallocated or has insufficient length "
      "to store %d characters or not writeable\n", len + 1);
  va_start(ap, fmt);
  validate_format(fmtdesc, fmt, ap, "sprintf", buffer, len + 1);
  va_start(ap, fmt);
  return vsprintf(buffer, fmt, ap);
}

int builtin_snprintf(const char *fmtdesc, char *buffer, size_t size,
    const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  validate_format(fmtdesc, fmt, ap, "snprintf", buffer, size);
  /* Check that the input buffer is large enough. However, if there are zero
     characters to write, it does not matter */
  if (size > 0 && !writeable((uintptr_t)buffer, size, (uintptr_t)buffer))
    vabort("sprintf: output buffer is unallocated or has insufficient length "
      "to store %d characters and \0 terminator or not writeable\n", size);
  va_start(ap, fmt);
  return vsnprintf(buffer, size, fmt, ap);
}

int builtin_syslog(const char *fmtdesc, int priority, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  validate_format(fmtdesc, fmt, ap, "syslog", NULL, 0);
  va_start(ap, fmt);
  return vsyslog(priority, fmt, ap);
}

/* }}} */
#endif /* E_ACSL_VALIDATE_FORMAT_STRINGS */
#endif /* E_ACSL_FORMAT_H */
