/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2013                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

#include "locale.h"
#include "limits.h"
struct lconv __C_locale = {".","","","","","","","","","",CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX};

struct lconv *__frama_c_locale=&__C_locale;

char*__frama_c_locale_names[1]={"C"};
char *setlocale(int category, const char *locale) {
  if (*locale == 'C') 
    { __frama_c_locale = &__C_locale;
      return __frama_c_locale_names[0];
    };
  return NULL;
}

struct lconv *localeconv(void) {
  return __frama_c_locale;
}
