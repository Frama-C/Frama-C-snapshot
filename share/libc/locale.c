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
