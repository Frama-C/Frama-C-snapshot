/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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

#ifndef __FC_ICONV
#define __FC_ICONV
#include "__fc_define_restrict.h"
#include "__fc_define_size_t.h"
typedef void * iconv_t;

extern int __FC_errno;
/*@ assigns *outbuf[0 .. *outbytesleft-1] \from *inbuf[0 .. *inbytesleft-1]; 
  assigns __FC_errno ; */
size_t  iconv(iconv_t cd, char **restrict inbuf, size_t *restrict inbytesleft,
            char **restrict outbuf, size_t *restrict outbytesleft);

/*@ assigns __FC_errno;
  ensures \result == 0 || \result == 1 ; */
int     iconv_close(iconv_t);

/*@ assigns \result \from tocode[..],fromcode[..];
  assigns __FC_errno; */
iconv_t iconv_open(const char *tocode, const char *fromcode);

#endif
