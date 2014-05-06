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

/* ISO C: 7.26 */
#ifndef __FC_WCTYPE_H
#define __FC_WCTYPE_H

#include "__fc_define_wint_t.h"

int iswalnum(wint_t wc);

int iswalpha(wint_t wc);

int iswascii(wint_t wc);

int iswblank(wint_t wc);

int iswcntrl(wint_t wc);

int iswdigit(wint_t wc);

int iswgraph(wint_t wc);

int iswhexnumber(wint_t wc);

int iswideogram(wint_t wc);

int iswlower(wint_t wc);

int iswnumber(wint_t wc);

int iswphonogram(wint_t wc);

int iswprint(wint_t wc);

int iswpunct(wint_t wc);

int iswrune(wint_t wc);

int iswspace(wint_t wc);

int iswspecial(wint_t wc);

int iswupper(wint_t wc);

int iswxdigit(wint_t wc);

#endif
