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

/* ISO C: 7.25 */
#ifndef __FC_WCHAR_H
#define __FC_WCHAR_H

#include "__fc_define_wchar_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_file.h"

wchar_t * wmemchr(const wchar_t *s, wchar_t c, size_t n);

int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);

wchar_t * wmemcpy(wchar_t * s1, const wchar_t * s2, size_t n);

wchar_t * wmemmove(wchar_t *s1, const wchar_t *s2, size_t n);

wchar_t * wmemset(wchar_t *s, wchar_t c, size_t n);

wchar_t * wcscat(wchar_t * s1, const wchar_t * s2);

wchar_t * wcschr(const wchar_t *s, wchar_t c);

int wcscmp(const wchar_t *s1, const wchar_t *s2);

wchar_t * wcscpy(wchar_t * s1, const wchar_t * s2);

size_t wcscspn(const wchar_t *s1, const wchar_t *s2);

size_t wcslcat(wchar_t *s1, const wchar_t *s2, size_t n);

size_t wcslcpy(wchar_t *s1, const wchar_t *s2, size_t n);

size_t wcslen(const wchar_t *s);

wchar_t * wcsncat(wchar_t * s1, const wchar_t * s2, size_t n);

int wcsncmp(const wchar_t *s1, const wchar_t * s2, size_t n);

wchar_t * wcsncpy(wchar_t * s1, const wchar_t * s2, size_t n);

wchar_t * wcspbrk(const wchar_t *s1, const wchar_t *s2);

wchar_t * wcsrchr(const wchar_t *s, wchar_t c);

size_t wcsspn(const wchar_t *s1, const wchar_t *s2);

wchar_t * wcsstr(const wchar_t * s1, const wchar_t * s2);


/* It is unclear whether these are more often in wchar.h or stdio.h */

int fwprintf(FILE * stream, const wchar_t * format, ...);

int swprintf(wchar_t * ws, size_t n, const wchar_t * format, ...);

int wprintf(const wchar_t * format, ...);


int wscanf(const wchar_t * format, ...);

int fwscanf(FILE * stream, const wchar_t * format, ...);

int swscanf(const wchar_t * str, const wchar_t * format, ...);

#endif
