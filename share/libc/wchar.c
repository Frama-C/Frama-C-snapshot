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

/* ISO C: 7.25 */
#include "wchar.h"

wchar_t* wmemcpy(wchar_t *dest, const wchar_t *src, size_t n)
{
  for (size_t i = 0; i < n; i++) {
    dest[i] = src[i];
  }
  return dest;
}

wchar_t * wmemset(wchar_t *dest, wchar_t val, size_t len)
{
  for (size_t i = 0; i < len; i++) {
    dest[i] = val;
  }
  return dest;
}

wchar_t* wcscpy(wchar_t *dest, const wchar_t *src)
{
  size_t i;
  for (i = 0; src[i] != L'\0'; i++)
    dest[i] = src[i];
  dest[i] = L'\0';
  return dest;
}

size_t wcslen(const wchar_t * str)
{
  size_t i;
  for (i = 0; str[i] != L'\0'; i++);
  return i;
}

wchar_t * wcsncpy(wchar_t *dest, const wchar_t *src, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++) {
    dest[i] = src[i];
    if (src[i] == L'\0') break;
  }
  for (; i < n; i++)
    dest[i] = L'\0';
  return dest;
}

wchar_t * wcscat(wchar_t *dest, const wchar_t *src)
{
  size_t i;
  size_t n = wcslen(dest);
  for (i = 0; src[i] != L'\0'; i++) {
    dest[n+i] = src[i];
  }
  dest[n+i] = L'\0';
  return dest;
}

wchar_t* wcsncat(wchar_t *dest, const wchar_t *src, size_t n)
{
  size_t dest_len = wcslen(dest);
  size_t i;

  for (i = 0 ; i < n && src[i] != L'\0' ; i++)
    dest[dest_len + i] = src[i];
  dest[dest_len + i] = L'\0';

  return dest;
}
