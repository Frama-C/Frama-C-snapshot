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

wchar_t* wmemcpy(wchar_t * region1, const wchar_t * region2, size_t n)
{
  const wchar_t* first = (const wchar_t*)region2;
  const wchar_t* last = ((const wchar_t*)region2) + n;
  wchar_t* result = (wchar_t*)region1;
  wchar_t* dest = result;
  while (first != last)
    *dest++ = *first++;
  return result;
}

wchar_t * wmemset(wchar_t *dest, wchar_t val, size_t len)
{
  wchar_t *ptr = dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}


wchar_t* wcscpy(wchar_t * s1, const wchar_t * s2)
{
  wchar_t *os1 = s1;

  while (*s1++ = *s2++)
    ;
  return (os1);
}

size_t wcslen(const wchar_t * str)
{
  const wchar_t *s =str;
  for (s = str; *s; ++s);
  return(s - str);
}

wchar_t * wcsncpy(wchar_t * s1, const wchar_t * s2, size_t n)
{
  wchar_t *os1 = s1;

  n++;
  while ((--n != 0) && ((*s1++ = *s2++) != L'\0'))
    ;
  if (n != 0)
    while (--n != 0)
      *s1++ = L'\0';
  return (os1);
}

wchar_t * wcscat(wchar_t * s1, const wchar_t * s2)
{
  wchar_t *os1 = s1;

  while (*s1++)
    ;
  --s1;
  while (*s1++ = *s2++)
    ;
  return (os1);
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
