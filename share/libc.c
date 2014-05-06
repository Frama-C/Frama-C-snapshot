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

#include "libc.h"

#ifndef FRAMA_C_MEMCPY
#include "builtin.h"
void* memcpy(void* region1, const void* region2, size_t n)
{
  if (n > 0)
    Frama_C_memcpy(region1, region2, n);
  return region1;
}

void* memmove(void* region1, const void* region2, size_t n)
{
  if (n > 0)
    Frama_C_memcpy(region1, region2, n);
  return region1;
}
#else
void* memcpy(void* region1, const void* region2, size_t n)
{
  const char* first = (const char*)region2;
  const char* last = ((const char*)region2) + n;
  char* dest = (char*)region1;
  while (first != last)
    {
      *dest = *first;
      dest++;
      first++;
    }
  return region1;
}
#endif

void* memset (void* dest, int val, size_t len)
{
  unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}

int strcmp(const char *s1, const char *s2)
{
  if (s1 == s2)
    return (0);
  while (*s1 == *s2++)
    if (*s1++ == '\0')
      return (0);
  return (*(unsigned char *)s1 - *(unsigned char *)--s2);
}

char* strcat(char *s1, const char *s2)
{
  char *os1 = s1;

  while (*s1++)
    ;
  --s1;
  while (*s1++ = *s2++)
    ;
  return (os1);
}

char* strcpy(char *s1, const char *s2)
{
  char *os1 = s1;

  while (*s1++ = *s2++)
    ;
  return (os1);
}

/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */
char *
strncpy(char *s1, const char *s2, size_t n)
{
  char *os1 = s1;

  n++;
  while ((--n != 0) && ((*s1++ = *s2++) != '\0'))
    ;
  if (n != 0)
    while (--n != 0)
      *s1++ = '\0';
  return (os1);
}

/*
 * Compare strings (at most n bytes)
 *	returns: s1>s2; >0  s1==s2; 0  s1<s2; <0
 */
int
strncmp(const char *s1, const char *s2, size_t n)
{
  n++;
  if (s1 == s2)
    return (0);
  while (--n != 0 && *s1 == *s2++)
    if (*s1++ == '\0')
      return (0);
  return (n == 0 ? 0 : *(unsigned char *)s1 - *(unsigned char *)--s2);
}

size_t
strlen(const char *s)
{
  const char *s0 = s + 1;

  while (*s++ != '\0')
    ;
  return (s - s0);
}

int
memcmp(const void *s1, const void *s2, size_t n)
{
  if (s1 != s2 && n != 0) {
    const unsigned char	*ps1 = s1;
    const unsigned char	*ps2 = s2;

    do {
      if (*ps1++ != *ps2++)
	return (ps1[-1] - ps2[-1]);
    } while (--n != 0);
  }
  return 0;
}

#define	ISDIGIT(_c) \
	((_c) >= '0' && (_c) <= '9')

#define	ISXDIGIT(_c) \
	(ISDIGIT(_c) || \
	((_c) >= 'a' && (_c) <= 'f') || \
	((_c) >= 'A' && (_c) <= 'F'))

#define	ISLOWER(_c) \
	((_c) >= 'a' && (_c) <= 'z')

#define	ISUPPER(_c) \
	((_c) >= 'A' && (_c) <= 'Z')

#define	ISALPHA(_c) \
	(ISUPPER(_c) || \
	ISLOWER(_c))

#define	ISALNUM(_c) \
	(ISALPHA(_c) || \
	ISDIGIT(_c))

#define	ISSPACE(_c) \
	((_c) == ' ' || \
	(_c) == '\t' || \
	(_c) == '\r' || \
	(_c) == '\n')

static int
isdigit(int c)
{
	return (ISDIGIT(c));
}


static int
isxdigit(int c)
{
	return (ISXDIGIT(c));
}


static int
islower(int c)
{
	return (ISLOWER(c));
}


static int
isupper(int c)
{
	return (ISUPPER(c));
}


static int
isalpha(int c)
{
	return (ISALPHA(c));
}


static int
isalnum(int c)
{
	return (ISALNUM(c));
}


static int
isspace(int c)
{
	return (ISSPACE(c));
}

int
atoi(const char *p)
{
  int n;
  int c, neg = 0;
  unsigned char	*up = (unsigned char *)p;

  if (!isdigit(c = *up)) {
    while (isspace(c))
      c = *++up;
    switch (c) {
    case '-':
      neg++;
      /* FALLTHROUGH */
    case '+':
      c = *++up;
    }
    if (!isdigit(c))
      return (0);
  }
  for (n = '0' - c; isdigit(c = *++up); ) {
    n *= 10; /* two steps to avoid unnecessary overflow */
    n += '0' - c; /* accum neg to avoid surprises at MAX */
  }
  return (neg ? n : -n);
}

char *
strchr (s, c)
  const char *s;
  int c;
{
  do {
    if (*s == c)
      {
	return (char*)s;
      }
  } while (*s++);
  return (0);
}

char *
strrchr (s, c)
  const char *s;
  int c;
{
  char *rtnval = 0;

  do {
    if (*s == c)
      rtnval = (char*) s;
  } while (*s++);
  return (rtnval);
}

char *
strstr (s1, s2)
  char *s1, *s2;
{
  char *p = s1;
  int len = strlen (s2);

  for (; (p = strchr (p, *s2)) != 0; p++)
    {
      if (strncmp (p, s2, len) == 0)
	{
	  return (p);
	}
    }
  return (0);
}

char * getenv(const char * c) {
  return (char*)0;
}


volatile int any;
int my_errno = 0;

int *_errno()
{
  return &my_errno;
}

#if 0
int fprintf(FILE *restrict stream, const char *restrict format, ...)
{
  return any;
}
#endif

int printf(const char *restrict format, ...)
{
  return any;
}

int sprintf(char *restrict s, const char *restrict format, ...)
{
  int i;

  for (i = 0; format[i] != '\0'; i++)
    {
      s[i] = format[i];
    }
  /* boucle qui copie le format vers s */
  return any;
}

int snprintf (char *restrict s, size_t size, const char *restrict fmt, ...) {
  int i;
  for (i = 0; i < size && fmt[i] != '\0'; i++)
      s[i] = fmt[i];
  return any;
}

/*@ ensures \valid(t2);
    assigns *t2; */
int localtime_r (struct tm *t2, const time_t t);

int toupper (int c)
{
  if ((c >= 'a') && (c <= 'z'))
    return c - 0x20;
  return c;
}

int abs (int i)
{
  if (i < 0)
    return -i;
  return i;
}

#if 0
int vsprintf(char *restrict s, const char *restrict format, .../*va_list va_arg*/)
{
  return sprintf(s,format/*,va_arg*/);
}
#endif

ssize_t read(int fd, void *buf, size_t count)
{
  unsigned char *ptr = (unsigned char*)buf;
  while (count-- > 0)
    *ptr++ = any;
  return any;
}
