/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2016                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

#include "string.h"
#include "stdlib.h"

#ifdef __FC_USE_BUILTIN__
#include "__fc_builtin.h"
void* memcpy(void* region1, const void* region2, size_t n)
{
  if (n > 0)
    Frama_C_memcpy(region1, region2, n);
  return region1;
}

void* memmove(void *dest, const void *src, size_t n)
{
  Frama_C_memcpy(dest, src, n);
  return dest;
}
#else
void* memcpy(void* region1, const void* region2, size_t n)
{
  const char* first = (const char*)region2;
  const char* last = ((const char*)region2) + n;
  char* result = (char*)region1;
  char* dest = result;
  while (first != last)
    *dest++ = *first++;
  return result;
}
#endif

char *strdup(const char *s)
{
  size_t l = strlen(s) + 1;
  char *p = malloc(l);
  memcpy(p, s, l);
  return p;
}

void* memset (void* dest, int val, size_t len)
{
  unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}

int strcmp(const char *s1, const char *s2)
{
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
strlen(const char * str)
{
    const char *s =str;
    for (s = str; *s; ++s);
    return(s - str);
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

char *strchr(const char *s, int c)
{ const char ch = c;
  for ( ; *s != ch; s++)
    if (*s == '\0') return 0;
  return (char *)s;
}

char *strrchr(const char *s, int c)
{
    char* ret=0;
    do {
        if( *s == (char)c )
            ret=s;
    } while(*s++);
    return ret;
}

char *strstr(char *string, char *substring)
{
    char *a, *b;
    
    /* First scan quickly through the two strings looking for a
     * single-character match.  When it's found, then compare the
     * rest of the substring.
     */

    b = substring;
    if (*b == 0) {
	return string;
    }
    for ( ; *string != 0; string += 1) {
	if (*string != *b) {
	    continue;
	}
	a = string;
	while (1) {
	    if (*b == 0) {
		return string;
	    }
	    if (*a++ != *b++) {
		break;
	    }
	}
	b = substring;
    }
    return (char *) 0;
}

char* strncat(char *dest, const char *src, size_t n)
{
  size_t dest_len = strlen(dest);
  size_t i;

  for (i = 0 ; i < n && src[i] != '\0' ; i++)
    dest[dest_len + i] = src[i];
  dest[dest_len + i] = '\0';

  return dest;
}

char *strerror(int errnum)
{
  return "strerror message by Frama-C";
}

int char_equal_ignore_case(char c1, char c2) {
  if( c1 >= 'A' && c1 <= 'Z') c1 -= ('A' - 'a');
  if( c2 >= 'A' && c2 <= 'Z') c2 -= ('A' - 'a');
  if( c1 == c2) return 0;
  else return (int) ((unsigned char) c2 - (unsigned char) c1);
}

int strcasecmp(const char *s1, const char *s2)
{
  while(*s1 && *s2) {
    int res = char_equal_ignore_case(*s1,*s2);
    if(res != 0) return res;
    s1++;
    s2++;
  }

  if( *s1 == 0 && *s2 == 0) return 0;
  if( *s1 == 0) return -1;
  return 1;
}
