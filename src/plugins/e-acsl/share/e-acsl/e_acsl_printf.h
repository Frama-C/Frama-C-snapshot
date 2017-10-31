/****************************************************************************/
/*                                                                          */
/*  Copyright (c) 2004,2012 Kustaa Nyholm / SpareTimeLabs                   */
/*                                                                          */
/*  All rights reserved.                                                    */
/*                                                                          */
/*  Redistribution and use in source and binary forms, with or without      */
/*  modification, are permitted provided that the following conditions      */
/*  are met:                                                                */
/*                                                                          */
/*  Redistributions of source code must retain the above copyright          */
/*  notice, this list of conditions and the following disclaimer.           */
/*                                                                          */
/*  Redistributions in binary form must reproduce the above copyright       */
/*  notice, this list of conditions and the following disclaimer in the     */
/*  documentation and/or other materials provided with the distribution.    */
/*                                                                          */
/*  Neither the name of the Kustaa Nyholm or SpareTimeLabs nor the names    */
/*  of its contributors may be used to endorse or promote products derived  */
/*  from this software without specific prior written permission.           */
/*                                                                          */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR   */
/*  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT   */
/*  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,  */
/*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        */
/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   */
/*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   */
/*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   */
/*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    */
/*                                                                          */
/****************************************************************************/

/*! ***********************************************************************
 * \file   e_acsl_printf.h
 * \brief Malloc and stdio free implementation printf.
 *
 * Supported format strings:
 * - Flag characters:
 *     - 0       - the following value will be is zero-padded.
 *
 * - Field width:
 *     - Optional positive decimal integer following flag characters.
 *
 * - Length modifier:
 *     - l       - the following integer conversion corresponds to a long int or
 *                  unsigned long int argument.
 *
 * - Standard conversion specifiers:
 *    - d       - signed integers.
 *    - u       - unsigned integers.
 *    - f       - floating point numbers. Floating point numbers do not support
 *    -             precision specification.
 *    - x,X     - hexadecimal numbers.
 *    - p       - void pointers.
 *
 * - Non-standard conversion specifiers:
 *     - a       - memory-address.
 *     - b, B    - print field width bits of a number left-to-right (b) or
 *      right-to-left (B). Unless specified field-width of 8 is used. Bits
 *      over a 64-bit boundary are ignored.
 *     - v, V    - print first field width bits of a memory region given by a
 *      void pointer left-to-right (v) or right-to-left (V). Unless specified
 *      field-width of 8 is used.
***************************************************************************/

#ifndef E_ACSL_PRINTF_H
#define E_ACSL_PRINTF_H

#include <unistd.h>
#include <stdint.h>
#include <stdarg.h>
#include "e_acsl_alias.h"

/* ****************** */
/* Public API         */
/* ****************** */

/* Replacement for printf with support for the above specifiers */
static int rtl_printf(char *fmt, ...);

/* Same as printf but write to a string buffer */
static int rtl_sprintf(char* s, char *fmt, ...);

/* Same as printf but write to the error stream. */
static int rtl_eprintf(char *fmt, ...);

/* Same as printf but write to a file descriptor. */
static int rtl_dprintf(int fd, char *fmt, ...);

/* ****************** */
/* Implementation     */
/* ****************** */

typedef void (*putcf) (void*,char);

/* Unsigned long integers to string conversion (%u) */
static void uli2a(unsigned long int num, unsigned int base, int uc,char * bf) {
  int n=0;
  unsigned long int d=1;
  while (num/d >= base)
    d*=base;
  while (d!=0) {
    int dgt = num / d;
    num%=d;
    d/=base;
    if (n || dgt>0|| d==0) {
      *bf++ = dgt+(dgt<10 ? '0' : (uc ? 'A' : 'a')-10);
      ++n;
    }
  }
  *bf=0;
}

/* Unsigned pointer-wide integers to memory address conversion (%a) */
static void addr2a(uintptr_t addr, char * bf) {
  *bf++ = '0';
  *bf++ = 'x';

  unsigned int digits = 1;
  int n=0;
  unsigned long int d=1;
  while (addr/d >= 10) {
    d*=10;
    digits++;
  }

  unsigned int ctr = 0;
  while (d!=0) {
    ctr++;
    int dgt = addr / d;
    addr%=d;
    d/=10;
    if (n || dgt>0|| d==0) {
      *bf++ = dgt+(dgt<10 ? '0' : 'a' - 10);
      ++n;
    }
    if (--digits%5 == 0 && d != 0)
        *bf++ = '-';
  }
  *bf=0;
}

/* Pointer to string conversion (%p) */
static void ptr2a(void *p, char *bf) {
    *bf++ = '0';
    *bf++ = 'x';
    uli2a((intptr_t)p,16,0,bf);
}

/* Signed long integer to string conversion (%ld) */
static void li2a (long num, char * bf) {
  if (num<0) {
    num=-num;
    *bf++ = '-';
  }
  uli2a(num,10,0,bf);
}

/* Signed integer to string conversion (%d) */
static void ui2a(unsigned int num, unsigned int base, int uc,char * bf) {
  int n=0;
  unsigned int d=1;
  while (num/d >= base)
    d*=base;
  while (d!=0) {
    int dgt = num / d;
    num%= d;
    d/=base;
    if (n || dgt>0 || d==0) {
      *bf++ = dgt+(dgt<10 ? '0' : (uc ? 'A' : 'a')-10);
      ++n;
    }
  }
  *bf=0;
}

/* Integer bit-fields to string conversion (%b, %B) */
static void bits2a(long int v, int size, char *bf, int l2r) {
  int i;
  if (l2r) {
    for(i = 0; i < size; i++) {
      *bf++ = '0' + ((v >> i) & 1);
      if (i && i+1 < size && (i+1)%8 == 0)
        *bf++  = ' ';
    }
  } else {
    for(i = size - 1; i >= 0; i--) {
      *bf++ = '0' + ((v >> i) & 1);
      if (i && i+1 < size && i%4 == 0)
        *bf++  = ' ';
    }
  }
  *bf=0;
}

/* Pointer bit-fields to string conversion (%v, %V) */
static void pbits2a(void *p, int size, char *bf, int l2r) {
  char *v = (char*)p;
  int i;
  if (l2r) {
    for(i = 0; i < size; i++) {
      *bf++ = '0' + ((v[i/8] >> i%8) & 1);
      if (i && i+1 < size && (i+1)%4 == 0)
        *bf++  = ' ';
    }
  } else {
    for(i = size - 1; i >= 0; i--) {
      *bf++ = '0' + ((v[i/8] >> i%8) & 1);
      if (i && i+1 < size && i%4 == 0)
        *bf++  = ' ';
    }
  }
  *bf=0;
}

/* Signed integer to string (%d) */
static void i2a (int num, char * bf) {
  if (num<0) {
    num=-num;
    *bf++ = '-';
  }
  ui2a(num,10,0,bf);
}

/* Char to int conversion  */
static int a2d(char ch) {
  if (ch>='0' && ch<='9')
    return ch-'0';
  else if (ch>='a' && ch<='f')
    return ch-'a'+10;
  else if (ch>='A' && ch<='F')
    return ch-'A'+10;
  else return -1;
}

static char a2i(char ch, char** src, int base, int* nump) {
  char* p= *src;
  int num=0;
  int digit;
  while ((digit=a2d(ch))>=0) {
    if (digit>base) break;
    num=num*base+digit;
    ch=*p++;
  }
  *src=p;
  *nump=num;
  return ch;
}

static void putchw(void* putp, putcf putf, int n, char z, char* bf) {
  char fc=z? '0' : ' ';
  char ch;
  char* p=bf;
  while (*p++ && n > 0)
    n--;
  while (n-- > 0)
    putf(putp,fc);
  while ((ch= *bf++))
    putf(putp,ch);
}

static void putcp(void* p,char c) {
  *(*((char**)p))++ = c;
}

static void _format(void* putp, putcf putf, char *fmt, va_list va) {
  char bf[256];
  char ch;
  while ((ch=*(fmt++))) {
    if (ch!='%') // if not '%' print character as is
      putf(putp,ch);
    else { // otherwise do the print based on the format following '%'
      char lz=0;
      char lng=0; // long (i.e., 'l' specifier)
      int w=0;
      ch=*(fmt++);
      if (ch=='0') { // '0' specifier - padding with zeroes
        ch=*(fmt++);
        lz=1;
      }
      if (ch>='0' && ch<='9') {
        ch=a2i(ch,&fmt,10,&w);
      }
      if (ch=='l') {
        ch=*(fmt++);
        lng=1;
      }
      switch (ch) {
        case 0:
          break;
        case 'u': {
          if (lng)
            uli2a(va_arg(va, unsigned long int),10,0,bf);
          else
            ui2a(va_arg(va, unsigned int),10,0,bf);
          putchw(putp,putf,w,lz,bf);
          break;
        }
        case 'd': {
          if (lng)
            li2a(va_arg(va, unsigned long int),bf);
          else
            i2a(va_arg(va, int),bf);
          putchw(putp,putf,w,lz,bf);
          break;
        }
        case 'p':
          ptr2a(va_arg(va, void*), bf);
          putchw(putp,putf,w,lz,bf);
          break;
        case 'a':
          addr2a(va_arg(va, uintptr_t), bf);
          putchw(putp,putf,w,lz,bf);
          break;
        case 'b':
          bits2a(va_arg(va, long), w > 64 ? 64 : w ? w : 8, bf, 1);
          putchw(putp,putf,0,0,bf);
          break;
        case 'B':
          bits2a(va_arg(va, long), w > 64 ? 64 : w ? w : 8, bf, 0);
          putchw(putp,putf,0,0,bf);
          break;
        case 'v':
          pbits2a(va_arg(va, void*), w ? w : 8, bf, 1);
          putchw(putp,putf,0,0,bf);
          break;
        case 'V':
          pbits2a(va_arg(va, void*), w ? w : 8, bf, 0);
          putchw(putp,putf,0,0,bf);
          break;
        case 'x':
        case 'X':
          if (lng)
            uli2a(va_arg(va, unsigned long int),16,(ch=='X'),bf);
          else
            ui2a(va_arg(va, unsigned int),16,(ch=='X'),bf);
          putchw(putp,putf,w,lz,bf);
          break;
        case 'f' : {
          double num = va_arg(va, double);
          int ord = (int)num;
          i2a(ord,bf);
          putchw(putp,putf,w,lz,bf);
          putf(putp,'.');
          num = num - ord;
          num *= 1000;
          ord = (int)num;
          i2a(ord,bf);
          putchw(putp,putf,w,lz,bf);
          break;
        }
        case 'c' :
          putf(putp,(char)(va_arg(va, int)));
          break;
        case 's' :
          putchw(putp,putf,w,0,va_arg(va, char*));
          break;
        case '%' :
          putf(putp,ch);
        default:
          break;
      }
    }
  }
}

static void _charc_stdout (void* p, char c) { write(1,&c,1); }
static void _charc_stderr (void* p, char c) { write(2,&c,1); }
static void _charc_file (void* p, char c) { write((size_t)p,&c,1); }

static void _charc_literal  (void* p, char c) {
  switch(c) {
    case '\r':
      write((size_t)p,"\\r",2);
      break;
    case '\f':
      write((size_t)p,"\\f",2);
      break;
    case '\b':
      write((size_t)p,"\\b",2);
      break;
    case '\a':
      write((size_t)p,"\\a",2);
      break;
    case '\n':
      write((size_t)p,"\\n",2);
      break;
    case '\t':
      write((size_t)p,"\\t",2);
      break;
    case '\0':
      write((size_t)p,"\\0",2);
      break;
    default:
      write((size_t)p,&c,1);
  }
}

static int rtl_printf(char *fmt, ...) {
  va_list va;
  va_start(va,fmt);
  _format(NULL,_charc_stdout,fmt,va);
  va_end(va);
  return 1;
}

static int rtl_eprintf(char *fmt, ...) {
  va_list va;
  va_start(va,fmt);
  _format(NULL,_charc_stderr,fmt,va);
  va_end(va);
  return 1;
}

static int rtl_dprintf(int fd, char *fmt, ...) {
  va_list va;
  va_start(va,fmt);
  intptr_t fd_long = fd;
  _format((void*)fd_long,_charc_file,fmt,va);
  va_end(va);
  return 1;
}

static int rtl_sprintf(char* s, char *fmt, ...) {
  va_list va;
  va_start(va,fmt);
  _format(&s,putcp,fmt,va);
  putcp(&s,0);
  va_end(va);
  return 1;
}

#define STDOUT(...) rtl_printf(__VA_ARGS__)
#define STDERR(...) rtl_eprintf(__VA_ARGS__)

#endif
