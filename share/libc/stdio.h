/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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

/* ISO C: 7.19 */
#ifndef __FC_STDIO
#define __FC_STDIO

#include "stdarg.h"
#include "errno.h"
#include "__fc_define_size_t.h"
#include "__fc_define_restrict.h"

struct __fc_pos_t { unsigned long __fc_stdio_position; };
typedef struct __fc_pos_t fpos_t;

struct __fc_FILE {
  fpos_t __fc_stdio_fpos;
  char* __fc_stdio_buffer;
  char __fc_stdio_error;
  char __fc_stdio_eof; 
  long __fc_stdio_id;};
typedef struct __fc_FILE FILE;

#include "__fc_define_null.h"

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

/* TODO: This chould be customizable */
#define BUFSIZ 0xFFFFFFFF

#define EOF (-1)

/* TODO: This chould be customizable */
#define FOPEN_MAX 512
/* TODO: This chould be customizable */
#define FILENAME_MAX 2048
/* TODO: This chould be customizable */
#define L_tmpnam 2048

#define SEEK_CUR 0
#define SEEK_END 1
#define SEEK_SET 2

/* TODO: This could be customizable */
#define TMP_MAX 0xFFFFFFFF

extern FILE * __fc_stderr;
#define stderr __fc_stderr

extern FILE * __fc_stdin;
#define stdin __fc_stdin

extern FILE * __fc_stdout;
#define stdout __fc_stdout

/*@ assigns \nothing; */ 
int remove(const char *filename);

/*@ assigns \nothing; */ 
int rename(const char *old, const char *new);

/*@ assigns \nothing; 
  ensures \result==\null || (\valid(\result) && \fresh(\result)) ; */ 
FILE *tmpfile(void);

/*@ 
  assigns \result \from s[..]; 
  assigns s[..] \from \nothing; 
  // TODO: more precise behaviors from ISO C 7.19.4.4 
*/
char *tmpnam(char *s);

/*@ assigns *stream; 
  ensures \result == 0 || \result == (-1);  // -1 expanded manually to EOF
  // TODO: more precise behaviors from ISO C 7.19.4.1 
*/
int fclose(FILE *stream);

/*@ assigns *stream;
   ensures \result == 0 || \result == (-1);  // -1 expanded manually from EOF
  // TODO: more precise behaviors from ISO C 7.19.5.2
 */
int fflush(FILE *stream);

/*@ assigns \nothing; 
  ensures \result==\null || (\valid(\result) && \fresh(\result)) ; */ 
FILE *fopen(const char * __restrict filename,
     const char * __restrict mode);

/*@ 
  assigns *stream; 
  ensures \result==\null || \result==stream ; */ 
FILE *freopen(const char * restrict filename,
              const char * restrict mode,
              FILE * restrict stream);

/*@ assigns *stream \from buf; */
void setbuf(FILE * restrict stream,
     char * restrict buf);

/*@ assigns *stream \from buf,mode,size; */
int setvbuf(FILE * restrict stream,
     char * restrict buf,
     int mode, size_t size);

/*@ assigns *stream; */
int fprintf(FILE * restrict stream,
     const char * restrict format, ...);

/*@ assigns *stream;
// unsupported...
 */
int fscanf(FILE * restrict stream,
     const char * restrict format, ...);

/*@ assigns *__fc_stdout; */
int printf(const char * restrict format, ...);

/*@ assigns *__fc_stdin; 
// unsupported...
 */
int scanf(const char * restrict format, ...);

/*@ assigns s[0..n]; 
// unsupported...
 */
int snprintf(char * restrict s, size_t n,
    const char * restrict format, ...);

/*@ assigns s[0..]; 
// unsupported...
 */
int sprintf(char * restrict s,
     const char * restrict format, ...);

/*@ assigns \nothing ; */
int sscanf(const char * restrict s,
     const char * restrict format, ...);

/*@ assigns *stream \from format[..], arg; */
int vfprintf(FILE * restrict stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *stream \from format[..], *stream; 
// TODO: assign arg too. */
int vfscanf(FILE * restrict stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdout \from arg; */
int vprintf(const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdin \from format[..]; 
// TODO: assign arg too. */
int vscanf(const char * restrict format,
     va_list arg);

/*@ assigns s[0..n] \from format[..], arg; 
 */
int vsnprintf(char * restrict s, size_t n,
     const char * restrict format,
     va_list arg);

/*@ assigns s[0..] \from format[..], arg; 
 */
int vsprintf(char * restrict s,
     const char * restrict format,
     va_list arg);

/* @ TODO: assigns arg ; */
int vsscanf(const char * restrict s,
     const char * restrict format,
     va_list arg);

/*@ assigns *stream;
 */
int fgetc(FILE *stream);

/*@ assigns s[0..n],*stream;
 */
char *fgets(char * restrict s, int n,
    FILE * restrict stream);

/*@ assigns *stream ; */
int fputc(int c, FILE *stream);

/*@ assigns *stream \from s[..]; */
int fputs(const char * restrict s,
     FILE * restrict stream);

/*@ assigns *stream; */
int getc(FILE *stream);

/*@ assigns \result \from *__fc_stdin ;
 */
int getchar(void);

/*@ assigns s[..] \from *__fc_stdin ;
  ensures \result == s || \result == \null;
 */
char *gets(char *s);

/*@ assigns *stream ; */
int putc(int c, FILE *stream);

/*@ assigns *__fc_stdout \from c; */
int putchar(int c);

/*@ assigns *__fc_stdout \from s[..]; */
int puts(const char *s);

/*@ assigns *stream \from c; */
int ungetc(int c, FILE *stream);

/*@ assigns ((char*)ptr)[0..(nmemb*size)] \from *stream; */
size_t fread(void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@ assigns *stream \from ((char*)ptr)[0..(nmemb*size)]; */
size_t fwrite(const void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@ assigns *pos \from *stream ; */
int fgetpos(FILE * restrict stream,
     fpos_t * restrict pos);

/*@ assigns *stream \from offset, whence ; 
  assigns __FC_errno ; */
int fseek(FILE *stream, long int offset, int whence);

/*@ assigns *stream \from *pos; */
int fsetpos(FILE *stream, const fpos_t *pos);

/*@ assigns \result, __FC_errno \from *stream ;*/
long int ftell(FILE *stream);

/*@  assigns *stream \from \nothing; */
void rewind(FILE *stream);
/*@  assigns *stream  \from \nothing; */
void clearerr(FILE *stream);

/*@ assigns \result \from *stream ;*/
int feof(FILE *stream);

/*@ assigns \result \from *stream ;*/
int ferror(FILE *stream);

/*@ assigns __fc_stdout \from __FC_errno, s[..]; */
void perror(const char *s);


#endif
