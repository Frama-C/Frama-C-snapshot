/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
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
#include "features.h"
#include "__fc_machdep.h"
#include "stdarg.h"
#include "stddef.h"
#include "errno.h"
#include "__fc_define_stat.h"
#include "__fc_define_fpos_t.h"
#include "__fc_define_file.h"
#include "__fc_define_null.h"

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define BUFSIZ __FC_BUFSIZ
#define EOF __FC_EOF
#define FOPEN_MAX __FC_FOPEN_MAX
#define FILENAME_MAX __FC_FILENAME_MAX
#define L_tmpnam __FC_L_tmpnam

#include "__fc_define_seek_macros.h"

#define TMP_MAX __FC_TMP_MAX

__BEGIN_DECLS

extern FILE * __fc_stderr;
#define stderr (__fc_stderr)

extern FILE * __fc_stdin;
#define stdin (__fc_stdin)

extern FILE * __fc_stdout;
#define stdout (__fc_stdout)

/*
  Note: currently some functions only consider the __fc_stdio_id field of FILE.
        This models the fact that operations on different files are considered
        non-interferent between them.
*/

/*@ assigns \nothing; */ 
int remove(const char *filename);

/*@ assigns \nothing; */ 
int rename(const char *old_name, const char *new_name);

/*@ assigns \nothing; 
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE))) ; */ 
FILE *tmpfile(void);

/*@
  assigns \result \from s[..]; 
  assigns s[..] \from \nothing; 
  // TODO: more precise behaviors from ISO C 7.19.4.4 
*/
char *tmpnam(char *s);

/*@
  requires \valid(stream);
  assigns \result \from stream, stream->__fc_stdio_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.4.1 
*/
int fclose(FILE *stream);

/*@
  requires stream == \null || \valid_read(stream);
  assigns \result \from stream, stream->__fc_stdio_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.5.2
 */
int fflush(FILE *stream);

FILE __fc_fopen[__FC_FOPEN_MAX];
FILE* const __p_fc_fopen = __fc_fopen;

/*@ 
  assigns \result \from filename[..],mode[..], __p_fc_fopen; 
  ensures
  \result==\null
  || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1])) ;
*/ 
FILE *fopen(const char * restrict filename,
     const char * restrict mode);

/*@ assigns \result \from fildes,mode[..]; 
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE)));
 */
FILE *fdopen(int fildes, const char *mode);

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

/*@ assigns *stream \from stream->__fc_stdio_id; */
// unsupported...
int fprintf(FILE * restrict stream,
     const char * restrict format, ...);

/*@ assigns *stream \from stream->__fc_stdio_id;
// unsupported...
 */
int fscanf(FILE * restrict stream,
     const char * restrict format, ...);

/*@ assigns *__fc_stdout \from format[..];
// unsupported...
*/
int printf(const char * restrict format, ...);

/*@ assigns *__fc_stdin; 
// unsupported...
 */
int scanf(const char * restrict format, ...);

/*@ assigns s[0..n-1]; 
// unsupported...
 */
int snprintf(char * restrict s, size_t n,
    const char * restrict format, ...);

/*@ assigns s[0..]; 
// unsupported...
 */
int sprintf(char * restrict s,
     const char * restrict format, ...);

// unsupported...
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

/*@ assigns s[0..n-1] \from format[..], arg; 
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

/*@ assigns s[0..n-1],*stream \from *stream;
  assigns \result \from s,n,*stream;
  ensures \result == \null || \result==s;
 */
char *fgets(char * restrict s, int n,
    FILE * restrict stream);

/*@ assigns *stream ; */
int fputc(int c, FILE *stream);

/*@ assigns *stream \from s[..]; */
int fputs(const char * restrict s,
     FILE * restrict stream);

/*@ assigns \result,*stream \from *stream; */
int getc(FILE *stream);

/*@ assigns \result \from *__fc_stdin ; */
int getchar(void);

/*@ assigns s[..] \from *__fc_stdin ;
  assigns \result \from s, __fc_stdin;
  ensures \result == s || \result == \null;
 */
char *gets(char *s);

/*@ assigns *stream \from c; */
int putc(int c, FILE *stream);

/*@ assigns *__fc_stdout \from c; */
int putchar(int c);

/*@ assigns *__fc_stdout \from s[..]; */
int puts(const char *s);

/*@ assigns *stream \from c; */
int ungetc(int c, FILE *stream);

/*@
  requires \valid(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(stream);
  assigns *(((char*)ptr)+(0..(nmemb*size)-1)) \from size, nmemb, *stream;
  assigns \result \from size, *stream;
  ensures \result <= nmemb;
  ensures \initialized(((char*)ptr)+(0..(\result*size)-1));
  //TODO: specify precise fields from struct FILE
*/
size_t fread(void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@
  requires \valid_read(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(stream);
  assigns *stream, \result \from *(((char*)ptr)+(0..(nmemb*size)-1));
  ensures \result <= nmemb;
  //TODO: specify precise fields from struct FILE
*/
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
int fileno(FILE *stream);

/*@ assigns *stream \from \nothing ;*/
void flockfile(FILE *stream);

/*@ assigns *stream \from \nothing ;*/
void funlockfile(FILE *stream);

/*@ assigns \result,*stream \from \nothing ;*/
int ftrylockfile(FILE *stream);

/*@ assigns \result \from *stream ;*/
int ferror(FILE *stream);

/*@ assigns __fc_stdout \from __FC_errno, s[..]; */
void perror(const char *s);

/*@ assigns \result,*stream \from *stream; */
int getc_unlocked(FILE *stream);
/*@ assigns \result \from *__fc_stdin ; */
int getchar_unlocked(void);
/*@ assigns *stream \from c; */
int putc_unlocked(int c, FILE *stream);
/*@ assigns *__fc_stdout \from c; */
int putchar_unlocked(int c);

/*@  assigns *stream  \from \nothing; */
void clearerr_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
int feof_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
int ferror_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
int fileno_unlocked(FILE *stream);
int fflush_unlocked(FILE *stream);
int fgetc_unlocked(FILE *stream);
int fputc_unlocked(int c, FILE *stream);
size_t fread_unlocked(void *ptr, size_t size, size_t n,
                             FILE *stream);
size_t fwrite_unlocked(const void *ptr, size_t size, size_t n,
		       FILE *stream);

char *fgets_unlocked(char *s, int n, FILE *stream);
int fputs_unlocked(const char *s, FILE *stream);

__END_DECLS

#define IOV_MAX 1024

#endif
