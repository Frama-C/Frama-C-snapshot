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

/* ISO C: 7.19 */
#ifndef __FC_STDIO
#define __FC_STDIO
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
#include "__fc_string_axiomatic.h"
#include "stdarg.h"
#include "stddef.h"
#include "errno.h"
#include "__fc_define_stat.h"
#include "__fc_define_fpos_t.h"
#include "__fc_define_file.h"
#include "__fc_define_null.h"
#include "__fc_define_eof.h"

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define BUFSIZ __FC_BUFSIZ
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
  Note: currently some functions only consider the __fc_FILE_id field of FILE.
        This models the fact that operations on different files are considered
        non-interferent between them.
*/

/*@ assigns \nothing; */ 
extern int remove(const char *filename);

/*@ assigns \nothing; */ 
extern int rename(const char *old_name, const char *new_name);

/*@ assigns \nothing; 
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE))) ; */ 
extern FILE *tmpfile(void);

/*@
  assigns \result \from s[..]; 
  assigns s[..] \from \nothing; 
  // TODO: more precise behaviors from ISO C 7.19.4.4 
*/
extern char *tmpnam(char *s);

/*@
  requires \valid(stream);
  assigns \result \from stream, stream->__fc_FILE_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.4.1 
*/
extern int fclose(FILE *stream);

/*@
  requires stream == \null || \valid_read(stream);
  assigns \result \from stream, stream->__fc_FILE_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.5.2
 */
extern int fflush(FILE *stream);

FILE __fc_fopen[__FC_FOPEN_MAX];
FILE* const __fc_p_fopen = __fc_fopen;

/*@ 
  assigns \result \from filename[..],mode[..], __fc_p_fopen; 
  ensures
  \result==\null
  || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1])) ;
*/ 
extern FILE *fopen(const char * restrict filename,
     const char * restrict mode);

/*@ assigns \result \from fildes,mode[..]; 
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE)));
 */
extern FILE *fdopen(int fildes, const char *mode);

/*@ 
  assigns *stream; 
  ensures \result==\null || \result==stream ; */ 
extern FILE *freopen(const char * restrict filename,
              const char * restrict mode,
              FILE * restrict stream);

/*@ assigns *stream \from buf; */
extern void setbuf(FILE * restrict stream,
     char * restrict buf);

/*@ assigns *stream \from buf,mode,size; */
extern int setvbuf(FILE * restrict stream,
     char * restrict buf,
     int mode, size_t size);

/*@
  // Axiomatic used by the Variadic plugin to generate specifications
  // for some functions, e.g. snprintf().
  axiomatic format_length {
    //TODO: this logic function will be extended to handle variadic formats
    logic integer format_length{L}(char *format);
  }
*/

// Direct specifications for variadic functions are unsupported;
// use the Variadic plug-in instead.
extern int fprintf(FILE * restrict stream,
     const char * restrict format, ...);
extern int fscanf(FILE * restrict stream,
     const char * restrict format, ...);
extern int printf(const char * restrict format, ...);
extern int scanf(const char * restrict format, ...);
extern int snprintf(char * restrict s, size_t n,
    const char * restrict format, ...);
extern int sprintf(char * restrict s,
     const char * restrict format, ...);
extern int sscanf(const char * restrict s,
     const char * restrict format, ...);

/*@ assigns *stream \from format[..], arg; */
extern int vfprintf(FILE * restrict stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *stream \from format[..], *stream; 
// TODO: assign arg too. */
extern int vfscanf(FILE * restrict stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdout \from arg; */
extern int vprintf(const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdin \from format[..]; 
// TODO: assign arg too. */
extern int vscanf(const char * restrict format,
     va_list arg);

/*@ assigns s[0..n-1] \from format[..], arg; 
 */
extern int vsnprintf(char * restrict s, size_t n,
     const char * restrict format,
     va_list arg);

/*@ assigns s[0..] \from format[..], arg; 
 */
extern int vsprintf(char * restrict s,
     const char * restrict format,
     va_list arg);

/* TODO: assigns arg ; */
extern int vsscanf(const char * restrict s,
     const char * restrict format,
     va_list arg);

/*@ assigns *stream;
 */
extern int fgetc(FILE *stream);

/*@ assigns s[0..n-1],*stream \from *stream;
  assigns \result \from s,n,*stream;
  ensures \result == \null || \result==s;
 */
extern char *fgets(char * restrict s, int n,
    FILE * restrict stream);

/*@ assigns *stream ; */
extern int fputc(int c, FILE *stream);

/*@ assigns *stream \from s[..]; */
extern int fputs(const char * restrict s,
     FILE * restrict stream);

/*@ assigns \result,*stream \from *stream; */
extern int getc(FILE *stream);

/*@ assigns \result \from *__fc_stdin ; */
extern int getchar(void);

/*@ assigns s[..] \from *__fc_stdin ;
  assigns \result \from s, __fc_stdin;
  ensures \result == s || \result == \null;
 */
extern char *gets(char *s);

/*@ assigns *stream \from c; */
extern int putc(int c, FILE *stream);

/*@ assigns *__fc_stdout \from c; */
extern int putchar(int c);

/*@ assigns *__fc_stdout \from s[..]; */
extern int puts(const char *s);

/*@ assigns *stream \from c; */
extern int ungetc(int c, FILE *stream);

/*@
  requires \valid(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(stream);
  assigns *(((char*)ptr)+(0..(nmemb*size)-1)) \from size, nmemb, *stream;
  assigns \result \from size, *stream;
  ensures \result <= nmemb;
  ensures \initialized(((char*)ptr)+(0..(\result*size)-1));
  //TODO: specify precise fields from struct FILE
*/
extern size_t fread(void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@
  requires \valid_read(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(stream);
  assigns *stream, \result \from *(((char*)ptr)+(0..(nmemb*size)-1));
  ensures \result <= nmemb;
  //TODO: specify precise fields from struct FILE
*/
extern size_t fwrite(const void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@ assigns *pos \from *stream ; */
extern int fgetpos(FILE * restrict stream,
     fpos_t * restrict pos);

/*@
  requires \valid(stream);
  requires whence == SEEK_SET || whence == SEEK_CUR || whence == SEEK_END;
  assigns *stream \from *stream, indirect:offset, indirect:whence;
  assigns \result, __fc_errno \from indirect:*stream, indirect:offset,
                                    indirect:whence; */
extern int fseek(FILE *stream, long int offset, int whence);

/*@ assigns *stream \from *pos; */
extern int fsetpos(FILE *stream, const fpos_t *pos);

/*@ assigns \result, __fc_errno \from *stream ;*/
extern long int ftell(FILE *stream);

/*@  assigns *stream \from \nothing; */
extern void rewind(FILE *stream);

/*@  assigns *stream  \from \nothing; */
extern void clearerr(FILE *stream);

/*@ assigns \result \from *stream ;*/
extern int feof(FILE *stream);

/*@ assigns \result \from *stream ;*/
extern int fileno(FILE *stream);

/*@ assigns *stream \from \nothing ;*/
extern void flockfile(FILE *stream);

/*@ assigns *stream \from \nothing ;*/
extern void funlockfile(FILE *stream);

/*@ assigns \result,*stream \from \nothing ;*/
extern int ftrylockfile(FILE *stream);

/*@ assigns \result \from *stream ;*/
extern int ferror(FILE *stream);

/*@ assigns __fc_stdout \from __fc_errno, s[..]; */
extern void perror(const char *s);

/*@ assigns \result,*stream \from *stream; */
extern int getc_unlocked(FILE *stream);
/*@ assigns \result \from *__fc_stdin ; */
extern int getchar_unlocked(void);
/*@ assigns *stream \from c; */
extern int putc_unlocked(int c, FILE *stream);
/*@ assigns *__fc_stdout \from c; */
extern int putchar_unlocked(int c);

/*@  assigns *stream  \from \nothing; */
extern void clearerr_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
extern int feof_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
extern int ferror_unlocked(FILE *stream);
/*@ assigns \result \from *stream ;*/
extern int fileno_unlocked(FILE *stream);
extern int fflush_unlocked(FILE *stream);
extern int fgetc_unlocked(FILE *stream);
extern int fputc_unlocked(int c, FILE *stream);
extern size_t fread_unlocked(void *ptr, size_t size, size_t n,
                             FILE *stream);
extern size_t fwrite_unlocked(const void *ptr, size_t size, size_t n,
		       FILE *stream);

extern char *fgets_unlocked(char *s, int n, FILE *stream);
extern int fputs_unlocked(const char *s, FILE *stream);

extern int dprintf(int fd, const char *restrict format, ...);

__END_DECLS

#define IOV_MAX 1024

__POP_FC_STDLIB
#endif
