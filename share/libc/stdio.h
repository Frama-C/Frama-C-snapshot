/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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
#include "__fc_define_ssize_t.h"

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define BUFSIZ __FC_BUFSIZ
#define FOPEN_MAX __FC_FOPEN_MAX
#define FILENAME_MAX __FC_FILENAME_MAX
#ifndef __FC_L_tmpnam
#error machdep should have defined __FC_L_tmpnam!
#endif
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

/*@ // missing: assigns 'filesystem' \from filename[0..];
    // missing: assigns errno one of several possible values
  requires valid_filename: valid_read_string(filename);
  assigns \result \from indirect:filename[0..strlen(filename)];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int remove(const char *filename);

/*@ // missing: assigns 'filesystem' \from old_name[0..], new_name[0..];
    // missing: assigns errno one of 21 different possible values
  requires valid_old_name: valid_read_string(old_name);
  requires valid_new_name: valid_read_string(new_name);
  assigns \result \from indirect:old_name[0..strlen(old_name)],
                        indirect:new_name[0..strlen(new_name)];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int rename(const char *old_name, const char *new_name);

FILE __fc_fopen[__FC_FOPEN_MAX];
FILE* const __fc_p_fopen = __fc_fopen;

/*@
  assigns \result \from __fc_p_fopen;
  ensures result_null_or_valid_fd:
    \result == \null || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1]));
*/
extern FILE *tmpfile(void);

char __fc_tmpnam[L_tmpnam];
char * const __fc_p_tmpnam = __fc_tmpnam;

/*@
  // Note: the tmpnam example in POSIX uses an array of size L_tmpnam+1
  // missing: assigns __fc_p_tmpnam[0..L_tmpnam] \from 'PRNG and internal state'
  // missing: if called more than TMP_MAX, behavior is implementation-defined
  requires valid_s_or_null: s == \null || \valid(s+(0 .. L_tmpnam));
  assigns __fc_p_tmpnam[0 .. L_tmpnam] \from __fc_p_tmpnam[0 .. L_tmpnam],
                                             indirect:s;
  assigns s[0 .. L_tmpnam] \from indirect:s, __fc_p_tmpnam[0 .. L_tmpnam];
  assigns \result \from s, __fc_p_tmpnam;
  ensures result_string_or_null: \result == \null || \result == s ||
                                 \result == __fc_p_tmpnam;
*/
extern char *tmpnam(char *s);

/*@
  // missing: assigns errno
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:stream, indirect:*stream;
  ensures result_zero_or_EOF: \result == 0 || \result == EOF;
*/
extern int fclose(FILE *stream);

/*@
  // missing: assigns errno
  requires null_or_valid_stream: stream == \null || \valid_read(stream);
  ensures result_zero_or_EOF: \result == 0 || \result == EOF;
  assigns \result
    \from indirect:*stream, indirect:__fc_fopen[0 .. __FC_FOPEN_MAX-1];
  assigns *stream, __fc_fopen[0 .. __FC_FOPEN_MAX-1]
    \from indirect:stream, *stream,
          __fc_fopen[0 .. __FC_FOPEN_MAX-1]; // may flush ALL open streams
  behavior flush_all:
    assumes all_streams: stream == \null;
    assigns __fc_fopen[0 .. __FC_FOPEN_MAX-1]
      \from __fc_fopen[0 .. __FC_FOPEN_MAX-1]; // flush ALL open streams
    assigns \result \from indirect:__fc_fopen[0 .. __FC_FOPEN_MAX-1];
  behavior flush_stream:
    assumes single_stream: stream != \null;
    assigns *stream \from *stream;
    assigns \result \from indirect:*stream;
  complete behaviors;
  disjoint behaviors;
 */
extern int fflush(FILE *stream);

/*@
  requires valid_filename: valid_read_string(filename);
  requires valid_mode: valid_read_string(mode);
  assigns \result \from indirect:filename[0..strlen(filename)],
                        indirect:mode[0..strlen(mode)], __fc_p_fopen;
  ensures result_null_or_valid_fd:
    \result==\null || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1])) ;
*/ 
extern FILE *fopen(const char * restrict filename,
     const char * restrict mode);

/*@
  requires valid_mode: valid_read_string(mode);
  assigns \result, __fc_fopen[fd] \from indirect:fd,
                                        indirect:mode[0..strlen(mode)],
                                        indirect:__fc_fopen[fd], __fc_p_fopen;
  ensures result_null_or_valid_fd:
    \result == \null || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1])) ;
 */
extern FILE *fdopen(int fd, const char *mode);

/*@
  requires valid_filename: valid_read_string(filename);
  requires valid_mode: valid_read_string(mode);
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:filename[..], indirect:mode[..], __fc_p_fopen,
                        indirect:stream;
  assigns *stream \from indirect:filename[..], indirect:mode[..], __fc_p_fopen,
                        indirect:stream;
  ensures result_null_or_valid_fd:
    \result==\null || \result \in &__fc_fopen[0 .. __FC_FOPEN_MAX-1];
  ensures stream_opened:
    *stream \in __fc_fopen[0 .. __FC_FOPEN_MAX-1];
*/
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

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from *stream;
  assigns \result \from indirect:*stream;
  ensures result_uchar_or_eof: 0 <= \result <= __FC_UCHAR_MAX || \result == EOF;
 */
extern int fgetc(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  requires room_s: \valid(s+(0..size-1));
  assigns s[0..size-1] \from indirect:size, indirect:*stream;
  assigns \result \from s, indirect:size, indirect:*stream;
  ensures result_null_or_same: \result == \null || \result == s;
  ensures initialization:at_least_one:\result != \null ==> \initialized(&s[0]);
          // the return value does not tell how many characters were written,
          // so we can only ensure the first one was initialized
  ensures terminated_string_on_success:
    \result != \null ==> valid_string(s);
 */
extern char *fgets(char * restrict s, int size,
    FILE * restrict stream);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from c, *stream;
  assigns \result \from indirect:*stream;
*/
extern int fputc(int c, FILE *stream);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns *stream \from s[0..strlen(s)], *stream;
  assigns \result \from indirect:s[0..strlen(s)], indirect:*stream;
*/
extern int fputs(const char * restrict s,
     FILE * restrict stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result, *stream \from *stream;
*/
extern int getc(FILE *stream);

/*@
  assigns \result, *__fc_stdin \from *__fc_stdin;
*/
extern int getchar(void);

// Number of characters that will read by gets()
/*@
  axiomatic GetsLength {
    logic size_t gets_length{L} reads *__fc_stdin;
  }
*/

/*@
  requires room_s: \valid(s+(0..gets_length));
  assigns s[0..gets_length] \from *__fc_stdin ;
  assigns \result \from s, *__fc_stdin;
  assigns *__fc_stdin \from *__fc_stdin;
  ensures result_null_or_same: \result == s || \result == \null;
 */
extern char *gets(char *s);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from c, *stream;
  assigns \result \from indirect:*stream;
*/
extern int putc(int c, FILE *stream);

/*@
  assigns *__fc_stdout \from c, *__fc_stdout;
  assigns \result \from indirect:*__fc_stdout;
*/
extern int putchar(int c);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns *__fc_stdout \from s[0..strlen(s)], *__fc_stdout;
  assigns \result \from indirect:s[0..strlen(s)], indirect:*__fc_stdout;
*/
extern int puts(const char *s);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from indirect:c;
  assigns \result \from indirect:c, indirect:*stream;
  ensures result_ok_or_error: \result == c || \result == EOF;
*/
extern int ungetc(int c, FILE *stream);

/*@
  requires valid_ptr_block: \valid(((char*)ptr)+(0..(nmemb*size)-1));
  requires valid_stream: \valid(stream);
  assigns *(((char*)ptr)+(0..(nmemb*size)-1)), *stream
    \from indirect:size, indirect:nmemb, indirect:*stream;
  assigns \result \from size, indirect:*stream;
  ensures size_read: \result <= nmemb;
  ensures initialization: \initialized(((char*)ptr)+(0..(\result*size)-1));
*/
extern size_t fread(void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@
  requires valid_ptr_block: \valid_read(((char*)ptr)+(0..(nmemb*size)-1));
  requires valid_stream: \valid(stream);
  assigns *stream, \result \from indirect:*(((char*)ptr)+(0..(nmemb*size)-1));
  ensures size_written: \result <= nmemb;
*/
extern size_t fwrite(const void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict stream);

/*@
  requires valid_stream: \valid(stream);
  requires valid_pos: \valid(pos);
  requires initialization:pos: \initialized(pos);
  assigns *pos \from indirect:*stream;
  assigns \result \from indirect:*stream;
 */
extern int fgetpos(FILE * restrict stream,
     fpos_t * restrict pos);

/*@
  requires valid_stream: \valid(stream);
  requires whence_enum: whence == SEEK_SET || whence == SEEK_CUR || whence == SEEK_END;
  assigns *stream \from *stream, indirect:offset, indirect:whence;
  assigns \result, __fc_errno \from indirect:*stream, indirect:offset,
                                    indirect:whence;
*/
extern int fseek(FILE *stream, long int offset, int whence);

/*@
  requires valid_stream: \valid(stream);
  requires valid_pos: \valid_read(pos);
  requires initialization:pos: \initialized(pos);
  assigns *stream \from *pos;
*/
extern int fsetpos(FILE *stream, const fpos_t *pos);

/*@
  requires valid_stream: \valid(stream);
  assigns \result, __fc_errno \from indirect:*stream ;
  ensures success_or_error:
    \result == -1 || (\result >= 0 && __fc_errno == \old(__fc_errno));
*/
extern long int ftell(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from \nothing;
 */
extern void rewind(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream  \from \nothing;
 */
extern void clearerr(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
extern int feof(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
extern int fileno(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from \nothing;
*/
extern void flockfile(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from \nothing;
*/
extern void funlockfile(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result,*stream \from \nothing;
*/
extern int ftrylockfile(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
extern int ferror(FILE *stream);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns __fc_stdout \from __fc_errno, s[0..strlen(s)];
 */
extern void perror(const char *s);

/*@
  requires valid_stream: \valid(stream);
  assigns \result,*stream \from *stream;
*/
extern int getc_unlocked(FILE *stream);

/*@
  assigns \result \from *__fc_stdin;
 */
extern int getchar_unlocked(void);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream \from c;
  assigns \result \from indirect:*stream;
*/
extern int putc_unlocked(int c, FILE *stream);

/*@
  assigns *__fc_stdout \from c;
  assigns \result \from indirect:*__fc_stdout;
 */
extern int putchar_unlocked(int c);

/*@
  requires valid_stream: \valid(stream);
  assigns *stream  \from \nothing;
*/
extern void clearerr_unlocked(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
extern int feof_unlocked(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
extern int ferror_unlocked(FILE *stream);

/*@
  requires valid_stream: \valid(stream);
  assigns \result \from indirect:*stream;
*/
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

/*@ axiomatic pipe_streams {
  predicate is_open_pipe{L}(FILE *stream); // is stream an open pipe?
  // Logic label L is not used, but must be present because the
  // predicate depends on the memory state
  }
*/

/*@
  requires valid_command: valid_read_string(command);
  requires valid_type: valid_read_string(type);
  assigns \result \from indirect:*command, indirect:*type,
    __fc_p_fopen;
  assigns __fc_fopen[0..] \from indirect:*command, indirect:*type,
    __fc_fopen[0..];
  ensures result_error_or_valid_open_pipe:
    \result == \null ||
    (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1]) &&
     is_open_pipe(\result));
*/
extern FILE *popen(const char *command, const char *type);

/*@
  requires valid_stream: \valid(stream);
  requires open_pipe: is_open_pipe(stream);
  assigns \result \from indirect:*stream;
  ensures closed_stream: !is_open_pipe(stream);
*/
extern int pclose(FILE *stream);

// This file may be included by non-POSIX machdeps, which do not define
// ssize_t, so we must check it
#ifdef __FC_POSIX_VERSION
// No specification given; include "stdio.c" to use Frama-C's implementation
ssize_t getline(char **lineptr, size_t *n, FILE *stream);
#endif

__END_DECLS

#define IOV_MAX 1024

__POP_FC_STDLIB
#endif
