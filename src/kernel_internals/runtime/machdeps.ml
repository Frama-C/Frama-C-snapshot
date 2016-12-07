(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

open Cil_types

let x86_16 = {
  version          = 
    "x86 16 bits mode (gcc like compiler) with big or huge memory model";
  compiler = "generic";
  sizeof_short     = 2;
  sizeof_int       = 2;
  sizeof_long      = 4;
  sizeof_longlong  = 8;
  sizeof_ptr       = 4;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 16;
  sizeof_void      = 1;
  sizeof_fun       = 1;
  size_t = "unsigned int";
  wchar_t = "int";
  ptrdiff_t = "int";
  alignof_short = 2;
  alignof_int = 2;
  alignof_long = 4;
  alignof_longlong = 4;
  alignof_ptr = 4;
  alignof_float = 2;
  alignof_double = 8;
  alignof_longdouble = 16;
  alignof_str = 1;
  alignof_fun = 1;
  alignof_aligned= 8;
         (* I don't know if attribute aligned is supported by any 16bits 
            compiler. *)
  char_is_unsigned = false;
  const_string_literals = true;
  little_endian = true;
  underscore_name = true ;
  has__builtin_va_list = true;
  __thread_is_keyword = true;
}

let gcc_x86_16 = { x86_16 with compiler = "gcc" }

let x86_32 = {
  version          = "gcc 4.0.3 - X86-32bits mode";
  compiler         = "generic";
  sizeof_short     = 2;
  sizeof_int       = 4;
  sizeof_long      = 4;
  sizeof_longlong  = 8;
  sizeof_ptr       = 4;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 12;
  sizeof_void      = 1;
  sizeof_fun       = 1;
  size_t = "unsigned int";
  wchar_t = "int";
  ptrdiff_t = "int";
  alignof_short = 2;
  alignof_int = 4;
  alignof_long = 4;
  alignof_longlong = 4;
  alignof_ptr = 4;
  alignof_float = 4;
  alignof_double = 4;
  alignof_longdouble = 4;
  alignof_str = 1;
  alignof_fun = 1;
  alignof_aligned= 16;
  char_is_unsigned = false;
  const_string_literals = true;
  little_endian = true;
  underscore_name = false ;
  has__builtin_va_list = true;
  __thread_is_keyword = true;
}

let gcc_x86_32 = { x86_32 with compiler = "gcc" }

let x86_64 = {
  version          = "gcc 4.0.3 AMD64";
  compiler         = "generic";
  sizeof_short     = 2;
  sizeof_int       = 4;
  sizeof_long      = 8;
  sizeof_longlong  = 8;
  sizeof_ptr       = 8;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 16;
  sizeof_void      = 1;
  sizeof_fun       = 1;
  size_t = "unsigned long";
  wchar_t = "int";
  ptrdiff_t = "long";
  alignof_short = 2;
  alignof_int = 4;
  alignof_long = 8;
  alignof_longlong = 8;
  alignof_ptr = 8;
  alignof_float = 4;
  alignof_double = 8;
  alignof_longdouble = 16;
  alignof_str = 1;
  alignof_fun = 1;
  alignof_aligned= 16;
  char_is_unsigned = false;
  const_string_literals = true;
  little_endian = true;
  underscore_name = false ;
  has__builtin_va_list = true;
  __thread_is_keyword = true;
}

let gcc_x86_64 = { x86_64 with compiler = "gcc" }

let ppc_32 = {
  version          = "4.0.1 (Apple Computer, Inc. build 5367)";
  compiler         = "standard";
  sizeof_short     = 2;
  sizeof_int       = 4;
  sizeof_long      = 4;
  sizeof_longlong  = 8;
  sizeof_ptr       = 4;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 16;
  sizeof_void      = 1;
  sizeof_fun       = 1;
  size_t = "unsigned long";
  wchar_t = "int";
  ptrdiff_t = "int";
  alignof_short = 2;
  alignof_int = 4;
  alignof_long = 4;
  alignof_longlong = 4;
  alignof_ptr = 4;
  alignof_float = 4;
  alignof_double = 4;
  alignof_longdouble = 16;
  alignof_str = 1;
  alignof_fun = 4;
  alignof_aligned= 16;
  char_is_unsigned = false;
  const_string_literals = true;
  little_endian = false;
  underscore_name = false ;
  has__builtin_va_list = true;
  __thread_is_keyword = true;
}

let msvc_x86_64 = {
  version          = "MSVC - X86-64bits mode";
  compiler         = "msvc";
  sizeof_short     = 2;
  sizeof_int       = 4;
  sizeof_long      = 4;
  sizeof_longlong  = 8;
  sizeof_ptr       = 8;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 8;
  sizeof_void      = 0;
  sizeof_fun       = 0; (* sizeof(f) results in a compilation error *)
  size_t = "unsigned long long"; (* defined as 'unsigned __int64' *)
  wchar_t = "unsigned short";
  ptrdiff_t = "long long"; (* defined as '__int64' *)
  alignof_short = 2;
  alignof_int = 4;
  alignof_long = 4;
  alignof_longlong = 8;
  alignof_ptr = 8;
  alignof_float = 4;
  alignof_double = 8;
  alignof_longdouble = 8;
  alignof_str = 1; (* __alignof("a") results in compilation error C2059:
                      syntax error: 'string' *)
  alignof_fun = 8;
  alignof_aligned= 1; (* MSVC does not seem to have an 'align' attribute
                         equivalent to GCC's *)
  char_is_unsigned = false;
  const_string_literals = true;
  little_endian = true;
  underscore_name = false ;
  has__builtin_va_list = false;
  __thread_is_keyword = false;
}
