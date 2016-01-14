open Cil_types

let mach =
{
  version          = "foo";
  compiler         = "bar";
  sizeof_short     = 2;
  sizeof_int       = 3;
  sizeof_long      = 4;
  sizeof_longlong  = 8;
  sizeof_ptr       = 4;
  sizeof_float     = 4;
  sizeof_double    = 8;
  sizeof_longdouble  = 12;
  sizeof_void      = 1;
  sizeof_fun       = 1;
  size_t = "unsigned long";
  wchar_t = "int";
  ptrdiff_t = "int";
  alignof_short = 2;
  alignof_int = 3;
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

let () = File.new_machdep "custom" mach

let () = Kernel.Machdep.set "custom"
