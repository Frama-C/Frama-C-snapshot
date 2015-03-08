
open Cil_types

let md = {
	 version          = "";
         compiler = "gcc";
	 sizeof_short     = 2;
	 sizeof_int       = 2;
	 sizeof_long      = 4;
	 sizeof_longlong  = 8;
	 sizeof_ptr       = 4;
	 sizeof_float     = 4;
	 sizeof_double    = 8;
	 sizeof_longdouble  = 8;
	 sizeof_void      = -1;
	 sizeof_fun       = 0;
	 alignof_short = 2;
	 alignof_int = 2;
	 alignof_long = 2;
	 alignof_longlong = 2;
	 alignof_ptr = 2;
	 alignof_float = 2;
	 alignof_double = 2;
	 alignof_longdouble = 2;
	 alignof_str = 0;
	 alignof_fun = 0;
         alignof_aligned= 0;
	 char_is_unsigned = true;
	 const_string_literals = true;
	 little_endian = true;
	 underscore_name = true ;
	 size_t = "unsigned int";
	 wchar_t = "int";
	 ptrdiff_t = "int";
         has__builtin_va_list = true;
         __thread_is_keyword = false;
  }

let () =
  File.new_machdep "unsigned_char" md
