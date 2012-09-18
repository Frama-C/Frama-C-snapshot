typedef __attribute__((FC_BUILTIN)) int foo;

enum __attribute__ ((FC_BUILTIN)) bar { bla, bli };

struct __attribute__ ((FC_BUILTIN)) baz { int x; };

enum discard { a,b,c };

struct discard { int y; };
