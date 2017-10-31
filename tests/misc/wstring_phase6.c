/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -journal-disable -print -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -variadic-no-translation
*/
#include <stdio.h>

// See http://stackoverflow.com/questions/18102502/mixing-wide-and-narrow-string-literals-in-c
main(){
printf( "%s\n", "123" "456" );
printf( "%ls\n", L"123" L"456" );
printf( "%ls\n", "123" L"456" );
printf( "%ls\n", L"123" "456" );
printf( "%ls\n", L"123" L"456" );
}
