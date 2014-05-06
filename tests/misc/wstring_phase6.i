/* run.config
   OPT: -journal-disable -print 
*/

// See http://stackoverflow.com/questions/18102502/mixing-wide-and-narrow-string-literals-in-c
main(){
printf( "%s\n", "123" "456" );
printf( "%ls\n", L"123" L"456" );
printf( "%ls\n", "123" L"456" );
printf( "%ls\n", L"123" "456" );
printf( "%ls\n", L"123" L"456" );
}
