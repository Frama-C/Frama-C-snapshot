/* run.config
DONTRUN: main test is located in tests/syntax/formals_decl_leak.i
*/

void f(int y);

void h () { f(4); }
