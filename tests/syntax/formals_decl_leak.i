/* run.config
OPT: -check -print -load-script tests/syntax/formals_decl_leak.ml tests/syntax/formals_decl_leak_1.i
*/

void f(int x);

void g() { f(3); }
