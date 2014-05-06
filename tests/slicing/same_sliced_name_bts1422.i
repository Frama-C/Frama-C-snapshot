/* run.config
OPT: -main foo -slice-value y -then-on 'Slicing export' -print
*/

int y;

void foo(int x);

void foo(int x) { x++; y++; }

void (*ptr)(int x) = &foo;
