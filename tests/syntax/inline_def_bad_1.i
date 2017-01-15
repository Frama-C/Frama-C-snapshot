/* run.config
STDOPT: +"@PTEST_DIR@/inline_def_bad_2.i"
*/

extern inline f() { return 1; }

int g() { return f(); }
