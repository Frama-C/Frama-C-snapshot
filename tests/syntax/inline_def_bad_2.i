/* run.config
DONTRUN: main test is inline_def_bad_1.i
*/

// should be an error: we have two definitions for the same function
extern inline f() { return 42; }

int h() { return f(); }
