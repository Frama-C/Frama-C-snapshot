/* run.config
STDOPT: +"@PTEST_DIR@/inline_def_2.i"
*/

// inline definition can be used in this translation unit, but does not
// preclude an external definition to exist in another one.
inline int f(int x) { return x; }

inline int f1() { return 1; }

// this time, f2 is a normal external definition.
extern inline f2() { return 3; }

int g(int x) { return f(x) + f1() + f2 (); }
