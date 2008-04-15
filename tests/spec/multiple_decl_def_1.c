/* run.config
   OPT: -print tests/spec/multiple_decl_def_2.c -journal-disable
*/

/* see bug #43 && #128 */

/*@ requires x >= 0; */
extern int f(int x);

/*@ requires x >= 0; */
extern int g(int x);

int main () { g(0); return f(0); }
