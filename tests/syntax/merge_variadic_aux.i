/* run.config
DONTRUN: auxiliary test file for merge_variadic.i
*/

/*@ assigns \result \from filename[0..], flags ; */
extern int open(const char *filename, int flags, ...);

extern int foo(int x, ...);

int bar () { return foo(3,4); }
