/*@ predicate rel(unsigned long long x, unsigned long long y) = x == y; */

/*@ predicate rel(char x, char y, integer foo) = x == y; */

/*@ ensures rel(\result, x); */
char f(int x) { return x; }
