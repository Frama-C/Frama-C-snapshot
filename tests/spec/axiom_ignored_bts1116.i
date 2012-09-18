
/*@ axiom l: // must be rejected as axiom outside an axiomatic
    \forall int i; i < 0;
 */

struct _str {
    int x;
};

//@ ensures \result < 0;
int ftest(int i) {
    return i;
}

