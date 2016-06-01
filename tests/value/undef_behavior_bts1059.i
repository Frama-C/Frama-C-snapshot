/* run.config*
STDOPT: #"-unspecified-access"
*/
/*@ assigns \nothing; */
void f(int,int);
/*@ assigns \nothing; */
void g(int,int);
volatile int c;

int main() {
    int a = 1;
    int b = 0;

    if (c)
      if (a = b || ++a == 2) //UB (no sequence point between ++a and a=...)
        f(a, b);
      else
        g(a, b);
    b = b++ || a--; // NO UB (we first incr b, decr a, set b to the result,
                    // with a sequence point between all operations.

    a = (a++,b++); // NO UB
    if (c) a = (b++, a++); // UB
    return 0;
}
