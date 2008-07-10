/*@ requires n >= 0;
    requires \valid(p+ (0..n-1));
    assigns \nothing;
    ensures \result == \sum(0,n-1,\lambda integer i; p[i]*p[i]);
*/
int sqsum(int* p, int n);

#define INT_MAX (1<<30-1)

int sqsum(int* p, int n) {
  int S=0, tmp;
  for(int i = 0; i < n; i++) {
    //@ assert p[i] * p[i] <= INT_MAX;
    tmp = p[i] * p[i];
    //@ assert tmp >= 0;
    //@ assert S + tmp <= INT_MAX;
    S += tmp;
  }
  return S;
}
