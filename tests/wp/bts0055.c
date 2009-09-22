int T[10];

// acces to *(\result)
/*@ requires 0 <= x < 10;
    ensures *\result == x && \base_addr (\result) == \base_addr(T);
    assigns T[x];
*/
int * ret_ptr (int x) {
  T[x] = x;
  return T+x;
}

//@ ensures \result == 0;
int call_ret_ptr (void) {
  int * p = ret_ptr (0);
  return *p;
}
