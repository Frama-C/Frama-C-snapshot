
#pragma IntModel(modulo)

/*@ requires \valid(up);
  @ behavior identity:
  @   assumes *up <= INT_MAX;
  @   ensures \result == *up;
  @*/
int uint_to_int(unsigned* up) {
  int* ip = (int*) up;
  return *ip;
}

/*@ requires \valid_range(uarr,0,9);
  @*/
void init_uint_through_int(unsigned* uarr) {
  int* cur = (int*)uarr;
  int i;
  //@ loop invariant i >= 0;
  for (i = 0; i < 10; ++i) {
    *(cur + i) = 0;
  }
}

//@ requires \valid(p);
int uchar_ptr(unsigned char* p) { return *p; }

//@ requires \valid(p);
int schar_ptr(signed char* p) { return *p; }

//@ requires \valid(p);
int char_ptr(char* p) {
  if (*p > CHAR_MAX) return uchar_ptr(p);
  return schar_ptr(p);
}


/*
Local Variables:
compile-command: "LC_ALL=C make cast_integer_pointers"
End:
*/
