
typedef struct _wrap {
  int i;
} *ptr;

//@ ensures \result == 0;
ptr f() {
  ptr p;
  p = 0;
  return p;
}

//@ ensures \result == 0;
int* f2() {
  int* p;
  p = 0;
  return p;
}

#define MYNULL 0

//@ ensures \result == \null;
ptr g() {
  ptr p;
  p = MYNULL;
  return p;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make null2"
End:
*/
