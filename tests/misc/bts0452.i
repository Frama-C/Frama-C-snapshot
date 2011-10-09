/* run.config
   OPT: -typecheck -load-script tests/misc/bts0452.ml
*/

/* must emit falls-through warning. */
int f (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    if (foo) return 1;
  }
}

/* must emit falls-through warning. */
int h (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    { if (foo) goto L ;
      return 1;
    L: break; }
  }
}

/* must NOT emit falls-through warning. */

int g (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    if (foo) return 1; else return 2;
  }
}

/* must NOT emit falls-through warning. */
int k (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    { goto L ;
      break;
    L: return 0; }
  }
}

/* must NOT emit falls-through warning. */
int l (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    { L: goto L ;
      break;
    }
  }
}

/* must NOT emit falls-through warning */
int main (int foo, char** args) {
  switch(foo) {
  case 1:
    return 0;
    break;

  default:
    return 1;
  }
}

/* must NOT emit falls-through warning */
int m (int foo, char** args) {
  if (foo >= 0 && foo <=10) { return 0; } else { return 1; }

}
