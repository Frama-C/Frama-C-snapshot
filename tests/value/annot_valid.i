int G;
int main (int u) {
  int *p;
L:  p = &G; char *c = &G;

  switch (u) {
  case 0:
  //@ assert \valid(p);
    break;
  case 1:
  //@ assert \valid(p+1);
    break;
  case 2:
  //@ assert \valid((char*)p+1);
    break;
  case 3:
  //@ assert \valid(c+1);
    break;
  case 4:
  //@ assert \valid(c+3);
    break;
  case 5:
  //@ assert \valid(c+4);
    break;
  case 6:
  //@ assert (char *)p < c;
    break;
  case 7:
  //@ assert p <= (int*)1;
    break;
  case 8:
  //@ assert (int)p == 3;
    break;
  case 9:
  //@ assert (int)p != 3;
    break;
  case 10:
  //@ assert \exists int x ;  x != 0 ==> *p == x;
    break;
  case 11:
  //@ assert \forall int x ;  \true;
    break;
  case 12:
  //@ assert \valid((long long *)5);
    break;
  case 13:
  //@ assert \valid(p);
    break;
  case 14:
  //@ assert (\valid((char*)5));
    break;
  case 15:
  //@ assert p != \null;
    break;
  case 16:
  //@ assert \valid{L}(p);
  //@ assert !\at(\valid(p), L);
    break;
  case 17: {
    int x;
    p = &x;
    //@ assert !\valid{L}(p); // Incorrect
    break;
  }
  }

  return 0;
}
