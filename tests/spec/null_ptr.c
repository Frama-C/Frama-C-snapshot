//@ predicate null(char *x) = x == 0;
//@ predicate eq(char *x, char *y) = x == y;
//@ predicate my_null(char *x) = x == (void*)0;

void f(char *x) {
  x = 0;
  //@ assert x == (void*)0;
  //@ assert my_null(x);
  //@ assert null(x);
  //@ assert eq(x,0);
}

//@ ensures \result == \true;
int g() { return 1; }

/*@ predicate foo (integer x) = x != 0; */

//@ predicate totology_1{L} = !\valid{L}((char*)\null);
//@ predicate totology_2{L1,L2} = !\fresh{L1,L2}(\null,1);

//@ predicate error_1 = foo(\true); // -> Ignored global annotation
//@ predicate error_2{L} = \valid{L}(\null); // -> Ignored global annotation
//@ predicate error_3{L} = \valid_read{L}(\null); // -> Ignored global annotation
