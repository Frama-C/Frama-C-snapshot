int t[10], u[11];
struct ts { int f1; int f2; } s1, s2, s3[10];
unsigned int x;

void eq_tsets () {

 //@ assert \union() == \union();

  //@ assert \union(1) == \union(1);
  //@ assert \union(1, 2) == \union(1, 2);
  //@ assert \union(2, 1) == \union(1, 2);
  //@ assert ! (\union(1, 2) == \union(1, 3));
  //@ assert ! (\union(1, 2) == \union(1));
  //@ assert ! (\union(1 ,2) == \union(3, 4));
  //@ assert \union(1, 2) != \union(1, 3);
  //@ assert \union(1, 2) != \union(1);
  //@ assert \union(1 ,2) != \union(3, 4);

  //@ assert \union(x, x+1) != \union(-1, -3);

  //@ assert \union(1.0) == \union(1.0);

  //@ assert \union(&t) == \union(&t);
  //@ assert ! (\union(&t[0..1]) == \union(&t[0..2]));
  //@ assert ! (\union(&t[0..1]) == \union(&t[2..3]));
  //@ assert (\union(&t[0..1]) == \union(&t[0..1]));

  // Seems to be OK according to the typing given by the kernel. The WP is also happy
  //@ assert \union(\union(1,2)) == \union(\union(1), \union(2));
  //@ assert \union(\union(1,2)) == \union(\union(1), 2);
  //@ assert \union(\union(1,2)) == \union(1, 2);
  //@ assert \union(\union(1,1)) == \union(\union(1), 1);

  //@ assert &s3[0..1].f2 != 0;
  //@ assert &s3[0 .. -1].f1 != &s3[0..1].f2;
  //@ assert &s3[0 .. 1].f1 != &s3[0..1].f1;

  //@ assert s1 == s2; // True at link-time
  //@ assert t != u; // false

  //@ assert \union(0) == \union(0.0); 
  //@ assert \union(1.0) == \union(1);
  //@ assert \union(1, 1.0) == \union(1.0, 1);

  //@ assert \union() != \union(x);

  //@ assert \inter(&t, &u) == \empty;

}

void eq_char() {
  char c = '\x82'; // equal to 130. Very different from \130 which is in octal
  Frama_C_show_each(c);
  //@ assert c == '\x82';
  //@ assert c == 130-256;
}

void casts() {
  //@ assert (float)5 == 5.;
  //@ assert (double)5 == 5.;
}

void main () {
  eq_tsets();
  eq_char();
  casts();
}
