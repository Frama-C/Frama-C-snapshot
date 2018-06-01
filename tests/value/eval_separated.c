int x, y, t1[6], t2[6];

void main (int c1, int c2) {
  int *p, *q, *r;
  //@ assert !\separated(&x, &x);
  //@ assert \separated(&x, &y);

  //@ assert !\separated(&x+2, &x+2);
  //@ assert \separated(&x+2, &x+3);

  q = (int)&q+ (int)&q;
  r = (int)&r+ (int)&r;
  //@ assert !\separated(q, q);
  //@ assert \separated(q, q+2);
  //@ assert \separated(q, r);

  //@ assert !\separated(&t1[1-1],&t1[0]);
  //@ assert !\separated(&t1, &t1);

  //@ assert !\separated(&t1[0]+(0..0), &t1[0]);
  //@ assert \separated(&t1[0]+(0.. -1), &t1[0]);

  //@ assert !\separated(&t1[0]+2, &t1[0]+2);
  //@ assert \separated(&t1[0]+2, &t1[0]+3);

  //@ assert \separated(&t1[0], &t2[0]); // first cells are separated
  //@ assert \separated(&t1, &t2); // all arrays are separated
  //@ assert \separated(&t1[0], &t1[1]);

  //@ assert \separated(&t1[0]+(0..3), &t1[0]+(4..5));
  //@ assert !\separated(&t1[0]+(0..3), &t1[0]+(3..5));

  //@ assert \separated(&t1[c1], &t2[c1]);

  p= &x;
  //@ assert !\separated(&x, p);
  p = &t1[c1];
  //@ assert \separated(p, &t2[c2]);

  if (c1 >= 0 && c1 <= 3 && c2 >= 3 && c2 <= 4) {
    //@ assert \separated(&t1[c1], &t1[c2+1]);

    //@ assert \separated(&t1[c1], &t1[c2]); // Really unknown

    //@ assert \separated(&t1[c1], &t1[c1+1]); // Unknown by imprecision
  }
}
