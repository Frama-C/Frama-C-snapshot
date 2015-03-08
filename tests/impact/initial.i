/* run.config
   COMMENT: also tests the parsing of cmdline options of type string_set
   STDOPT: +"-pdg-verbose 0" +"-main main1 -impact-pragma g1" +"-then -main main2 -impact-pragma='-@all,+g2'" +"-then -main main3 -impact-pragma='-g2,+g3'" 
*/

int x1, x2, y2, z2, x3;
volatile int c;

/* First case: Out(x1) in main1 is not impacted. It is equivalent to
   Out(x1) in g1, which is part of the initial query */

void f1() {
  x1 = 1;
}

void g1() {
  if (c) {
    //@ impact pragma stmt;
    f1();
  }
}

void main1() {
  while(1) {
    g1();
  }
}

/* Second case: Out(x2) in main2 IS impacted, due to the circular dependency
   x2 <- y2 <- z2; However, this cannot be seen immediately, so the nodes
   Out(x2) in in h2 and main2 are first put in the worklist as belonging
   to the initial impact. */

void f2() {
  x2 = y2;
}


void aux2() {
  y2 = z2;
}

void g2() {
  if (c) {
    //@ impact pragma stmt;
    f2();
    if (c) aux2();
  }
}

void h2() {
  g2();
  z2 = x2;
}

void main2() {
  while(1) {
    h2();
  }
}

void f3() {
  x3 = 1;
}


void g3() {
  //@ impact pragma stmt;
  f3();
  if (c) {
    x3 = x3;
  }
}


/* Third case: Out(x3) in main3 is impacted, as it represents Out(x3) in
   the call to f3(), AND x3=x3. However, Out(x3) in the call to f3() is not
   self-impacting */
void main3() {
  while(1) {
    g3();
  }
}
