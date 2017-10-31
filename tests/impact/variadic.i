/* run.config
   COMMENT: also tests the parsing of cmdline options of type string_set
   STDOPT: +"-impact-pragma main" +"-then -main main1 -impact-pragma='-main,+main1'" +"-then -main main2 -impact-pragma='-@all,+main2'" +"-then -main main3 -impact-pragma='+aux3,-main2'" +"-then -main main4 -impact-pragma='-aux3,+aux4'" 
   */

int f(int, ...);

int main () {
  int i=0;
  /*@ impact pragma stmt; */
  i++;
  f(i);
}

int y;

//@ assigns y \from y, x;
void g1(int x, ...) {
  y = x + y;
}

//@ assigns y \from x;
void g2(int x, ...);


int main1() {
  int x = 3;
  //@ impact pragma stmt;
  g1(1, 2, 3);
  g1(1, 2);
  return y;
}

int main2() {
  int x = 3;
  //@ impact pragma stmt;
  g2(1, 2, 3);
  g2(1, 2);
  return y;
}


int z;

//@ assigns z \from y;
void g3(int , ...);

int aux3(int x, ...) {
  int t = 3;
  //@ impact pragma stmt;
  g1(t);
  g1(t);
  return y;
}

int main3() {
  aux3(1, 2);
  aux3(2, 3);
  return y;
}

void aux4(int x) {
  //@ impact pragma stmt;
  y = x;
}

int aux4bis(int x, ...) {
  aux4(x);
  return y;
}

int main4() {
  aux4bis(1, 2);
  aux4bis(1, 2, 3);
  return y;
}
