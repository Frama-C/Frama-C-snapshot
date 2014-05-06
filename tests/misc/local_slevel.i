/* run.config
   STDOPT: +"-print"
*/

int *p;

void main() {
  int v;
  unsigned int r = 0;
  for (int i=0; i<80; i++) {
    //@ slevel 50; assigns v; ensures \true;  // Also test the pretty-printer
    if (i%2) {
      v = 1;
    } else {
      v = -1;
    }
    Frama_C_show_each(v, i, r);
    v = v * v;
    r = r + 2 / (v+1);
    //@ slevel default;
    ;
  }
}

void g() {// Do not crash when loop unrolling clears the dependencies of the AST
  //@ loop pragma UNROLL 1;
  for (int i=0; i<5; i++) {
  }
}
