/* run.config 
OPT: -load-module lib/plugins/Report -no-val-show-progress -val -val-use-spec g1,g2,g3 -then -report
*/

/* Test what is printed when Value evaluates a post-condition:
   - function with a body (h below): emit a status, plus message "postcondition
     got status..."
   - function with only a spec: do not emit a status (respect the Kernel's
     green/blue statuses) + emit a message only in case of invalid: messages
     for Valid/Unkown are not informative
   - function with a body, but for which we evaluate the spec (val-use-spec,
     g below): emit the status (otherwise the verification is incomplete),
     emit message only in the invalid case.
   - in the last two cases, do nothing in the case 'ensures \false' (the
     warning is too distracting otherwise)
*/


extern int i;

//@ ensures 0 == 1;
void f1();

/*@ behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void f2();

/*@ ensures i == 4; */
void f3();

/*@ ensures \false; */
void f4();

/*@ ensures 0 == 1; */
void g1() {
}

/*@ behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void g2() {
}

/*@ ensures i == 4; */
void g3() {
}

/*@ ensures 0 == 1; */
void h1() {
}

/*@ behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void h2() {
}

/*@ ensures i == 4; */
void h3() {
}

/*@ ensures \false; */
void h4() {
}


//@ ensures \result == p;
int* k(int *p);


void main(j) {
  if (j & 1) {
    f1();
  }
  if (j & 2) {
    f2();
  }
  if (j & 3) {
    f3();
  }
  if (j & 4) {
    f4();
  }
  if (j & 5) {
    g1();
  }
  if (j & 6) {
    g2();
  }
  if (j & 7) {
    g3();
  }
  if (j & 8) {
    h1();
  }
  if (j & 9) {
    h2();
  }
  if (j & 10) {
    h3();
  }
  if (j & 11) {
    h4();
  }
  if (j & 12) {
    int v;
    k(&v);
  }

}
