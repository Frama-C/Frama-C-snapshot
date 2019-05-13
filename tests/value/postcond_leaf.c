/* run.config* 
OPT: -no-autoload-plugins @EVA_CONFIG@ -load-module eva,inout,report -eva-no-show-progress -eva -eva-use-spec g1,g2,g3 -then -report
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
void f1(void);

/*@
  assigns i \from i;
  behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void f2(void);

/*@
 assigns i \from i;
 ensures i == 4;
*/
void f3(void);

/*@
  assigns \nothing;
  ensures \false;
*/
void f4(void);

/*@
  assigns i \from \nothing;
  ensures 0 == 1;
*/
void g1() {
}

/*@
  assigns i \from i;
  behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void g2() {
}

/*@
  assigns \nothing;
  ensures i == 4;
*/
void g3() {
}

/*@
  assigns \nothing;
  ensures 0 == 1;
*/
void h1() {
}

/*@
  assigns \nothing;
  behavior b:
      assumes i == 1;
      ensures 0 == 1;
    complete behaviors b; */
void h2() {
}

/*@
  assigns \nothing;
  ensures i == 4;
*/
void h3() {
}

/*@
  assigns \nothing;
  ensures \false;
*/
void h4() {
}


/*@
  assigns *p \from i;
  assigns \result \from p;
  ensures \result == p;
*/
int* k(int *p);


volatile int j;
void main() {
  if (j) {
    f1();
  }
  if (j) {
    f2();
  }
  if (j) {
    f3();
  }
  if (j) {
    f4();
  }
  if (j) {
    g1();
  }
  if (j) {
    g2();
  }
  if (j) {
    g3();
  }
  if (j) {
    h1();
  }
  if (j) {
    h2();
  }
  if (j) {
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

