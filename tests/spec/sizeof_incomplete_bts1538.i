/* run.config
STDOPT: +"-kernel-msg-key printer:logic-types"
*/
int t[];

void main() {
  int v = sizeof(t);
  //@ assert v == sizeof(int [10]); // OK
  //@ assert sizeof(t) == 0; // VALID (Value/WP) but incorrect
  //@ assert sizeof(t) == sizeof(int [10]); // INVALID
}

int t[10];
