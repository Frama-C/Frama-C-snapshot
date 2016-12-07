/* run.config*
   STDOPT: +"-inout"
*/

int t[10];

extern int j;

//@ requires 0 <= j < 10;
void main() {
  //@ assert \forall int i; 0 <= i <= 9 ==> t[i] == 0;
  //@ assert \forall integer i; 0 <= i <= 9 ==> t[i] == 0;
  //@ assert !(\exists int i; 0 <= i <= 9 && t[i] == 1);
  t[1] = 2;
  //@ assert !(\forall int i; 0 <= i <= 9 ==> t[i] == 0);
  //@ assert \exists int i; 0 <= i <= 9 && t[i] == 2;
}
