/* run.config
   STDOPT: +"-then" +"-report"
*/

int x;
//@ requires ReqTrue: \true; ensures PostTrue: \true; // Test printing of ACSL names
void f(unsigned int c, unsigned int d) {
  //@ assert A1: c != 1 || d != 2;
  //@ assert A2: d/2-c !=0;
  x = 1/(d/2-c);
}

/*@ requires c + 1 == 2; 
  @ requires c+d==3; */
//implied: requires c==1 && d==2;
void main(unsigned int c, unsigned int d) {
  if (c == 1 && d==2)
    f(c, d);
  /*@ assert c==1 && d==2; */
  f(d,c);
}
