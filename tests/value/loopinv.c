/* run.config* 
OPT: @VALUECONFIG@ -no-autoload-plugins -load-module from,inout,value,report -slevel-function main2:20 -pp-annot -val -then -report
*/

/*@ requires valid: \valid(&t[0..s-1]);
    requires c: 1 <= c < s; */
void init (int *t, int c, int s) {
  int* p = t;
  /*@ loop invariant \valid(p) && p < &t[s-1]; */
  while(1) {
    *(++p) = 1;
    if(p >= t+c) break;
  }
}


void main1 (int c) {
  int t1[72];
  int t2[11];

  if (c >= 1 && c < 72) {
    init(t1, c, 72);

    if (c < 8)
      init(t2, c, 11);
  }
}

void main2() {
  int i = 0;
  int j = 0;
  /*@ loop invariant i < 10;
    loop invariant i == j; */
  while (1) {
    i++;
    j++;
  }
}

void main3() {  // Widening is completely inactivated on this example
  int j = 0;
  //@ loop invariant i == 2*j || i == 2*j+1;
  for (int i=0; i<100; i++) {
    if (i%2==1)
      j++;
    Frama_C_show_each(i,j);
  }
}

/* The result of the widening should be reduced by the loop invariant,
   but the loop invariant must have an unknown status if it still does not hold
   in the reduced state —here due to the missing backward propagation on the
   multiplication. Change the invariant for a more complicated one when this
   propagator is implemented. */
void main4 () {
  int a = 9;
  int x = 0;
 /*@ loop invariant x<10 && x*x<10; */
  while(x < a) x++;
}

void main(int c) {
  main1(c);
  if (c) main2();
  main3();
  main4();
}
