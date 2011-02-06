int G;
/*@ behavior b2: 
      assumes c < 1 ; */
void f (int c);

/*@ behavior b1: 
      assumes c > 0 ;
    complete behaviors b1, b2; */
void f (int c) {
    /*@ for b1: assert \true; */
  }

/*@ requires \true;
    assigns G ;
    ensures \result == 0 || \result == 1;
  @ behavior no :
      assumes c < 1 ; 
      assigns \nothing ;
      ensures \result == 0 ;
  @ behavior at_least_one :
      assumes c > 0 ; 
      assigns G ;
      ensures \result == 1 ;
  @ complete behaviors ;
  @ disjoint behaviors ;
  @*/
int main(int c) { f(c) ; return c> 0; }
