// RUNCOQ: will ask regtests to check Coq proofs of this program

// int model: unbounded mathematical integers
//@+ CheckArithOverflow = no

/*@ inductive isfib(integer x, integer r) {
  @  case isfib0: isfib(0,0);
  @  case isfib1: isfib(1,1);
  @  case isfibn: 
  @   \forall integer n r p; 
  @     n >= 2 && isfib(n-2,r) && isfib(n-1,p) ==> isfib(n,p+r);
  @ }
  @*/ 

//@ lemma isfib_2_1 : isfib(2,1);
//@ lemma isfib_6_8 : isfib(6,8);

// provable only if def is inductive (least fix point)
//@ lemma not_isfib_2_2 : ! isfib(2,2);

public class Fibonacci {

    /*@ requires n >= 0;
      @ ensures isfib(n, \result); 
      @*/
    public static long Fib(int n) {
	long y=0, x=1, aux;

	/*@ loop_invariant 0 <= i <= n && isfib(i+1,x) && isfib(i,y);
	  @ loop_variant n-i;
	  @*/
	for(int i=0; i < n; i++) {
	    aux = y;
	    y = x;
	    x = x + aux;
	}
	return y;
    }
}

