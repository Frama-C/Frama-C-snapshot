

/* McCarthy's ``91'' function. */

//@+ CheckArithOverflow = no


public class MacCarthy {

    /*@ decreases \int_max(0,101-n) ;
      @ behavior less_than_101:
      @   assumes n <= 100;
      @   ensures \result == 91;
      @ behavior greater_than_100:
      @   assumes n >= 101;
      @   ensures \result == n - 10;
      @*/
    public static int f91(int n) {
	if (n <= 100) {
	    return f91(f91(n + 11));
	}
	else
	    return n - 10;
    }

}