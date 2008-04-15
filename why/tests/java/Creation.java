
class Creation {

    int simple_val;

    /*@ behavior normal:
      @   assigns this.simple_val; // not \nothing 
      @   ensures this.simple_val == 0;
      @*/
    Creation() {
	this(0);
    }

    /*@ behavior normal:
      @   assigns this.simple_val; // not \nothing
      @   ensures this.simple_val == n;
      @*/
    Creation(int n) {
	simple_val = n;
    }

    /*@ behavior normal:
      @   assigns this.simple_val; // not \nothing
      @   ensures this.simple_val == n + m;
      @*/
    Creation(int n,int m) {
	this(n+m); 
    }

    /*@ behavior normal:
      @   ensures \result == 17;
      @*/
    public static int test1() {
	Creation t = new Creation(17);
	return t.simple_val;
    }

    /*@ behavior normal:
      @   ensures \result == 0;
      @*/
    public static int test2() {
	Creation t = new Creation();
	return t.simple_val;
    }

    /*@ behavior normal:
      @   assigns \nothing;             // BUG !!!!!!!!!!!!
      @   ensures \result == 17;
      @*/
    public static int test3() {
	Creation t = new Creation(10,7);
	return t.simple_val;
    }

}


class TestSuperConstructor extends Creation {

    /*@ behavior normal:
      @   assigns simple_val;
      @   ensures simple_val == 12;
      @*/
    TestSuperConstructor() {
	super(12);
    }

}


