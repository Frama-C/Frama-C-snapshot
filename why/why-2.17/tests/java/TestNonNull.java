
//@+ NonNullByDefault = all


class TestNonNull {
    
    static final int N = 2;

    static int[] st;
    //@ static invariant st_length: st.length >= 4;

    int[] t;



    TestNonNull() {
	t = new int[3];
	//@ assert t.length == 3;
    }


    //@ requires t.length >= 4;
    void test(int[] t) {
	int i = t[3];
	t[2] = 1;
	int j = t[N];
	t[N + 1] = 1;
	st[3] == 1;
    }

}
