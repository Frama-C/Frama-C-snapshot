class Literals {

    public static final int x = 0xbad;

    /*@ ensures \result == 0xd;
      @*/
    int f() {
	int x = 0xbad;
	return 015;
    }

}
