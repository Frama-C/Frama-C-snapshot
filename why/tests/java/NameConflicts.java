
class NameConflicts {

    int i;
    
    void setI(int i) {
	this.i = i;
    }

    /*@ behavior normal:
      @   ensures \result == 0;
      @*/
    int m() {
	int result = 0;
	return 0;
    }

    int field;

    int field() { return field; }

}

