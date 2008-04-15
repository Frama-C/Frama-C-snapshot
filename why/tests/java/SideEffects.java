
//@+ CheckArithOverflow = no

class SideEffects {

    /*@ requires t.length > 10;
      @*/
    void m1(int t[]) {
	int i = 0;
	t[i++] = 1;
	//@ assert t[0] == 1 && i == 1;    
	t[++i] = 2;
	//@ assert t[0] == 1 && t[2] == 2 && i == 2;    
	t[--i] = 3;
	//@ assert t[0] == 1 && t[2] == 2 && t[1] == 3 && i == 1;    
	t[i--] = 4;
	//@ assert t[0] == 1 && t[2] == 2 && t[1] == 4 && i == 0;    
    }

}

