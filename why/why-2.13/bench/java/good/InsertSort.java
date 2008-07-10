//@+ CheckArithOverflow = no

public class InsertSort {
	
	/*@ requires (t != null)&&(0<=i<t.length);
	@*/	
	public static void insertion (int[] t, int i){
		
		int j;
		j = i;
		//@ loop_invariant (0<=j<=i); decreases j;
		while (j > 0)
		    t[j--] = 0;
	}
		
}
