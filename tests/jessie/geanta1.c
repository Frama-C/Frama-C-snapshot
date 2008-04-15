typedef struct {
	int elem1;
        int elem2;
 } EXEMPLE_STRUCTURE ;


/*@ requires \valid(p) && \valid(q) ;
  @ behavior p_changed:
  @   assumes n > 0;
  @   assigns *p;
  @   ensures (*p).elem1 == n;
  @  
  @ behavior q_changed:
  @   assumes n <= 0;
  @   assigns *q;
  @   ensures (*q).elem1 == n;
  @*/
void f(int n, EXEMPLE_STRUCTURE *p, EXEMPLE_STRUCTURE *q) {
  if (n > 0) (*p).elem1 = n; else (*q).elem1 = n;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make geanta1"
End:
*/
