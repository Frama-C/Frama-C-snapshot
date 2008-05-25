#pragma SeparationPolicy(Regions)

//@ logic integer num_of_pos{Here}(integer i,integer j,int t[]) reads t[i];

/*@ axiom num_of_pos_empty{Here} :
  @   \forall integer i, integer j, int t[];
  @       i > j ==> num_of_pos(i,j,t) == 0;
  @*/
 
/*@ axiom num_of_pos_true_case{Here} :
  @   \forall integer i, integer j, int t[];
  @       i <= j && t[j] > 0 ==> 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t) + 1;
  @*/

/*@ axiom num_of_pos_false_case{Here} :
  @   \forall integer i, integer j, int t[];
  @       i <= j && ! t[j] > 0 ==> 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t);
  @*/

/*@ axiom num_of_pos_strictly_increasing{Here}:
  @   \forall integer i, integer j, integer k, integer l, int t[];
  @       j < k && k <= l && t[k] > 0 ==> num_of_pos(i,j,t) < num_of_pos(i,l,t);
  @*/

/*@ requires length >= 0 && \valid_range(t,0,length-1);
  @*/
void m(int t[], int length) {
  int count = 0;
  int i;
  int *u;

  /*@ loop invariant
    @    0 <= i && i <= length && 
    @    0 <= count && count <= i && 
    @    count == num_of_pos(0,i-1,t)  ;
    @ loop variant length - i;
    @*/
  for (i=0 ; i < length; i++) {
    if (t[i] > 0) count++;
  }
  u = (int *)calloc(count,sizeof(int));
  count = 0;
  
  /*@ loop invariant
    @    0 <= i && i <= length && 
    @    0 <= count && count <= i && 
    @    count == num_of_pos(0,i-1,t);
    @ loop variant length - i;
    @*/
  for (i=0 ; i < length; i++) {
    if (t[i] > 0) {
      u[count++] = t[i];
    }
  }
}


/* 
Local Variables:
compile-command: "LC_ALL=C make muller"
End:
*/
