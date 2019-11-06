extern char A[20];

//@ predicate equal(integer x,integer y) = x==y ;

/*@
  ensures \forall integer i ; 0 <= i < n ==> \at( A[i] == *(p + i) , Pre);
 */
extern void Write(char *p, int n);


/*@
  ensures equal(A[0],1) ;
 */
void Job(void)
{
  char DataWrite;
  DataWrite = 1 ;
  Write((& DataWrite),1);
  return;
}
