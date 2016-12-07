/* run.config
   OPT:
*/

/* run.config_qualif
   OPT: -wp -wp-par 1
*/

/*@ predicate p_pointer{L1,L2}(int *a,int i,int j) =
   \at(a[i],L1) == \at(a[j],L2) ;
*/

// ARRAY PASSED BY VALUES
/*@ predicate p_arrays(int a[10],int i,int b[10],int j) =
    a[i] == b[j] ;
*/

// USELESS LABELS and USELESS \AT
/*@ predicate p_dummy{L1,L2}(int a[10],int i,int j) =
   \at(a[i],L1) == \at(a[j],L2) ;
*/

int arr[10];

/*@ 
  requires 0 <= i < 10;
  requires 0 <= j < 10;
  requires 0 <= k < 10;
  ensures PTR: p_pointer{Pre,Here}((int *)arr,i,j); 
  ensures ARR: p_arrays(\old(arr),i,arr,j);
  ensures DUM: p_dummy{Pre,Here}(arr,j,k);
 */
void job(i,j,k) {
  int tmp = arr[i];
  arr[i] = arr[j] ;
  arr[j] = tmp ;
  arr[k] = tmp ;
}
