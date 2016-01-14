/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-model +ref
*/

/*@ predicate p_pointer{L1,L2}(int *a,int i,int j) =
   \at(a[i],L1) == \at(a[j],L2) ;
*/

// ARRAY PASSED BY VALUES !!
/*@ predicate p_arrays(int a[],int i,int b[],int j) =
    a[i] == b[j] ;
*/

// USELESS LABELS and USELESS \AT !!
/*@ predicate p_dummy{L1,L2}(int a[],int i,int j) =
   \at(a[i],L1) == \at(a[j],L2) ;
*/

int arr[10];

/*@ ensures qed_ok: PTR: todo: p_pointer{Pre,Here}((int *)arr,i,j); 
  @ ensures qed_ok: ARR: todo: p_arrays(\old(arr),i,arr,j);
  @ ensures qed_ok: DUM: todo: p_dummy{Pre,Here}(arr,j,k);
 */
void job(i,j,k) {
  int tmp = arr[i];
  arr[i] = arr[j] ;
  arr[j] = tmp ;
  arr[k] = tmp ;
}
