/* run.config
   COMMENT: linear search (example from the SAC'13 article)
*/

int A[10];

/*@ requires \forall integer i; 0 <= i < 9 ==> A[i] <= A[i+1]; 
    behavior exists:
      assumes \exists integer j; 0 <= j < 10 && A[j] == elt;
      ensures \result == 1;
    behavior not_exists:
      assumes \forall integer j; 0 <= j < 10 ==> A[j] != elt;
      ensures \result == 0; */
int search(int elt){
  int k;
  // linear search in a sorted array
  /*@ loop invariant 0 <= k <= 10; 
    @ loop invariant \forall integer i; 0 <= i < k ==> A[i] < elt; */
  for(k = 0; k < 10; k++)
    if(A[k] == elt) return 1; // element found
    else if(A[k] > elt) return 0; // element not found (sorted array)
  return 0; // element not found
} 

int main(void) {

  int found;
  for(int i = 0; i < 10; i++) A[i] = i * i;

  found = search(36);
  /*@ assert found == 1; */

  found = search(5);
  /*@ assert found == 0; */

  return 0;
}
