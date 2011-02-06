int a[100];

 /*@
 predicate my_sorted_array(int old_a[], integer start_index, integer end_index) =
   \forall integer k1, k2;
    start_index <= k1 <= k2 <= end_index ==> a[k1] <= a[k2];
*/
 

/*@
 predicate all_smaller_than_the_last (int old_a[], integer start_index, integer end_index) =
   \forall integer k1;
    start_index <= k1 < end_index ==> a[k1] <= a[end_index];
*/
 
 
 
//use of swap funktion causes ERROR
 
/*@
 requires 0 < length;
 requires \valid_range(a, 0, length-1);
 ensures my_sorted_array(a, 0, length-1);
*/
void bubble_sort(int* old_a, int length)
{
 
 
 int auf = 1;
 int ab;
 int fixed_auf = auf;
 
 /*@
  loop invariant fixed_auf == auf;
  loop invariant 0 < auf <= length;
  loop invariant all_smaller_than_the_last(a, 0, auf-1);
  loop invariant my_sorted_array(a, 0, auf-1);
  loop invariant \forall integer k; auf < k < length ==> a[k] == \at(a[k], Pre);
  loop assigns auf, fixed_auf, ab, a[0..auf];
 */
 for (; auf < length; auf++, fixed_auf = auf)  
 {  
  //@ assert my_sorted_array(a, 0, auf-1);    //IMPORTANT
  fixed_auf = auf;
  ab=auf;
  //@ assert my_sorted_array(a, ab, auf);
  /*@
   loop invariant fixed_auf == auf;
   loop invariant 0 <= ab <= auf;
   loop invariant all_smaller_than_the_last(a, 0, auf-1);
   loop invariant my_sorted_array(a, 0, ab-1);
   loop invariant my_sorted_array(a, ab, auf);
  
   loop invariant \forall integer k; auf < k < length ==> a[k] == \at(a[k], Pre);
  loop assigns ab, a[0..auf];
  */   
  while (0 < ab && a[ab] < a[ab-1])
  {   
    //@ assert my_sorted_array(a, 0, ab-1);   //IMPORTANT
    //@ assert my_sorted_array(a, ab, auf);   //IMPORTANT
   
   
    //@ assert a[ab] < a[ab-1];        //IMPORTANT
    //@ assert a[ab] <= a[auf];
     
     int temp = a[ab];
    a[ab] = a[ab-1];               
    a[ab-1] = temp;
   
   
    //@ assert a[ab-1] <= a[ab];        //IMPORTANT
    // not completely correct (actually  <), because only swapped when a[ab] < a[ab-1],
  
        
    //@ assert my_sorted_array(a, ab+1, auf); // OK
 
   //@ assert  a[ab] <= a[auf]; 
   //Problem: should be correct but is not proven
   //Solved: is proven due to predicate "all_smaller_than_the_last"
 
   //@ assert my_sorted_array(a, 0, ab-2); //ok //IMPORTANT
   
   //@ assert ab < auf ==> all_smaller_than_the_last(a, ab, ab+1);
   // NEEDS TO BE PROVEN   
      
   //@ assert  a[ab] <= a[auf];
   // NEEDS TO BE PROVEN  
   //@ assert my_sorted_array(a, ab, auf); // FAILURE
   
   // ==>   
   //@ assert my_sorted_array(a, ab-1, auf);  //IMPORTANT
 
   ab = ab-1;                
   //@ assert my_sorted_array(a, 0, ab-1);   //IMPORTANT 
   //@ assert my_sorted_array(a, ab, auf);   //IMPORTANT
  }
  //@ assert my_sorted_array(a, 0, auf);     //IMPORTANT
 }
}
 
