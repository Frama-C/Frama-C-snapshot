/* FRAMAC_SHARE=share  bin/viewer.opt -pp-annot tests/wp/quicksort.c */

/*external permut_ij : ptr -> ptr -> int -> int -> Prop : Quicksort ;
external permut : ptr -> ptr -> Prop : Quicksort ;
external high_bound : ptr -> int -> int -> int -> Prop : Quicksort ;
external low_bound : ptr -> int -> int -> int -> Prop : Quicksort ;
*/

#define SIZE 100
int T[SIZE];

/*@ requires (0 <= i < SIZE) && (0 <= j < SIZE);
    ensures T[i] == \old(T[j]) && T[j] == \old(T[i]);
    assigns T[i], T[j];
 */
void swap (int i, int j) {
  int v;
  v = T[i];
  T[i] = T[j];
  T[j] = v;
}

/*@
requires (0 <= l < i) && (i < SIZE)
     && (\forall int k; l+1 <= k <= i-1 ==> T[k] <= T[l]);
ensures i-1 <= \result <= i 
     && (\forall int k; l <= k <= \result ==> T[k] <= T[\result])
     && (\forall int res; res == \result ==> 
                          T[l] == \old(T[res]) && T[res] == \old(T[l]))
     && T[\result] <= T[i];
*/
int mv_pv (int l, int i) { 
  int res;
  if (T[i] < T[l]) { 
    swap(l, i);
    res =  i;
    }
  else { 
    swap(l, i - 1); 
    res = i - 1;  
    } 
  return res;
}
      

/*
Pre : (0 <= l < r) && r < length(T);
Modifies : T;
Post : 
  (l <= result && result <= r) &&
  high_bound (T, l, result,  T[result]) &&
  low_bound (T, result, r, T[result]) &&
  permut (T, T@0) &&
  (forall k:int. (k < l \/ k > r) => T[k] = T@0[k]);
*/
int partition (int l, int r)
{
  int pv, i, j, res;
  pv = T[l];
  i = l+1;
  j = r;

  while (i < j)
    /*
    Inv: (l+1 <= i <= r) && j <= r && i <= j+1 && permut (T, T@0) 
       && high_bound (T, l+1, i-1, pv) && (low_bound (T, j+1, r, pv))
       && (forall k:int. (k <= l \/ k > r) => T[k] = T@0[k]);
    Modifies : i, j, T;
    */
    {
       while (T[i] <= pv && i < j)
	 /*
      	 Inv: l+1 <= i <= r && high_bound (T, l+1, i-1, pv) && i <= j+1;
         Modifies : i;
	 */
         { i = i + 1; }

       while (T[j] >= pv && i < j) 
	 /*
         Inv: j <= r && low_bound (T, j+1, r, pv) 
           && ~(T[i] <= pv && i < j) && i <= j+1;
         Modifies : j;
	 */
	 { j = j - 1; }

       if (i < j) 
         {
      	   swap( i, j);
	   i = i + 1;
	   j = j - 1;
	 }
    } 

  res = mv_pv (l, i);

  return res;
}


/*
Pre:  0 <= l && r < length(T);
Modifies: T;

Post: 
      (forall i j:int. l <= i <= j <= r => T[i] <= T[j])  &&
      (forall k:int. (k < l \/ k > r) => T[k] = T@0[k]) &&
      permut (T, T@0) ;
      */

void quick_rec (int l, int r) 
{
   int p;
   if (l < r) 
     {
       p = partition(l, r);
       quick_rec(l, p-1);
       quick_rec(p+1, r);
     }
}

/*
void sort (int n) 
Pre: n = length (T);
Modifies: T;
{
  quick_rec (0, n-1);
}
Post : (forall i j:int.  (0 <= i <= j < n) => T[i] <= T[j]) &&
        permut (T, T@0) ;
*/
