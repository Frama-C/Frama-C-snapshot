/*@
   predicate is_valid_int_range(int* p, int n) =
           (0 <= n) && \valid_range(p,0,n-1);

   lemma foo: \forall int* p,n; is_valid_int_range(p,n) <==> \valid_range(p,0,n-1);

   
*/


/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);

   assigns \nothing;

   behavior all_equal:
     assumes \forall int i; 0 <= i < n ==> a[i] == b[i];
     ensures \result == 1;

   behavior some_not_equal:
     assumes \exists int i; 0 <= i < n && a[i] != b[i];
     ensures \result == 0;

   complete behaviors all_equal, some_not_equal;
   disjoint behaviors all_equal, some_not_equal;
*/
int equal(const int* a, int n, const int* b)
{
  /*@
     loop invariant 0 <= i <= n;
     loop invariant \forall int k; 0 <= k < i ==> a[k] == b[k];
     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     if (a[i] != b[i])
       return 0;

  return 1;
}

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior some:
     assumes \exists int i; 0 <= i < n && a[i] == val;
     ensures 0 <= \result < n;
     ensures a[\result] == val;
     ensures \forall int i; 0 <= i < \result ==> a[i] != val;

   behavior none:
     assumes \forall int i; 0 <= i < n ==> a[i] != val;
     ensures \result == n;

   complete behaviors some, none;
   disjoint behaviors some, none;
*/
int find(const int* a, int n, int val)
{
  /*@
    loop invariant 0 <= i <= n;
    loop invariant \forall int k; 0 <= k < i ==> a[k] != val;
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++)
    if (a[i] == val)
      return i;

  return n;
}

/*@
   predicate
     found{A}(int* a, int n, int val) =
       \exists int i; 0 <= i < n && a[i] == val;
*/
/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior some:
     assumes found(a, n, val);
     ensures 0 <= \result < n;
     ensures a[\result] == val;
     ensures !found(a, \result, val);

   behavior none:
     assumes !found(a, n, val);
     ensures \result == n;

   complete behaviors some, none;
   disjoint behaviors some, none;
*/
int find2(const int* a, int n, int val)
{
  /*@
    loop invariant 0 <= i <= n;
    loop invariant !found(a, i, val);
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++)
    if (a[i] == val)
      return i;

  return n;
}

/*@
   predicate
     found_first_of{A}(int* a, int m, int* b, int n) =
       \exists int i; 0 <= i < m && found{A}(b, n, \at(a[i],A));
*/
/*@
   requires is_valid_int_range(a, m);
   requires is_valid_int_range(b, n);

   assigns \nothing;

   behavior found:
     assumes found_first_of(a, m, b, n);
     ensures 0 <= \result < m;
     ensures found(b, n, a[\result]);
     ensures !found_first_of(a, \result, b, n);

   behavior not_found:
     assumes !found_first_of(a, m, b, n);
     ensures \result == m;

   complete behaviors found, not_found;
   disjoint behaviors found, not_found;
*/
int find_first_of(const int* a, int m, const int* b, int n)
{
  /*@
     loop invariant 0 <= i <= m;
     loop invariant !found_first_of(a, i, b, n);
     loop   variant m-i;
  */
  for(int i = 0; i < m; i++)
     if (find(b, n, a[i]) < n)
          return i;

  return m;
}

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior empty:
     assumes n == 0;
     ensures \result == 0;

   behavior not_empty:
     assumes 0 < n;
     ensures 0 <= \result < n;
     ensures \forall int i; 0 <= i < n       ==> a[i] <= a[\result];
     ensures \forall int i; 0 <= i < \result ==> a[i] < a[\result];

   complete behaviors empty, not_empty;
   disjoint behaviors empty, not_empty;
*/
int max_element(const int* a, int n)
{
  if (n == 0) return 0;
  int max = 0;
  /*@
     loop invariant 0 <= i <= n;
     loop invariant 0 <= max < n;
     loop invariant \forall int k; 0 <= k < i   ==> a[k] <= a[max];
     loop invariant \forall int k; 0 <= k < max ==> a[k] < a[max];
     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     if (a[max] < a[i])
       max = i;

  return max;
}


/*@
   requires n > 0;
   requires \valid(p+ (0..n-1));

   assigns \nothing;

   ensures \forall int i; 0 <= i <= n-1 ==> \result >= p[i];
   ensures \exists int e; 0 <= e <= n-1 && \result == p[e];
*/
int max_seq(const int* p, int n)
{
  return p[max_element(p, n)];
}

/*@
   axiomatic counting_axioms
   {
      logic integer counting{L}(int* a, integer n, int val)
        reads a[0..n-1];

      axiom counting_empty{L}:
        \forall int* a, integer n, int val; n <= 0 ==>
           counting(a, n, val) == 0;

      axiom counting_hit{L}:
        \forall int* a, integer n, int val; n >= 0 && a[n] == val ==>
           counting(a, n+1, val) == counting(a, n, val) + 1;

      axiom counting_miss{L}:
        \forall int* a, integer n, int val; n >= 0 && a[n] != val ==>
           counting(a, n+1, val) == counting(a, n, val);
    }
*/

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   ensures \result == counting(a, n, val);
*/
int count(const int* a, int n, int val)
{
  int cnt = 0;
  /*@
     loop invariant 0 <= i <= n;
     loop invariant 0 <= cnt <= i;
     loop invariant cnt == counting(a, i, val);
     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     if (a[i] == val)
       cnt++;

  return cnt;
}

/*@
   requires \valid(p);
   requires \valid(q);

   assigns *p;
   assigns *q;

   ensures *p == \old(*q);
   ensures *q == \old(*p);
*/
void swap(int* p, int* q)
{
  int const save = *p;
  *p = *q;
  *q = save;
}

/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);
   //requires \separated(a, b);

   assigns a[0..n-1];
   assigns b[0..n-1];

   ensures \forall int k; 0 <= k < n ==> a[k] == \old(b[k]);
   ensures \forall int k; 0 <= k < n ==> b[k] == \old(a[k]);
*/
void swap_ranges(int* a, int n, int* b)
{
  /*@
     loop assigns a[0..i-1];
     loop assigns b[0..i-1];

     loop invariant 0 <= i <= n;
     loop invariant \forall int k; 0 <= k < i ==>
                     a[k] == \at(b[k],Pre);
     loop invariant \forall int k; 0 <= k < i ==>
                     b[k] == \at(a[k],Pre);

     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     swap(&a[i], &b[i]);
}

/*@
   requires is_valid_int_range(a, n);

   assigns a[0..n-1];

   ensures \forall int i; 0 <= i < n ==> a[i] == val;
*/
void fill(int* a, int n, int val)
{
  /*@
  loop invariant 0 <= i <= n;
  loop invariant \forall int k; 0 <= k < i ==> a[k] == val;
  loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     a[i] = val;
}

/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);

   assigns b[0..n-1];

   ensures \forall int i; 0 <= i < n ==> b[i] == a[i];
*/
void copy(const int* a, int n, int* b)
{
  /*@
     loop assigns b[0..i-1];
     loop invariant 0 <= i <= n;
     loop invariant \forall int k; 0 <= k < i ==> a[k] == b[k];
     loop   variant n-i;
  */
  for (int i = 0; i < n; ++i)
     b[i] = a[i];
}

/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);

   assigns b[0 .. n-1];

   ensures \forall int j; 0 <= j < n ==>
            a[j] == old_val && b[j] == new_val ||
            a[j] != old_val && b[j] == a[j];
   ensures \result == n;
*/
int replace_copy(const int* a, int n, int* b, int old_val, int
    new_val)
{
  /*@
     loop assigns b[0..i-1];
     loop invariant 0 <= i <= n;
     loop invariant \forall int j; 0 <= j < i ==>
                     a[j] == old_val && b[j] == new_val ||
                     a[j] != old_val && b[j] == a[j];
    loop   variant n-i;
   */
  for (int i = 0; i < n; ++i)
     b[i] = (a[i] == old_val ? new_val : a[i]);

  return n;
}

/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);

   assigns b[0 .. n-1];

   ensures \forall int k; \result <= k < n ==> b[k] == \old(b[k]);
   ensures \forall int k; 0 <= k < \result ==> b[k] != val;
   ensures \forall int x; x != val ==>
            counting(a, n, x) == counting(b, \result, x);
   ensures \result == n - counting(a, n, val);
   ensures 0 <= \result <= n;
*/
int remove_copy(const int* a, int n, int* b, int val)
{
  int j = 0;
  /*@
     loop assigns b[0..j-1];

     loop invariant 0 <= j <= i <= n;
     loop invariant \forall int k; j <= k < n ==>
                      b[k] == \at(b[k],Pre);
     loop invariant \forall int k; 0 <= k < j ==> b[k] != val;
     loop invariant \forall int x; x != val ==>
                      counting(a,i,x) == counting(b,j,x);
     loop invariant j == i - counting(a,i,val);

     loop   variant n-i;
   */
  for (int i = 0; i < n; ++i)
     if (a[i] != val)
       b[j++] = a[i];
  return j;
}

/*@
   requires is_valid_int_range(a, n);
   requires val + n < ((1<<31)-1); // INT_MAX;

   assigns a[0..n-1];

   ensures \forall int k; 0 <= k < n ==> a[k] == val + k;
*/
void iota(int* a, int n, int val)
{
  /*@
     loop assigns a[0..i-1];
     loop invariant 0 <= i <= n;
     loop invariant \forall int k; 0 <= k < i ==> a[k] == val+k;
     loop   variant n-i;
  */
  for(int i = 0; i < n; ++i)
     a[i] = val + i;
}

/*@
   predicate
     adjacent_found{Label}(int* a, int n) =
       \exists int i; 0 <= i < n-1 && a[i] == a[i+1];
*/

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior some:
     assumes adjacent_found(a, n);
     ensures 0 <= \result < n-1;
     ensures a[\result] == a[\result+1];
     ensures !adjacent_found(a, \result);

   behavior none:
     assumes !adjacent_found(a, n);
     ensures \result == n;

   complete behaviors some, none;
   disjoint behaviors some, none;
*/
int adjacent_find(int* a, int n)
{
  if (0 == n) return n;

  /*@
     loop invariant 0 <= i < n;
     loop invariant !adjacent_found(a, i);
     loop invariant 0 < i ==> a[i-1] != a[i];
     loop   variant n-i;
  */
  for (int i = 0; i < n-1; i++)
     if (a[i] == a[i+1])
       return i;

  return n;
}

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior empty:
     assumes n == 0;
     ensures \result == 0;

   behavior not_empty:
     assumes 0 < n;
     ensures 0 <= \result < n;
     ensures \forall int i; 0 <= i < n       ==> a[\result] <= a[i];
     ensures \forall int i; 0 <= i < \result ==> a[\result] < a[i];
*/
int min_element(int* a, int n)
{
  if (0 == n) return n;

  int min = 0;
  /*@
     loop invariant 0 <= i   <= n;
     loop invariant 0 <= min <  n;
     loop invariant \forall int k; 0 <= k < i ==> a[min] <= a[k];
     loop invariant \forall int k; 0 <= k < min ==> a[min] < a[k];
     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     if (a[i] < a[min])
       min = i;

  return min;
}
