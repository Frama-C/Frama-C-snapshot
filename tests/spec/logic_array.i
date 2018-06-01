/*@ axiomatic Array_unsigned {
  logic integer size10 = 10;
  type  T_array10_unsigned = unsigned [size10];
  logic T_array10_unsigned C_tab10 reads \nothing;
  predicate check(T_array10_unsigned tab, integer idx, unsigned v) = (tab[idx] == v) ;
  logic T_array10_unsigned modified(T_array10_unsigned tab, integer idx, unsigned v) = { tab \with [idx] = v };
  lemma check_modified: \forall T_array10_unsigned tab, integer idx, unsigned v;
        check(modified(tab,idx,v),idx,v);
}
*/

int A[];

int B[2] = { 42, 43 };

/*@ logic int f(int x[], integer idx) = x[idx]; */

/*@ lemma foo{L}: f(B,0) == 42; */

/*@ lemma bar{L}: f(A,0) == 44; */

/*@ lemma bli{L}: A != B; */

int A[] = { 44, 45, 46 };
