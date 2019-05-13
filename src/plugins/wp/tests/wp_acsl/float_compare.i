/*@ lemma test_float_compare:
      \forall float x,y;
      \is_finite(x) && \is_finite(y) ==>
      \le_float(x,y) ==> \lt_float(x,y) || \eq_float(x,y);
*/

/*@ lemma test_double_compare:
      \forall double x,y;
      \is_finite(x) && \is_finite(y) ==> \le_double(x,y) ==>
         \lt_double(x,y) || \eq_double(x,y);
*/

/*@ lemma test_float_compare_greater:
      \forall float x,y;
      \is_finite(x) && \is_finite(y) ==>
      \ge_float(x,y) ==> \gt_float(x,y) || \eq_float(x,y);
*/

/*@ lemma test_double_compare_greater:
      \forall double x,y;
      \is_finite(x) && \is_finite(y) ==> \ge_double(x,y) ==>
         \gt_double(x,y) || \eq_double(x,y);
*/
