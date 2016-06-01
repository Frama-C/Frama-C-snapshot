/* run.config_qualif
   OPT: -wp -wp-proof alt-ergo -wp-depth 16
   OPT: -wp -wp-proof why3:alt-ergo -wp-depth 16
*/

/*@ 
  axiomatic Nth {
  logic integer f(integer a);

  lemma access_16_16: ok: 
  \forall integer k ; 0 <= k < 16 ==>
    f(k)==\nth([| f(0), f(1), f(2),  f(3),  f(4),  f(5),  f(6),  f(7), 
                  f(8), f(9), f(10), f(11), f(12), f(13), f(14), f(15) |], k);

  lemma access_4_4: ok: 
  \forall integer k ; 0 <= k < 4 ==>
    f(k)==\nth([| f(0), f(1), f(2),  f(3) |], k);
              
  }
*/

