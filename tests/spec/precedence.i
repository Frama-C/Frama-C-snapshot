int x[10] ;
//@lemma prio_unary_plus:  3 - +2       +2     == 3;
//@lemma prio_unary_minus: 3 - -2       -2     == 3;
//@lemma prio_unary_amp:  (&x[1] - &x[0] & &x[2]  - &x[2]) == 0;
//@lemma prio_unary_star:  0 * *&x[2] * *&x[2] == 0;

//@ lemma prio_ternary_let: \let i = 0; i == 0 ? i : i - 1;
//@ lemma prio_ternary_let_2: \let i = 0; (\let i = 0; i==3) ? i!=0: i==0;
