/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-proof z3 -wp-print -wp-verbose 2
*/

/* ---------------------------------------------------------------------------*/

#define INT2DOUBLE(E,S) \
{\
    register int i1;\
    register double f1;\
    i1=(E);\
    i1=(i1<<3)>>15;\
    f1=(double)i1;\
    (S)=f1;\
}

/*@ logic integer read_bit(integer x, integer i) = 
                          ((x & (1<<(i-1))) == 0) ? 0 : 1; */

/*@
behavior x_29_13_positive :
    assumes read_bit(x,29) == 0;
    ensures \result == \sum(1, 16, 
                            \lambda integer k; read_bit(x,12+k) * (1<<(k-1)));

behavior x_29_13_negative :
    assumes read_bit(x,29) == 1;
    ensures \result == 1 + \sum(1, 16, 
                       \lambda integer k; (1-read_bit(x,12+k)) * (1<<(k-1)));
*/
double INT2DOUBLE_wrapper(unsigned int x) {
    double y;
    INT2DOUBLE(x,y)
    return y;
}

/* ---------------------------------------------------------------------------*/


