
int * p = (int *) 0x0033FF ;
int * q = (int *) 0x0066F0 ;
int * r = (int *) 0x0066F8 ;

/*@ 
  requires p_is_33FF: p == (int *) 0x0033FF ;
  requires q_is_66F0: q == (int *) 0x0066F0 ;
  requires r_is_q1_ko: (q+1) == (int *) 0x0066F8 ; // DON'T WANT TO PROVE THIS !
*/
void main(void) { return; }
