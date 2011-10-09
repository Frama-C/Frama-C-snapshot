typedef unsigned int TAB120[ 120 ] ;

extern volatile TAB120 volatile_tab_120_2[ 2 ];

volatile unsigned int* const p_first_volatile = &volatile_tab_120_2[0][0] ;

struct foo { int x; };

volatile struct foo f = { 1 };

volatile int* x = &f.x;

/*@ requires p_first_volatile == &volatile_tab_120_2[0][0] ;
  requires x == &f.x;
 */
int main(void) {
  p_first_volatile = &volatile_tab_120_2[1][112] ;
  return 0;
}
