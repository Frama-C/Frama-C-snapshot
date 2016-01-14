/* run.config
   OPT: tests/wp_plugin/init_linker.i
 */

/* run.config_qualif
   OPT: tests/wp_plugin/init_linker.i
 */

// To be linked with init_linker that defines the initial value of 'a'

extern int a ;
extern int b ;

/*@ 
  ensures OK: \at( a , Init ) == 2 ;
  ensures KO: \at( a , Init ) == 0 ;
  ensures KO: \at( b , Init ) == 0 ;
*/
void f(void) { return; }
