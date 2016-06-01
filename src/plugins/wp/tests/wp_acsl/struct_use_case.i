/* run.config
   OPT: -wp-model Caveat
*/

/* run.config_qualif
OPT: -journal-disable -wp -wp-model Caveat -wp-par 1 -wp-prop="-ko"
OPT: -journal-disable -wp -wp-model Caveat -wp-par 1 -wp-prop ko -wp-timeout 2
*/

/* run.config_qed
DONTRUN: (config_qed) see config_qualif
*/

struct {
    unsigned int a;
    unsigned int b[2];
} var;

/*@ ensures ko: var == { \old(var) \with .b[1] = x } ;
  @ ensures ok: var == { \old(var) \with .b[1] = x, .b[0] = y } ;
  @*/   
void f(unsigned int x, unsigned int y){
    var.b[0] = y;
    var.b[1] = x;
}

/*@ ensures ko: var == { \old(var) \with .b[1] = x } ;
  @ ensures ok: var == { \old(var) \with .b[1] = x, .a = y } ;
  @*/   
void g(unsigned int x, unsigned int y){
    var.a = y;
    var.b[1] = x;
}
