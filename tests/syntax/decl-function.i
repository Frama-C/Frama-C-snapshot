/* run.config
COMMENT: test the use of (==) in Ast.is_def_or_last_decl
OPT: -then -print -no-unicode
 */

int x;
void (*pf)(void);

extern void f_undefined (void) ;

void g (void) {
  f_undefined() ;
  //@ assert fcs_limitation: pf==&f_undefined;
}

//@ logic integer foo (integer y) = y;

/*@ requires fcs_limitation: pf==&f_undefined;
  @ ensures  x == foo (x); */
extern void f_undefined (void) ;

void main (void) {
  pf=&f_undefined;
  f_undefined() ;
  f_undefined() ;
}
