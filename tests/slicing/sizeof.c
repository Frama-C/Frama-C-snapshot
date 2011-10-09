/* run.config
  OPT: -check -deps -slice-return main -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOf_1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOf_2 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt2 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt3 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt_deref_1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_tab_1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt_tab_1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_pt_tab_2 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-return SizeOfE_tab_acces_1 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-pragma main -journal-disable -then-on 'Slicing export' -print
  OPT: -check -deps -slice-assert main -journal-disable -then-on 'Slicing export' -print
*/
struct St { int i, *p, tab[5] ; } st ;

unsigned int SizeOf_1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(int *) ;
}
unsigned int SizeOf_2 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(struct St) ;
}
unsigned int SizeOfE_pt1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(&x) ;
}
unsigned int SizeOfE_pt2 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(p) ;
}
unsigned int SizeOfE_pt3 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(p+i) ;
}
unsigned int SizeOfE_pt_deref_1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(*(p+i)) ;
}
unsigned int SizeOfE_tab_1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(tab) ;
}
unsigned int SizeOfE_pt_tab_1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(tab+i) ;
}
unsigned int SizeOfE_pt_tab_2 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(&(tab[i])) ;
}
unsigned int SizeOfE_tab_acces_1 (void) {
  int x = 1;
  int i = 2;
  int *p = &x;
  int tab[5] = {0,1,2,3,4};
  return sizeof(tab[i]) ;
}

int main (void) {
  int r = 0 ;
  r += sizeof (struct St);
  // How to write something like this: assert r == sizeof (st)
  //@ assert r != 0;
  r += SizeOf_1 ();
  r += SizeOf_2 ();
  r += SizeOfE_pt1 ();
  r += SizeOfE_pt2 ();
  r += SizeOfE_pt3 ();
  r += SizeOfE_pt_deref_1 ();
  r += SizeOfE_tab_1 ();
  r += SizeOfE_pt_tab_1 ();
  r += SizeOfE_pt_tab_2 ();
  r += SizeOfE_tab_acces_1 ();
  //@ slice pragma expr r;
  return r;
}
