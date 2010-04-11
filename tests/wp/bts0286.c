
int cpt0 ;

/*@
requires cpt0==0;
ensures \false ;
*/
void exit (int status);

int cpt1 ;
extern int tab1[] ;

/*@
requires cpt1==0;
assigns tab1[..] ;
*/
void f1 (int arg);

/*@
requires cpt0==0;
requires cpt1==0;
ensures \false ;
*/
void g (int x) {
  f1(4);
  exit(1);
}

int main (void) {return 0;}
