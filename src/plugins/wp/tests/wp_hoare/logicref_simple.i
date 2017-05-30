/* run.config
   OPT: -wp-model +ref -wp-msg-key refusage
*/

/* run.config_qualif
   OPT: -wp-model +ref
*/

//@ predicate simple (int * p3) = *p3 == 0;
//@ predicate simple_array (int * p4) = p4[3] == 0;
//@ predicate two_star(int **p) = simple(*p);
//@ predicate vpositive (int *p2) = \valid(p2) && *p2 >= 0 ;

//@ requires simple(c) ; assigns \nothing ; ensures \result == 1;
int fsimple (int *c) {return *c+1;}

int t[10];

//@ requires simple_array(&(t[0])); assigns \nothing ; ensures \result == 1 ; 
int fsimple_array (void) {return t[3]+1;}

//@ requires two_star(d) ; assigns \nothing ; ensures \result == 1;
int ftwo_star (int **d) {return **d+1;}


//@ requires vpositive(b) ; assigns *b;ensures *b == 0;
void fvpositive (int *b)
{
  // OK for ref model only
  //@ assert OK: \valid(b);
  *b = 0 ;
}

