
int G, X, T[10];

// Notice that \result is in fact a shortcut for \at(\result, Post)
/*@ ensures \old(\result - (X + 1)) == 0 ; // test of frama-c comment
*/
int test_at_old (int i) {
  X++;
L : i++;
  T[X] ++ ;
  //@ assert i > \at(i, L) && \at(i, Here) == \at(i, Pre) + 1;
  // Invalid assert Old is forbiden in 'assert'
  // @ assert i > \at(i, Old); 
  return X;
}

// this one seem problematic, but it is not because we push downward 
// the \at to the data.
void at_imbric_ok (int a, int b) {
  a = 0;
La : a = 1;
     b = 0;
Lb : b = 1;
  //@ assert \at(a + \at(b, Lb), La) == 0;
  // Notice that La and Lb have to be in that order to see the problem,
  // because if La appears before in WP computation, it then makes Lb visible
  // without need of special processing.
}

void at_imbric_ko (int a, int b) {
  a = 0;
  T[a] = 0;
La : a = 1;
     b = 0;
Lb : b = 1;
  //@ assert \at(T[ \at(b, Lb)], La) == 0;
}

// label are normalized by cil -> Lb disapear.
void at_same_point (int a, int b) {
  a = 0;
  b = 0;
  T[b] = 0;
La : 
Lb : a = 1;
     b = 1;
  //@ assert \at(T[ \at(b, Lb)], La) == 0;
}
 
 
//@ ensures \old(T[\at(X,Here)] + 1) == \at(T[X], Post) ;
void test_at_imbric (void) {
  X++;
  T[X] ++;
}

void labels_in_index (int a, int b) {
La : a = a+ 1;
     b = 0;
Lb : b = 1;
     T[a-1] = 0;
Lt : T[a-1] = 1;
  //@ assert \at(T[\at(a + \at(b, Lb), Pre)], Lt) == 0;
}


//@ ensures \result[\at(i, Pre)] == 0;
int * result_and_pre (int i) {
  int * p0, * pi;
  p0 = T;
  pi = p0 + i;
  i = 0;
  *pi = i;
  return p0;
}

// result_and_pre contient  no_itself_pointer :
// manque hypothèse qu'un pointeur ne peut pointé
// sur lui-même en l'absence de cast.
 
int *p;
int i; 
/*@ assigns *p;
  ensures p==\old(p); */
void no_itself_pointer(){*p=0;}

int main (void) { return 0 ; }
