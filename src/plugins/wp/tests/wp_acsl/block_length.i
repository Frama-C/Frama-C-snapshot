/* run.config_qualif
   OPT: -wp -wp-par 1
*/

int t[20]; 

int mat[10][5]; 

struct S {int i; int tab[4];}; 

int x; 

struct S s;
struct S ts[4]; 

/*@
   ensures Pt: \block_length(t) == 20*sizeof(int) ; 
   ensures Psiz1 : sizeof(mat[1]) == 5*sizeof(int);
   ensures Pmat1 : \block_length(mat[1]) == 50*sizeof(int);
   ensures Psiz2 : sizeof(mat) == 50*sizeof(int);
   ensures Pmat2 : \block_length(mat) == 50*sizeof(int);
   ensures Ps : \block_length(&s) == \block_length(&x) + 4*sizeof(int);
   ensures Pts : \block_length(ts) == 4* \block_length(&s);
 */
void f(void){return;}
