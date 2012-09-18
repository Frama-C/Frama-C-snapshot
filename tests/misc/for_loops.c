/* run.config
   STDOPT:
   STDOPT: +"-main main_2"
   STDOPT: +"-main g"
*/
#include "share/libc/__fc_builtin.c"

int x;
int f();

void main_2 () {
  int i,j;
  int nSelectors = Frama_C_interval(0,100);
  int w=0,v = 0;
  
  for (j = 0; j < nSelectors; j++) { if (Frama_C_interval(0,1)) w += 1;
    CEA_F(w);}
   // w widens to top_int
  
}

void main () {
  int i,j;
  int nSelectors = Frama_C_interval(0,0x7FFFFFFF);
  int w=0,v = 0;
  
  for (j = 0; j <= nSelectors; j++) 
    { v = j ;
      while (v>0) v--;
      CEA_F(j);}
  
}

void g () {
  int j;
  int T[1000];
  int nSelectors = Frama_C_interval(0,1000);
  int w=0;
  CEA_DUMP();
  for (j = 0; j < nSelectors; j++) T[j] = 1;
  CEA_DUMP();
  for (j = 0; j < nSelectors; j++) w += T[j];
  return;
}
