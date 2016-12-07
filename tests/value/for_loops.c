/* run.config*
   STDOPT:
   STDOPT: +"-main main_2"
   STDOPT: +"-main main_3"
   STDOPT: +"-main main_4"
*/

#include "__fc_builtin.h"
int x;

void main_2 () {
  int i,j;
  int nSelectors = Frama_C_interval(0,100);
  int w=0,v = 0;
  
  for (j = 0; j < nSelectors; j++) { if (Frama_C_interval(0,1)) w += 1;
    Frama_C_show_each_F(w);}
   // w widens to top_int
  
}

void main () {
  int i,j;
  int nSelectors = Frama_C_interval(0,0x7FFFFFFF);
  int w=0,v = 0;
  
  for (j = 0; j <= nSelectors; j++) 
    { v = j ;
      while (v>0) v--;
      Frama_C_show_each_F(j);}
  
}

void main_3 () {
  int j;
  int T[1000];
  int nSelectors = Frama_C_interval(0,1000);
  int w=0;
  Frama_C_dump_each();
  for (j = 0; j < nSelectors; j++) T[j] = 1;
  Frama_C_dump_each();
  for (j = 0; j < nSelectors; j++) w += T[j];
  Frama_C_show_each(w);
}

void main_4 () {
  int j = 0;
  int v = 0;

  while(j <= 5)
    { v = j;
      while (v < j+10 && v > 0) {
        v++;
      }
      Frama_C_show_each_F(j,v); // Check that the propagation strategy is "not too bad": if possible, without slevel, propagate the result of the loop plus the case where the loop condition never hold together.
      j ++;
    }
}
