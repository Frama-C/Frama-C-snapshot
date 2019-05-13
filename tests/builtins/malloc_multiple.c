/* run.config*
   OPT: -eva @EVA_CONFIG@ -slevel 50 -eva-mlevel 5
*/
#include<stdlib.h>
#define MAX 10

long long *T[MAX];
int allocate_T(int v)
{ int counter;
  int error=0;
  T[0]=malloc(sizeof(long long)) ;
  
  for(counter=1;counter<MAX;counter++)
    { T[counter]=malloc(sizeof(long long));
      *(T[counter]) = v;
      if (T[counter]==NULL) error++;
    }
  *(T[0]) = 111;
  *(T[1]) = 222;
  *(T[2]) = 333;
  *(T[3]) = 444;
  *(T[4]) = 555;
  return error;
}

int *F[MAX];
void allocate_and_free_last(void) {
  for(int counter=0;counter<MAX;counter++)
    { F[counter]=malloc(sizeof(int));
      *(F[counter]) = counter;
      Frama_C_show_each_F(F[counter]);
    }

  free(F[MAX-1]);
  //@ assert !\dangling(F[0]);
  //@ assert !\dangling(F[MAX-2]);
  //@ assert \dangling(F[MAX-1]);
  return;
}

int main(int a) {
  allocate_T(1);
  allocate_T(2);
  allocate_and_free_last();
  return 0;
}
