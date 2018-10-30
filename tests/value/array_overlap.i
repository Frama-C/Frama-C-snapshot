char T[10]={1,1,1,2,2,3,0};
char U[10]={1,1,1,2,2,3,0};

struct S { char t[6]; };
volatile int rand;

void main () {

  struct S* ptr;
  ptr = &T[1];
  if (rand)
    *ptr = *(struct S*)(&T[0]);
  {int i;
  if (rand) i = 0; else i = 1;
  ptr = &U[i];
  *ptr = *(struct S*)(&U[0]);
  
    }

}
