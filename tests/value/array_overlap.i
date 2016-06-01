char T[10]={1,1,1,2,2,3,0};
char U[10]={1,1,1,2,2,3,0};

struct S { char t[6]; };


void main (int c) {

  struct S* ptr;
  ptr = &T[1];
  *ptr = *(struct S*)(&T[0]);

  {int i;
  if (c) i = 0; else i = 1;
  ptr = &U[i];
  *ptr = *(struct S*)(&U[0]);
  
    }

}
