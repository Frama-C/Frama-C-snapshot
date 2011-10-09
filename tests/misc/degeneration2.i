void main (int c,int d) {
void *A,*B,*C,*D, *E;
 
  if (c) {A = (void*)&B;
  B= (void*)&C;
  C= (void*)&D;
  D= (void*)&E;
    };
  A = (void*)(-(int)A);
  
  while (c) {
    A = (void*)*((int*)A);
    }

  int offset_uninit;
  char T[10][10];
  int x = (d<=10)?((d>=0)?d:0):0;

  int vv = T[x][offset_uninit];

}
