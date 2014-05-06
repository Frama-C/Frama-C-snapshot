/* run.config
   GCC:
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -main main2 -journal-disable
   OPT: -val -deps -out -input -main main3 -journal-disable
*/


char StaticAllocHEAP[10000]={2,2,2,2};
void *LIB_MEM_StaticAlloc(int size, int hint) {

  static int next_free=0;
  CEA_ALLOCATION_WITH_STATIC_ALLOC(size);
  void * new = &(StaticAllocHEAP[next_free]);
/* Enforce a 4 byte boundary for allocation */
  next_free += (size%4==0)?size:size+4-size%4;
  return new;
}

struct S {int fst; char snd;};
#define MAX 3
void main() {
  struct S (*v)[MAX];
  v = (struct S(*)[MAX]) LIB_MEM_StaticAlloc(sizeof(struct S) * MAX, 0);
  int i = 29;
  do {
    CEA_I(i);
    v[0][i].snd=1;}
  while (i-- > -1);

  v[0][i].fst = 0;
}


void main2(void)
{ struct S (*v)[3] ;
  int i ;
  int tmp___0 = 0;

  {
  v = (struct S (*)[3])LIB_MEM_StaticAlloc((int )(sizeof(struct S ) * 3U), 0);
  i = 29;
  {

  while (1) {
    CEA_TMP(tmp___0);
    CEA_I(i);
    (*(v + 0))[i].snd = (char)1;
    tmp___0 = i;
    i --;
    if (! (tmp___0 > -1)) {
      goto while_0_break;
    }
  }

  while_0_break: /* CIL Label */ ;
  }
  (*(v + 0))[i].fst = 0;

  return;
}
}

void main3() {
  struct S (*v)[MAX];
  v = (struct S(*)[MAX]) LIB_MEM_StaticAlloc(sizeof(struct S) * MAX, 0);
  int i = 29;
  do {
    v[0][i].snd=1;
    i--;}
  while (i > 0);
  v[0][i].fst = 0;
}
