#include "malloc.c"
typedef struct S {char v; char w; char x;} U;

U *G1;

void f() {
  G1->w = 0;
  G1->x = 0;
}


typedef struct LIST_S {int v; struct LIST_S *next;} LIST;
int acc=0;

LIST *G;
void h2(){
  G=G->next;
  acc = G->v;
  while (G=G->next) {
    acc = acc + G->v;
    }
}

LIST ll = { 3 , 0 };

void main () {
  G = (LIST*)alloca(sizeof(LIST));
  acc+=0;
  G->v = 0;
  acc+=0;
  G->next = (LIST*)malloc (sizeof(LIST));
  acc+=0;
  G->next->v = 1;
  acc+=0;
  G->next->next = (LIST*)realloc (G,sizeof(LIST));
  acc+=0;
  G->next->next->v = 2;
  G->next->next->next = &ll;
  G->next->next->next->v = 5;
}
