/* run.config
   OPT: -context-valid-pointers -lib-entry -main f -slice-print -slice-assert f 
   OPT: -context-valid-pointers -lib-entry -main f -slice-print -slice-assert f -slicing-keep-annotations
*/

/* ~/frama-c/bin/viewer.opt -context-valid-pointers -lib-entry -main f \
     -slice-print -slice-assert f -no-unicode -quiet dillon-12mai09.c \
     &> result.log */

typedef struct { int a; double b; } las;

void g (las * p)
{ int i=0;
    while (i<5)
    {   p->b  = (double)i / (double)(i+1);
        p->a = 1 + i;
        i++;
    }
  //@ assert 1<=p->a<=6;
  //@ assert 0.0<=p->b<=1.0;
}

//@assigns *p;
void f (las * p)
{ g(p);
  //@ assert 0.0<=p->b<=1.0;
} 
