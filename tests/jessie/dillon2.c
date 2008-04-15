/* run.config
   DONTRUN: FS#394
*/
typedef struct { int a; } las;
las * p;
//@ logic las** p_ref{L} = &p;
 
/*@
requires \valid(p);
assigns p->a;
*/
void f3()
{ p->a = 5; }
 
/*@
requires \valid(p);
assigns (*p_ref)->a;
*/
void g3(int * p)
{ f3(); }

/* 
Local Variables:
compile-command: "LC_ALL=C make -j dillon2"
End:
*/
