/*@
requires \valid(p);
assigns *p;
*/
void f2(int * p) { *p = 3; }
 
/*@
assigns \nothing;
*/
void main2()
{ int c=2;
f2(&c);
}
 
/*@ assigns \nothing; */
void f2b()
{ int t[2]; t[0]=2; /* ... */ }

/* 
Local Variables:
compile-command: "LC_ALL=C make -j dillon1"
End:
*/
