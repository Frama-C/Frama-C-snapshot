/*@
   requires \valid(a);
   ensures *\result == *a;
*/
int* foo(int* a){
   return a;
}

/*
Local Variables:
compile-command: "LC_ALL=C make bts6453"
End:
*/

