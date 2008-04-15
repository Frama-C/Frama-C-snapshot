
void f(int *p) {}
int g()
{
 int i;
 f(&i);
 return i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make roux3"
End:
*/
