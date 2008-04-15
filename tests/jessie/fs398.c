/* run.config
   DONTRUN: FS#398
*/

int atoi(char *c);

void f() 
{
  atoi(0);
}

/*
Local Variables:
compile-command: "LC_ALL=C make fs398"
End:
*/
