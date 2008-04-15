/* run.config
   DONTRUN: pending FS#350
*/
/*@ requires argc <= 10;
  requires \valid(argv + (0..argc-1); */
int main(int argc, char **argv) {
  int i;
  char *t[10];
  for(i = 0; i < argc; i++) t[i] = argv[i];
  return 0;
}

/*
Local Variables:
compile-command: "LC_ALL=C make fs350"
End:
*/
