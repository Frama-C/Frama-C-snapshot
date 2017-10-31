/* run.config
   DONTRUN: linked with first which is the real test.
*/

/*@ behavior b:
  requires \valid(second);
  ensures \result == 0;*/
int bar(int *second);

void sub (char * c) {
  bar((int*)c);
  
}
