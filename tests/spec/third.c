/* run.config
   DONTRUN: linked with first which is the real test.
*/
/*@ behavior b:
  requires \valid(third);
  ensures \result == 0;*/
int bar(int *third) {
  third=(int*)*third;
  return 0;
}
