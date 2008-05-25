/* run.config
   DONTRUN: ghost code is not supported
 */
int G= 1, H = 2;

/*@ ghost char toggle = 0, y[10] = {1,2} ; */

//@ global invariant sum_G_H : toggle ==> G + H <=  3;

void main () {

/*@ ghost int LOCAL2 = 0; */

  if (H) /*@ ghost
    int local = 0;
    goto HH;
    local += G;
         */
 /*@ ghost
    int local = 0;
    goto HH;
    HH:local += G;
         */
    //@ assert \false;
  LL:G++;


  /*@ghost LOCAL2++ ; */

}
