/* run.config
   OPT: -obfuscate -print
*/
int my_var = 0;

/*@ global invariant I: my_var >= 0; */

enum my_enum {
  first, second, third = 4
};

/*@ requires my_var > 0;
    ensures my_var > \old(my_var);
*/
int my_func () {

  enum my_enum x = first;
  /*@ assert my_var >= first; */
  my_var++;
  return my_var + x;

}
