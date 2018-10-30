/* run.config
   DONTRUN:
   COMMENT: function declaration with a contract
   COMMENT: does not work, but yet for a bad reason
   COMMENT: (assigns \nothing generated and not yet supported)
*/

int X = 0;

/*@ requires X == 0; */
void f(void);

int main(void) {
  f();
  return 0;
}
