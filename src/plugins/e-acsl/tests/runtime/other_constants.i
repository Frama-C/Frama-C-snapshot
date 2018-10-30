/* run.config
   COMMENT: non integer constants
*/

enum bool { false, true };

int main(void) {
  // waiting for clarification of semantics of ACSL's literal strings
  //  /*@ assert "toto" != "titi"; */
  /*@ assert 'c' == 'c'; */
  /*@ assert false != true; */
  return 0;
}
