/*@ behavior x: assumes c; */
void no_empty_stmt_contract(int c) {
  //@ for x:
  ;
}
