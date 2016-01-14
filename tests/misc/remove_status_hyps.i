/* run.config_no_native_dynlink
   CMD: bin/toplevel.byte
   OPT: -load-script tests/misc/remove_status_hyps.ml
*/
/* run.config
   OPT: -load-script tests/misc/remove_status_hyps.ml
*/

int main(void) {
  /*@ assert P1: \true; */;
  /*@ assert P2: \true; */;
  /*@ assert P3: \true; */;
  /*@ assert P4: \true; */;
  return 0;
}
