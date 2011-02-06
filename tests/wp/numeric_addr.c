/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-proof z3 -wp-print -wp-verbose 2
*/


/*@ requires (no_rte: \valid(Valeur));
    requires (no_rte: \valid((unsigned int *)0x12345678));
    requires (no_rte: \separated(Valeur, (unsigned int *)0x12345678));
    ensures *Valeur == \old(*(unsigned int *)0x12345678);
*/
void LireStatusCpu(unsigned int *Valeur ) {
  /*@ assert (rte: \valid(Valeur)); */
  /*@ assert (rte: \valid((unsigned int *)0x12345678)); */
  *Valeur = *((unsigned int *)0x12345678);
  return;
}
