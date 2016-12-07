/* run.config
   OPT:
   OPT: -wp-msg-key refusage -wp-model Caveat
   OPT: -wp-msg-key refusage -wp-model Caveat -wp-unalias-vars src
*/

/* run.config_qualif
   DONTRUN:
*/

/*@ requires v1: \valid( ptr );
  @ requires v2: \valid_read( src + idx );
  @ requires s1: \separated( ptr, src + idx );
  @ assigns      *ptr;
  @*/
void f (char *ptr, char const *src, unsigned idx) {
  src += idx;
  *ptr = *src;
}
