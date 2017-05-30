/* run.config
   OPT: -wp-model Typed -wp-alias-vars src,dst
   OPT: -wp-model Typed -wp-context-vars src,dst -wp-fct memcpy_context_vars
*/

/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-alias-vars src,dst -wp-par 1
   OPT: -wp -wp-model Typed -wp-context-vars src,dst -wp-prop ok -wp-par 1
*/

//-----------------------------------------------------------------------------
// FUNCTION memcpy_alias_vars: since input pointers are MODIFIED, Caveat
// model (setting 'src' and  'dst' in context) CANNOT be used.
//-----------------------------------------------------------------------------
/*@ requires write_access: \valid( dst + (0 .. len-1) );
  @ requires read_access:  \valid_read( src + (0 .. len-1) );
  @ requires unaliasing:   \separated( dst + (0 .. len-1) , src + (0 .. len-1) );
  @ assigns                dst[ 0 .. len-1 ];
  @ ensures memcpy:        \forall integer k; 0 <= k < len-1 ==> dst[k] == \old( src[k] );
  @ ensures unmodified:    \forall integer k; 0 <= k < len-1 ==> src[k] == \old( src[k] );
  @*/
void memcpy_alias_vars (unsigned char *src, unsigned char *dst, int len) {
  /*@ loop invariant len: len <= \at( len, LoopEntry );
    @ loop invariant src: src + len == \at( src + len, LoopEntry );
    @ loop invariant dst: dst + len == \at( dst + len, LoopEntry );
    @ loop assigns        src, dst, len, \at( dst[0 .. len-1], LoopEntry ) ;
    @ loop invariant cpy: \let idx = \at( len, LoopEntry ) - len ;
    @                     \forall integer k; 0 <= k < idx
    @                     ==> \at( dst, LoopEntry )[k] == \at( src, LoopEntry )[k];
    @*/
  while (len > 0) {
    *(dst++) = *(src++);
    len--;
  }
}

//-----------------------------------------------------------------------------
// FUNCTION memcpy_context_vars: since input pointers are UNMODIFIED, Caveat 
// model (setting 'src' and 'dst' in context) CAN be used.
//-----------------------------------------------------------------------------
/*@ requires write_access:  \valid( dst + (0 .. len-1) );
  @ requires read_access:   \valid_read( src + (0 .. len-1) );
  @ requires unaliasing:    \separated( dst + (0 .. len-1) , src + (0 .. len-1) );
  @ assigns                 dst[ 0 .. len-1 ];
  @ ensures memcpy:     ok: \forall integer k; 0 <= k < len-1 ==> dst[k] == \old( src[k] ); 
  @ ensures unmodified: ok: \forall integer k; 0 <= k < len-1 ==> src[k] == \old( src[k] ); 
  @*/
void memcpy_context_vars (unsigned char *src, unsigned char *dst, int len) {
  unsigned char *src2=src, *dst2=dst;

  /*@ loop invariant ok: len: len <= \at( len, LoopEntry );
    @ loop invariant ok: src: src2 + len == \at( src2 + len, LoopEntry );
    @ loop invariant ok: dst: dst2 + len == \at( dst2 + len, LoopEntry );
    @ loop assigns            src2, dst2, len, \at( dst2[0 .. len-1], LoopEntry ) ;
    @ loop invariant ok: cpy: \let idx = \at( len, LoopEntry ) - len ;
    @                         \forall integer k; 0 <= k < idx
    @                         ==> \at( dst2, LoopEntry )[k] == \at( src2, LoopEntry )[k];
    @*/
  while (len > 0) {
    *(dst2++) = *(src2++);
    len--;
  }
}

//-----------------------------------------------------------------------------
