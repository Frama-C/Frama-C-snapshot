/* run.config
   OPT: -wp-extern-arrays
 */
/* run.config_qualif
   OPT: -wp-extern-arrays
   OPT: -wp-extern-arrays -wp-model +ref
 */

/* -------------------------------------------------------------------------- */
/* --- Sequences                                                          --- */
/* -------------------------------------------------------------------------- */

/*@ 
  axiomatic Sequences {
  
  logic int call( int seq , integer fct );
  logic int concat( int seq1 , int seq2 );

  }
*/

int seq = 0 ;

/* -------------------------------------------------------------------------- */
/* --- Injector / Collector                                               --- */
/* -------------------------------------------------------------------------- */

int service_cpt ;
int service_id[] ;
int service_result[] ;

/*@
  ensures ID: service_id == { \old(service_id) \with [service_cpt] = id } ;
  ensures RESULT: \result == service_result[service_cpt] ;
  ensures CPT: service_cpt == \old(service_cpt)+1 ;
  ensures SEQ: seq == call( \old(seq) , 0xF1 );
  assigns A: seq,service_cpt,service_id[..];
*/
int service(int id) ;


/* -------------------------------------------------------------------------- */
/* --- Job                                                                --- */
/* -------------------------------------------------------------------------- */

/*@
  ensures SEQ: seq == call( call ( \old(seq) , 0xF1 ) , 0xF1 ) ;
  ensures ID1: service_id[ \old(service_cpt)+1 ] == a ;
  ensures ID2: service_id[ \old(service_cpt)+2 ] == b ;
  ensures R1: \result != 0 ==> service_result[ \old(service_cpt)+1 ] != 0 ;
  ensures R2: \result != 0 ==> service_result[ \old(service_cpt)+2 ] != 0 ;
  ensures R0: \result != 0 ==> *error == 0 ;
  ensures E1: service_result[ \old(service_cpt)+1 ] == 0 ==> *error == a ;
  ensures E2: service_result[ \old(service_cpt)+1 ] != 0 ==>
              service_result[ \old(service_cpt)+2 ] == 0 ==> *error == b ;
  assigns A: seq,service_cpt,service_id[..],*error;
*/
int job( int a , int b , int * error )
{
  *error = 0 ;

  int ra = service( a );
  int rb = service( b );
  
  if (!ra) *error = a ;
  else if (!rb) *error = b ;

  return ra && rb ;
}
