/* run.config
OPT: -cpp-extra-args="-DNON_GHOST_DECL_GHOST_DEF"
OPT: -cpp-extra-args="-DGHOST_DECL_NON_GHOST_DEF"
OPT: -cpp-extra-args="-DGHOST_DEF_NON_GHOST_DECL"
OPT: -cpp-extra-args="-DNON_GHOST_DEF_GHOST_DECL"
*/

#ifdef NON_GHOST_DECL_GHOST_DEF

void function(void) ;
/*@ ghost void function(void){ } */

void user(void){
  function();
}

#endif

#ifdef GHOST_DECL_NON_GHOST_DEF

/*@ ghost void function(void) ; */
void function(void){ }

void user(void){
  function();
}

#endif

#ifdef GHOST_DEF_NON_GHOST_DECL

/*@ ghost void function(void){ } */
void function(void) ;

void user(void){
  function();
}

#endif

#ifdef NON_GHOST_DEF_GHOST_DECL

void function(void){ }
/*@ ghost void function(void) ; */

void user(void){
  function();
}

#endif
