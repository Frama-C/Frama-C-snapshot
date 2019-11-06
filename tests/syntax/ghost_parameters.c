/* run.config
   STDOPT: +" -cpp-extra-args=-DARGS_NOT_VOID"
   STDOPT: +" -cpp-extra-args=-DARGS_VOID"
   STDOPT: +" -cpp-extra-args=-DCOHERENT_DECL"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_LOCAL_DECL_NON_GHOST"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_GLOBAL_DECL_NON_GHOST"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_LOCAL_DECL_GHOST"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_GLOBAL_DECL_GHOST"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_LOCAL_DECL_MORE_GHOSTS"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_GLOBAL_DECL_MORE_GHOSTS"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_LOCAL_DECL_MORE_NON_GHOSTS"
   STDOPT: +" -cpp-extra-args=-DINCOHERENT_GLOBAL_DECL_MORE_NON_GHOSTS"
   STDOPT: +" -cpp-extra-args=-DVOID_EMPTY_GHOST_PARAMETER_LIST"
   STDOPT: +" -cpp-extra-args=-DVOID_GHOST_PARAMETER"
*/

#ifdef ARGS_NOT_VOID

void function(int a, int b) /*@ ghost (int c, int d) */{

}

void caller(void){
  // VALID
  function(1, 2) /*@ ghost (3, 4)*/ ;
  /*@ ghost function(1, 2, 3, 4) ; */

  // INVALID
  function(1, 2) ;
  function(1, 2) /*@ ghost (3) */ ;
  function(1) /*@ ghost (2, 3) */ ;
  function(1, 2, 3, 4) ;
  function() /*@ ghost (1, 2, 3, 4) */ ;

  /*@ ghost function(1, 2) ; */
  /*@ ghost function(1) ; */
  /*@ ghost function(1, 2, 3) ; */
}

#endif

#ifdef ARGS_VOID

void function(void) /*@ ghost (int c, int d) */{

}

void caller(void){
  // VALID
  function() /*@ ghost (3, 4)*/ ;
  /*@ ghost function(3, 4) ; */

  // INVALID
  function() ;
  function() /*@ ghost (3) */ ;
  function(1) /*@ ghost (2, 3) */ ;
  function(1, 2) ;
  function() /*@ ghost (1, 2, 3) */ ;

  /*@ ghost function() ; */
  /*@ ghost function(1) ; */
  /*@ ghost function(1, 2, 3) ; */
}

#endif

#ifdef COHERENT_DECL

void caller(void){
  void function(int a, int b) /*@ ghost(int c, int d) */ ;
  function(1, 2) /*@ ghost (3, 4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ ;

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_LOCAL_DECL_NON_GHOST

void caller(void){
  void function(int b) /*@ ghost(int c, int d) */ ;
  function(2) /*@ ghost (3, 4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_GLOBAL_DECL_NON_GHOST

void function(int b) /*@ ghost(int c, int d) */ ;

void caller(void){
  function(2) /*@ ghost (3, 4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_LOCAL_DECL_GHOST

void caller(void){
  void function(int a, int b) /*@ ghost(int d) */ ;
  function(1, 2) /*@ ghost (4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_GLOBAL_DECL_GHOST

void function(int a, int b) /*@ ghost(int d) */ ;

void caller(void){
  function(1, 2) /*@ ghost (3) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_LOCAL_DECL_MORE_GHOSTS

void caller(void){
  void function(int a, int b, int c) /*@ ghost(int d) */ ;
  function(1, 2, 3) /*@ ghost (4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_GLOBAL_DECL_MORE_GHOSTS

void function(int a, int b, int c) /*@ ghost(int d) */ ;

void caller(void){
  function(1, 2, 3) /*@ ghost (4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_LOCAL_DECL_MORE_NON_GHOSTS

void caller(void){
  void function(int a) /*@ ghost(int b, int c, int d) */ ;
  function(1) /*@ ghost (2, 3, 4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef INCOHERENT_GLOBAL_DECL_MORE_NON_GHOSTS

void function(int a) /*@ ghost(int b, int c, int d) */ ;

void caller(void){
  function(1) /*@ ghost (2, 3, 4) */ ;
}

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

#endif

#ifdef VOID_EMPTY_GHOST_PARAMETER_LIST

void function_void(void) /*@ ghost () */ {

}

#endif

#ifdef VOID_GHOST_PARAMETER

void function_void(void) /*@ ghost (void) */ {

}

void function_non_void(int x) /*@ ghost (void) */ {

}

#endif