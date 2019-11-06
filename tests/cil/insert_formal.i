/* run.config
MODULE: @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -print
*/

//                   v
void void_circumflex( void ) {

}

//                            v
void void_circumflex_g( void ) {

}

//                    v
void void_dollar( void ) {

}

//                        v
void void_dollar_g( void ) {

}

//                v
void a_circumflex( int a ) {

}

//                  v
void a_dollar( int a ) {

}

//                          v
void a_circumflex_g( int a ) {

}

//                      v
void a_dollar_g( int a ) {

}

//             v
void a_a( int a ){

}

//                      v
void ghost_a_circumflex( void ) /*@ ghost ( int a ) */ {

}

//                       v
void ghost_a_dollar( void ) /*@ ghost ( int a ) */ {

}

//                                           v
void ghost_a_circumflex_g( void ) /*@ ghost ( int a ) */ {

}

//                                             v
void ghost_a_dollar_g( void ) /*@ ghost ( int a ) */ {

}

//                                      v
void ghost_a_a( void ) /*@ ghost ( int a ) */ {

}

//                  v
void a_b_c_a (int a, int b, int c) {

}

//                         v
void b_a_c_a (int b, int a, int c) {

}

//                                              v
void all_ghost_a_b_c_a ( void )/*@ ghost (int a, int b, int c) */ {

}

//                                                     v
void all_ghost_b_a_c_a ( void )/*@ ghost (int b, int a, int c) */ {

}

//                        v
void a_ghost_b_c_a ( int a )/*@ ghost (int b, int c) */ {

}

//                                           v
void b_ghost_a_c_a ( int b )/*@ ghost (int a, int c) */ {

}

/*@ ghost
  //                     v
  void g_void_circumflex( void ) {

  }

  //                      v
  void g_void_dollar( void ) {

  }

  //                  v
  void g_a_circumflex( int a ) {

  }

  //                    v
  void g_a_dollar( int a ) {

  }

  //               v
  void g_a_a( int a ){

  }

  //                    v
  void g_a_b_c_a (int a, int b, int c) {

  }

  //                           v
  void g_b_a_c_a (int b, int a, int c) {

  }
*/
