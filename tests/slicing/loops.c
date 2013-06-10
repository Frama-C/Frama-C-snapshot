/* run.config
   OPT: -check -deps -lib-entry -main f1 -slice-pragma f1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main f1 -slice-assert f1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main f2 -slice-pragma f2 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main f2 -slice-assert f2 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main test_infinite_loop_3 -slice-value G -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main test_infinite_loop_4 -slice-value G -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main test_infinite_loop_5 -slice-value G -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main loop -slice-value Z  -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-calls loop -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-pragma loop -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-assert loop -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main loop -slice-rd Y -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main loop -slice-rd Z -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main loop -slice-wr Y -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -main loop -slice-wr Z  -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main stop_f1 -slice-pragma stop_f1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main stop_f1 -slice-assert stop_f1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main stop_f2 -slice-pragma stop_f2 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main stop_f2 -slice-assert stop_f2  -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-value Z  -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-rd Y -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-rd Z -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-wr Y -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -slice-wr Z -journal-disable -then-on 'Slicing export' -print
   OPT: -check -deps -lib-entry -main alarm -slice-threat alarm -journal-disable -then-on 'Slicing export' -print


 */

int f1 (int c) {
  int x = 0, s = 0;
  if (c) {
    while(1) { /* infinite loop */
      s++;
      //@ assert s > 0 ;
      }
    //@ assert \false ;
    }
  else
    x = 1;
   //@ slice pragma stmt;
  x ++;
  return x;
}

void f2 (int c) {
  int x1 = 0, x2 = 0;

  while (1) {
    if (c)
      x1++;
    else
      x2++;
    //@slice pragma expr x1;
    //@ assert x2 > 0 ;
  }
}

/*-------------------------------------------*/
void stop(void) __attribute__ ((noreturn)) ;

int stop_f1 (int c) {
  int x = 0, s = 0;
  if (c) {
    while(s < c) {
      s++;
      //@ assert s > 0 ;
      }
    stop () ; /* never returns */
    }
  else
    x = 1;
   //@ slice pragma stmt;
  x ++;
  return x;
}

void stop_f2 (int c) {
  int x1 = 0, x2 = 0;

  while (x1+x2 < c + 10) {
    if (c)
      x1++;
    else
      x2++;
    //@slice pragma expr x1;
    //@ assert x2 > 0 ;
    stop () ; /* never loops nor returns */
    x1++;     /* dead code */
    //@ assert \false ;
  }
}
/*-------------------------------------------*/
int G ;
void test_infinite_loop_3 (int ctrl1, int ctrl2,
                           int no_ctrl,
                           int data1, int data2,
                           int no_data) {
  G = 0 ;
  if (ctrl1) {
    G = data1 ;
    if (no_ctrl) { /* Don't control an assignment of G
                    * which leads to the return */
      G = no_data ; /* Don't affect the final value of G
                     * because the assignement
                     * does not lead to the return */
      while (1)
        G = no_data ; /* Don't affect the final value of G
                       * because the assignement
                       * does not lead to the return */
      G = no_data ; /* Don't affect the final value of G
                     * because the assignement
                     * is dead code */
      }
    if (ctrl2)
      G = data2 ;
    }
  return;
}

void test_infinite_loop_4 (int ctrl1, int ctrl2, int no_ctrl,
                           int data1, int data2, int no_data) {
  G = 0 ;
  while (ctrl1) {
    G += data1 ;
    if (no_ctrl) { /* Don't control an assignment of G
                    * which leads to the return */
      G += no_data ; /* Don't affect the final value of G
                     * because the assignement
                     * does not lead to  the return */
      while (1)
        G += no_data ;  /* Don't affect the final value of G
                       * because the assignement
                       * does not lead to the return */
      G += no_data ; /* Don't affect the final value of G
                      * because the assignement
                      * is dead code */
      }
    if (ctrl2)
      G += data2 ;
    }
  return;
}

void test_infinite_loop_5 (int ctrl1, int ctrl2, int no_ctrl,
                           int data1, int data2, int no_data) {
  G = 0 ;
  while (ctrl1) {
    G += data1 ;
    if (no_ctrl) { /* Don't control the final value of G.
                    * It only controls the terminaison of the function.
                    */
      G += no_data ;   /* Don't affect ... */
      while (1)
        G += no_data ; /* Don't affect ... */
      G += no_data ;   /* Don't affect ...  dead code */
      }
    else /* <-- This is the difference with test_infinite_loop_4.
          * It is only a syntaxical difference,
          * and not a semantical difference
          * since the previous statement "G += no_data" is dead.
          */
      if (ctrl2)
        G += data2 ;
    }
  return;
}
/*-------------------------------------------*/
int C1 = 1, C2 = 1 ;
int X, Y, Z ;

void loop (int cond) {
  if (cond) {
    int c = 0 ;
    /*@ loop pragma WIDEN_HINTS X, 10, 100 ; */ while (1) {
      //@ slice pragma ctrl ;
      if (c) {
        X++;
        Y = Z ;
        }
      c=1;
      //@ assert  c==1 ;
      }
    }
  Z = Y ; // dead code with -main main
}
/*---------------------*/
/*@ assigns *p \from p, y, Z ;
 */
void may_write_Y_from_Z (int * p, int y) ;
void test_assigns (int * p, int y) { if (y < Z) *p = y + Z; }
/*---------------------*/
void main (int y) {
  int no_ctrl = 1 ;
  Z = 0;
  if (no_ctrl)
    Z = X ;
  may_write_Y_from_Z (&Y, y) ;
  if (C1) {
    int cond = C2 ;
    loop (cond) ;
    }
}
/*-------------------------------------------*/

void alarm() {
  int i = 1;
  volatile int j = 3;
  //@ assert i == 1;
  j++;
}
