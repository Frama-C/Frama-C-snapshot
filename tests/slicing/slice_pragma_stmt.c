/* run.config
   OPT: -print  -journal-disable
   OPT: -main nop1 -slice-pragma nop1 -slice-print  -journal-disable
   OPT: -main nop2 -slice-pragma nop2 -slice-print  -journal-disable
   OPT: -main nop3 -slice-pragma nop3 -slice-print  -journal-disable
   OPT: -main nop4 -slice-pragma nop4 -slice-print  -journal-disable
   OPT: -main nop5 -slice-pragma nop5 -slice-print  -journal-disable
   OPT: -main nop6 -slice-pragma nop6 -slice-print  -journal-disable
   OPT: -main nop7 -slice-pragma nop7 -slice-print  -journal-disable
   OPT: -main nop8 -slice-pragma nop8 -slice-print  -journal-disable
   OPT: -main double_effect1 -slice-pragma double_effect1 -slice-print  -journal-disable
   OPT: -main double_effect2 -slice-pragma double_effect2 -slice-print  -journal-disable
   OPT: -main double_effect3 -slice-pragma double_effect3 -slice-print  -journal-disable
   OPT: -main double_effect4 -slice-pragma double_effect4 -slice-print  -journal-disable
   OPT: -main double_effect5 -slice-pragma double_effect5 -slice-print  -journal-disable
   OPT: -main test1 -slice-pragma test1 -slice-print  -journal-disable
   OPT: -main test2 -slice-pragma test2 -slice-print  -journal-disable
   OPT: -main test3 -slice-pragma test3 -slice-print  -journal-disable
   OPT: -main test4 -slice-pragma test4 -slice-print  -journal-disable
   OPT: -main test5 -slice-pragma test5 -slice-print  -journal-disable
   OPT: -main test6 -slice-pragma test6 -slice-print  -journal-disable
   OPT: -main test7 -slice-pragma test7 -slice-print  -journal-disable
   OPT: -main test8 -slice-pragma test8 -slice-print  -journal-disable
   OPT: -main test9 -slice-pragma test9 -slice-print  -journal-disable
*/
//-------------------
int x, y ;
//-------------------
void nop1(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing isn't correct since the effect...
  ; // <----- ...is missing with -print option
  x = 1 ;
 }
void nop2(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing isn't correct since the effect...
  {;} // <----- ...is missing with -print option
  x = 1 ;
 }

void nop3(int c1, int c2) {
  //@ slice pragma stmt;  // <----- slicing isn't correct since the effect...
  {;{;;};} // <----- ...is missing with -print option
  x = 1 ;
 }
void nop4(int c1, int c2) {
  //@ slice pragma stmt;
  if (c1) {;{;;};}
  x = 1 ;
 }
void nop5(int c1, int c2) {
  if (c2) goto L ;
  //@ slice pragma stmt; // <----- slicing is correct, but not the output
 L:;
  x = 1 ;
 }
void nop6(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing is correct, but not the output
 L:;
  x = 1 ;
 }
void nop7(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing is correct, but not the output
 L:{;}
  x = 1 ;
 }
void nop8(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing is correct, but not the output
  {L:{;}}
  x = 1 ;
 }
//-------------------
void double_effect1(int c1, int c2) {
  //@ slice pragma stmt;  // <----- slicing isn't correct since the...
  x += y++ ;   // <----- ...effect is lost with -print option
 }
void double_effect2(int c1, int c2) {
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
  { x += y++ ; }   // <----- ...effect is lost with -print option
 }
void double_effect3(int c1, int c2) {
  if (c2) goto L ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
  L: x += y++ ;   // <----- ...effect is lost with -print option
 }
void double_effect4(int c1, int c2) {
  if (c2) goto L ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
 L: {x += y++ ; }   // <----- ...effect is lost with -print option
 }
void double_effect5(int c1, int c2) {
  if (c2)
    //@ slice pragma stmt;
    {x += y++ ; }
 }
//-------------------
void test1(int c1, int c2) {
  if (c1 < c2)
    c1 = c2 ;
  //@ slice pragma stmt;
  x = c1 ;
}
void test2(int c1, int c2) {
  if (c1 < c2)
    c1 = c2 ;
  //@ slice pragma stmt;
  x = c1 ;
  y = c2 ;
}
void test3(int c1, int c2) {
  if (c1 < c2)
    c1 = c2 ;
  //@ slice pragma stmt;
  {x = c1 ;}
  y = c2 ;
}
void test4(int c1, int c2) {
  if (c1 < c2)
    c1 = c2 ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
  {x = c1 ; c2 ++ ;}  // <----- ...effect is lost with -print option
  y = c2 ;
}
void test5(int c1, int c2) {
  if (c1 < c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
 L: x = c1 ;   // <----- ...effect is lost with -print option
  y = c2 ;
}
void test6(int c1, int c2) {
  if (c1 < c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt;  // <----- slicing isn't correct since the...
 L: x = c1++ ;   // <----- ...effect is lost with -print option
  y = c2 ;
}
void test7(int c1, int c2) {
  if (c1 < c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
 L: {x = c1++ ; c2 ++ ;}   // <----- ...effect is lost with -print option
  y = c2 ;
}
void test8(int c1, int c2) {
  if (c1 < c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
  { L: x = c1++ ; c2 ++ ;}   // <----- ...effect is lost with -print option
  y = c2 ;
}
void test9(int c1, int c2) {
  if (c1 < c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt; // <----- slicing isn't correct since the...
  { x = c1 ; L: c2 = c2 + 1 ;}   // <----- ...effect is lost with -print option
  y = c2 ;
}
