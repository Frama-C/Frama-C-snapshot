/* run.config
   DONTRUN: cannot find entry point: cent_onzes
   OPT: -val -main test_onzes -journal-disable
   OPT: -val -main cent_onzes -journal-disable
*/

/***************** var CMP cste **********************/
int onze_0 (void) {
  int onze; for (onze=1000; onze >= 12 ; onze--) ; return onze ;
}
int onze_1 (void) {
  int onze; for (onze=1000; onze > 11 ; onze--) ; return onze ;
}
int onze_2 (void) {
  int onze; for (onze=0; onze < 11 ; onze++) ; return onze ;
}
int onze_3 (void) {
  int onze; for (onze=0; onze <= 10 ; onze++) ; return onze ;
}
int onze_4 (void) {
  int onze; for (onze=0; onze != 11 ; onze++) ; return onze ;
}
/***************** cste CMP var **********************/
int onze_5 (void) {
  int onze; for (onze=1000; 12 <= onze ; onze--) ; return onze ;
}
int onze_6 (void) {
  int onze; for (onze=1000; 11 < onze; onze--) ; return onze ;
}
int onze_7 (void) {
  int onze; for (onze=0; 11 > onze; onze++) ; return onze ;
}
int onze_8 (void) {
  int onze; for (onze=0; 10 >= onze; onze++) ; return onze ;
}
int onze_9 (void) {
  int onze; for (onze=0; 11 != onze; onze++) ; return onze ;
}

int r0,r1,r2,r3,r4,r5,r6,r7,r8,r9;
void test_onzes(void)
{
  r0 = onze_0();
  r1 = onze_1();
  r2 = onze_2();
  r3 = onze_3();
  r4 = onze_4();
  r5 = onze_5();
  r6 = onze_6();
  r7 = onze_7();
  r8 = onze_8();
  r9 = onze_9();
}

/***************** !(var CMP cste) **********************/
int cent_onze_0 (void) {
  int cent_onze; for (cent_onze=1000; !(cent_onze < 112) ; cent_onze--) ; return cent_onze ;
}
int cent_onze_1 (void) {
  int cent_onze; for (cent_onze=1000; !(cent_onze <= 111) ; cent_onze--) ;
  return cent_onze ;
}
int cent_onze_2 (void) {
  int cent_onze; for (cent_onze=0; !(cent_onze >= 111) ; cent_onze++) ;
  return cent_onze ;
}
int cent_onze_3 (void) {
  int cent_onze; for (cent_onze=0; !(cent_onze > 110) ; cent_onze++) ;
  return cent_onze ;
}
int cent_onze_4 (void) {
  int cent_onze; for (cent_onze=0; !(cent_onze == 111) ; cent_onze++) ;
  return cent_onze ;
}
/***************** !(cste CMP var) **********************/
int cent_onze_5 (void) {
  int cent_onze; for (cent_onze=1000; !(112 > cent_onze) ; cent_onze--) ; return cent_onze ;
}
int cent_onze_6 (void) {
  int cent_onze; for (cent_onze=1000; !(111 >= cent_onze) ; cent_onze--) ;
  return cent_onze ;
}
int cent_onze_7 (void) {
  int cent_onze; for (cent_onze=0; !(111 <= cent_onze) ; cent_onze++) ;
  return cent_onze ;
}
int cent_onze_8 (void) {
  int cent_onze; for (cent_onze=0; !(110 < cent_onze) ; cent_onze++) ;
  return cent_onze ;
}
int cent_onze_9 (void) {
  int cent_onze; for (cent_onze=0; !(111 == cent_onze) ; cent_onze++) ;
  return cent_onze ;
}

/***************** **********************/

int c0,c1,c2,c3,c4,c5,c6,c7,c8,c9;
void test_cent_onzes(void)
{
  c0 = cent_onze_0();
  c1 = cent_onze_1();
  c2 = cent_onze_2();
  c3 = cent_onze_3();
  c4 = cent_onze_4();
  c5 = cent_onze_5();
  c6 = cent_onze_6();
  c7 = cent_onze_7();
  c8 = cent_onze_8();
  c9 = cent_onze_9();
}
