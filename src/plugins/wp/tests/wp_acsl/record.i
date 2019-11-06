/* run.config
   STDOPT: +"-wp-no-extensional"
*/
/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="-qed_ko,-ko" -wp-no-extensional
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="qed_ko,ko" -wp-steps 50 -wp-no-extensional
*/

struct T {
  int i ;
  int j;
};
struct T s1, s2;

int t1[10];
int t2[10];


struct St {
  struct T st;
}s,u; 

struct St2 {
  struct St stt;
} st1,st2;


struct T1 {
  int i ; 
  int j ; 
  int q ;
}s3,s4;

/*@ 
  ensures M1: qed_ok: \forall integer i; s1.i == i && s1 == s2 ==> s2.i == i ;
  ensures M2: qed_ok: {s1 \with .i = (int)3}.i == 3 ;
  ensures M3: qed_ok: {s1 \with .i = (int)3}.j == s1.j ;
  ensures M4: qed_ok: {{s3 \with .i = (int)3} \with .j = (int) 4}.q == s3.q;
  ensures M5: qed_ok: {{s3 \with .i = (int)3} \with .j = (int) 4}.i == 3;
  ensures M6: qed_ok: {{s3 \with .i = (int)3} \with .j = (int) 4}.j == 4;
  ensures P1: qed_ok: {{s1 \with .i = s2.i}\with .j = s2.j} == s2  ;
  ensures P2: qed_ok: (\forall integer i; 0 <= i < 10 ==> t1[i] == t2[i]) ==> t1==t2;
  ensures P3: qed_ok: ({st1 \with .stt.st.i = (int)3}).stt.st.i == 3;
  ensures P4: qed_ok: ({{st1 \with .stt.st.i = (int)3}\with .stt.st.j = (int)4}).stt.st.i == 3;
  ensures P5: qed_ok: ({st1 \with .stt={ \with .st.i = (int)3 , .st.j = (int)4}}).stt.st.i == 3;

  ensures KP5: qed_ko: ({st1 \with .stt={ \with .st.i = (int)3 , .st.j = (int)4}}).stt.st.i == 4;
*/
void f (void) { return; }
