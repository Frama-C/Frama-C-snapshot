/* run.config
   OPT: -wp-no-let
   OPT: -main main_ko -wp-no-let -wp-no-init-const
*/
/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -main main_ko -wp-par 1 -wp-prop qed_ko -wp-steps 50 -wp-no-init-const 
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: partial and complete initialization of value                 --- */
/* -------------------------------------------------------------------------- */

struct S { int a; int b;};

struct S s = {2};

int t[2] = {1}; 
int t1[9+1] = {[5 ... 6]=2, [0 ... 3 ]=1  };

struct St {int tab[9+1];};
struct St st = {{1,2,3,4}};

struct Sc {int a; int b[2+1]; int c;}; 

struct Sc sc0 = {1,{2,3,4},5};
struct Sc sc1 = {1,2,3,4,5};
struct Sc sc2 = {1,{2,3},4};
struct Sc sc3 = {1,2,3,4};

struct Sc sq0 = {2,{2,2},2};
struct Sc sq1 = {.b={2,2}};

unsigned char tab[32];

union U {
  short t[4];
  short a;
  long long b;
} u = {.a=-1 };

/*@ requires qed_ok: Struct_Simple_a: s.a == 2 ;
    requires qed_ok: Struct_Simple_b: s.b == 0 ;
    requires qed_ok: Simple_Array_0 : t[0] == 1 ; 
    requires qed_ok: Simple_Array_1 : t[1] == 0 ; 
    requires qed_ok: With_Array_Struct_5 : st.tab[5] == 0 ; 
    requires qed_ok: With_Array_Struct_3 : st.tab[3] == 4 ; 
    requires qed_ok: Sc_eq : sc1 == sc0;
    requires qed_ok: Sc_t : sc2.b[2] == 0 ; 
    requires qed_ok: Sc_t : sc3.b[2] == 4 ;
    requires qed_ok: Sc_c_2 : sc2.c == 4; 
    requires qed_ok: Sc_c_3 : sc3.c == 0;
    requires qed_ok: Tab_no_init : tab[5] == 0 ; 
    requires qed_ok: Tab_todo : \forall int i; 0 <= i <= 31 ==> tab[i] <= 255;
    requires qed_ok: sq0.b[1]==2;
    requires qed_ok: sq0.b[2]==0;
    requires qed_ok: \forall integer i; 0 <= i <= 3 ==> t1[i] == 1;
    requires qed_ok: todo: t1[4] == 0;
    requires qed_ok: \forall integer i; 5 <  i <= 6 ==> t1[i] == 2;
    requires qed_ok: \forall integer i; 6 <  i <= 9 ==> t1[i] == 0;
    requires qed_ok: direct_init_union: u.a == -1;
 */
void main (int a){return;};

/*@ requires qed_ko: Sc_eq_ko : sc2 == sc3; 
    requires qed_ko: Sc_t : sc3.b[2] == 3 ;
    requires qed_ko: Sc_c_2 : sc2.c == 2; 
    requires qed_ko: Tab_no_init : tab[5] == 1 ; 
    requires qed_ko: With_Array_Struct_3 : st.tab[3] == 3 ; 
    requires qed_ko: Simple_Array_1 : t[1] == 1 ; 
    requires qed_ko: T1_6: t1[6] == 0;
    requires qed_ko: indirect_init_union_b: u.b == 0;
    requires qed_ko: indirect_init_union_t: u.t[0] == 0;
 */
void main_ko (void){return;}

const int ta1[5] = { [2]=1,[4]=1 };
/*@ ensures qed_ok: ta1[0]==ta1[1] && ta1[1]==ta1[3];
  @ ensures qed_ko: ta1[4]==0;
  @ ensures qed_ko: ta1[3]==1; */
void fa1(void) {return ;}

const int ta2[5] = { [2 ... 3]=1 };
/*@ ensures qed_ok: ta2[0]==ta2[1] && ta2[1]==ta2[4];
  @ ensures qed_ko: ta2[4]==1;
  @ ensures qed_ko: ta2[1]==1; */
void fa2(void) {return ;}

const int ta3[5] = { [1]=1, [3]=1};
/*@ ensures qed_ok: ta3[0]==ta3[2] && ta1[2]==ta1[4];
  @ ensures qed_ko: ta3[0]==1;
  @ ensures qed_ko: ta3[2]==1;
  @ ensures qed_ko: ta2[4]==1; */
void fa3(void) {return ;}

const struct { int a, b, c; } ts1[4] = { [2].a=1, [2].b=1 };
/*@ ensures qed_ok: ts1[0]==ts1[1] && ts1[1]==ts1[3] && ts1[2].a ==ts1[2].b;
  @ ensures qed_ko: ts1[2].c==1;
  @ ensures qed_ko: ts1[0].a==1;*/
void fs1(void) {return ;}
