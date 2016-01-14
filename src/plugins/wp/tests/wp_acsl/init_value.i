/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -wp -wp-par 1 -wp-prop qed_ko -wp-timeout 5
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: partial and complete initialization of value                 --- */
/* -------------------------------------------------------------------------- */

struct S { int a; int b;}; 

struct S s = {2};

int t[2] = {1}; 

struct St {int tab[10];};
struct St st = {{1,2,3,4}};

struct Sc {int a; int b[3]; int c;}; 

struct Sc sc0 = {1,{2,3,4},5};
struct Sc sc1 = {1,2,3,4,5};
struct Sc sc2 = {1,{2,3},4}; 
struct Sc sc3 = {1,2,3,4};

unsigned char tab[32];

int u [];

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

    requires qed_ko: Sc_eq_ko : sc2 == sc3; 
    requires qed_ko: Sc_t : sc3.b[2] == 3 ;
    requires qed_ko: Sc_c_2 : sc2.c == 2; 
    requires qed_ko: Tab_no_init : tab[5] == 1 ; 
    requires qed_ko: With_Array_Struct_3 : st.tab[3] == 3 ; 
    requires qed_ko: Simple_Array_1 : t[1] == 1 ; 
 */
void main (void){return;}




