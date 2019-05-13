/* run.config
  STDOPT: #"-eva -pdg -out -input -deps -calldeps -pdg"
*/

struct T1 {
   int M1 ;
   int M2 ;
};
struct T2 {
   struct T1 *M3 ;
   struct T1 *M4 ;
};
typedef struct T2 T3;
struct T4 {
   struct T1 *M5 ;
};
typedef struct T4 T5;
struct T1 G1;
struct T1 G2;
struct T1 G3;
T5 const G4 = {.M5 = & G1};
void F1(T3 * const f1)
{
  (f1->M4)->M2 = (f1->M3)->M2;
  return;
}

int F2(T5 * const f2)
{
  int V1 = 1;
  (f2->M5)->M1 = 0;
  return V1;
}

T3 const G5 = {.M3 = & G2, .M4 = & G3};
int main(void)
{
  int V2;
  F2((T5 *)(& G4));
  G2 = G1;
  F1((T3 *)(& G5));
  V2 = 0;
  return V2;
}


