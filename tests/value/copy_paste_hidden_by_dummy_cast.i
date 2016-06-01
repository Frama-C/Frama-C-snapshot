typedef unsigned short T_WORD16;
typedef unsigned int T_WORD32;
typedef short T_INT16;
typedef float T_FLOAT;

struct S {
   T_INT16 a ;
   T_WORD32 b ;
};
typedef struct S T_ERREUR_ANO;

T_ERREUR_ANO const  A4O1_Ci_sNO_ERREUR_ANO = {0, 0};

void main () {
  struct S Rl_sErreurAno ;
  Rl_sErreurAno = A4O1_Ci_sNO_ERREUR_ANO;
  
}

  
