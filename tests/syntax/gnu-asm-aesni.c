/* run.config
   COMMENT: FILE_TITLE: AES-NI - Instructions en assembleur inline
   COMMENT: FILE_DESCRIPTION: Certains microprocesseurs sont dotés de fonctions cryptographiques directement accessibles à l'aide d'un jeu d'instructions étendu. C'est le cas par exemple de microprocesseurs Intel et AMD qui reconnaissent l'extension AES-NI.
   COMMENT: FILE_DESCRIPTION: L'exemple ci-dessous appelle une fonction extraite de la bibliothèque open source libgcrypt qui implémente des opérations cryptographiques faisant appel aux instructions AES-NI via de l'assembleur inline.
   COMMENT: FILE_DESCRIPTION: La syntaxe Extended Asm de GCC, utilisée ici, permet au programmeur de spécifier les variables C d'entrée et de sortie de son code assembleur. Bien que l'analyse du code assembleur ne soit pas dans le cadre de Frama-C ne gère, il peut se révéler intéressant de prendre en compte la spécification des entrées/sorties donnée par le programmeur.
   -------------------------
   COMMENT: TEST_TITLE: Chiffrement via AES-NI
   COMMENT: TEST_MAIN: encrypt_aesni
   COMMENT: TEST_DESCRIPTION: Un message de 64 octets est initialisé à une valeur précise. Le nombre de tours est fixé à 12 et la clé de chiffrement étendue est initialisée à une valeur abstraite. L'appel à la fonction do_aesni_enc effectue le chiffrement et place le résultat à l'adresse mémoire pointée par le paramètre b. On vérifie ensuite que les cases du tableau b ont bien été initialisées et que le tableau a n'a pas été modifié.
   OPT: -cpp-extra-args='-DUSE_AESNI' -print
   -------------------------
*/
#ifdef __FRAMAC__
#include <__fc_builtin.h>
#else
volatile int nondet;
#define Frama_C_make_unknown(a, n) do {                 \
    for (int __i = 0; i < n; i++) a[i] = nondet;        \
  } while (0)
#define Frama_C_dump_each()
#endif
#include <string.h>
typedef unsigned short int byte;

/*
  The following code is extracted from the LGPL project libgcrypt.
*/

#define MAXROUNDS		14

/* Helper macro to force alignment to 16 bytes.  */
#ifdef HAVE_GCC_ATTRIBUTE_ALIGNED
# define ATTR_ALIGNED_16  __attribute__ ((aligned (16)))
#else
# define ATTR_ALIGNED_16
#endif

typedef union {
    int a;
    short b;
    char c[1];
    long d;
#ifdef HAVE_U64_TYPEDEF
    u64 e;
#endif
    float f;
    double g;
} PROPERLY_ALIGNED_TYPE;

/* Our context object.  */
typedef struct
{
  /* The first fields are the keyschedule arrays.  This is so that
     they are aligned on a 16 byte boundary if using gcc.  This
     alignment is required for the AES-NI code and a good idea in any
     case.  The alignment is guaranteed due to the way cipher.c
     allocates the space for the context.  The PROPERLY_ALIGNED_TYPE
     hack is used to force a minimal alignment if not using gcc of if
     the alignment requirement is higher that 16 bytes.  */
  union
  {
    PROPERLY_ALIGNED_TYPE dummy;
    byte keyschedule[MAXROUNDS+1][4][4];
  } u1;
  union
  {
    PROPERLY_ALIGNED_TYPE dummy;
    byte keyschedule[MAXROUNDS+1][4][4];
  } u2;
  int rounds;                /* Key-length-dependent number of rounds.  */
  unsigned int decryption_prepared:1; /* The decryption key schedule is available.  */
#ifdef USE_AESNI
  unsigned int use_aesni:1;           /* AES-NI shall be used.  */
#endif /*USE_AESNI*/
} RIJNDAEL_context ATTR_ALIGNED_16;

/* Macros defining alias for the keyschedules.  */
#define keyschenc  u1.keyschedule
#define keyschdec  u2.keyschedule

static inline void
do_aesni_enc (const RIJNDAEL_context *ctx, unsigned char *b,
              const unsigned char *a)
{
#define aesenc_xmm1_xmm0      ".byte 0x66, 0x0f, 0x38, 0xdc, 0xc1\n\t"
#define aesenclast_xmm1_xmm0  ".byte 0x66, 0x0f, 0x38, 0xdd, 0xc1\n\t"
  /* Note: For now we relax the alignment requirement for A and B: It
     does not make much difference because in many case we would need
     to memcpy them to an extra buffer; using the movdqu is much faster
     that memcpy and movdqa.  For CFB we know that the IV is properly
     aligned but that is a special case.  We should better implement
     CFB direct in asm.  */
  asm volatile ("movdqu %[src], %%xmm0\n\t"     /* xmm0 := *a     */
                "movdqa (%[key]), %%xmm1\n\t"    /* xmm1 := key[0] */
                "pxor   %%xmm1, %%xmm0\n\t"     /* xmm0 ^= key[0] */
                "movdqa 0x10(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x20(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x30(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x40(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x50(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x60(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x70(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x80(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0x90(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0xa0(%[key]), %%xmm1\n\t"
                "cmpl $10, %[rounds]\n\t"
                "jz .Lenclast%=\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0xb0(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0xc0(%[key]), %%xmm1\n\t"
                "cmpl $12, %[rounds]\n\t"
                "jz .Lenclast%=\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0xd0(%[key]), %%xmm1\n\t"
                aesenc_xmm1_xmm0
                "movdqa 0xe0(%[key]), %%xmm1\n"

                ".Lenclast%=:\n\t"
                aesenclast_xmm1_xmm0
                "movdqu %%xmm0, %[dst]\n"
                : [dst] "=m" (*b)
                : [src] "m" (*a),
                  [key] "r" (ctx->keyschenc),
                  [rounds] "r" (ctx->rounds)
                : "cc", "memory");
#undef aesenc_xmm1_xmm0
#undef aesenclast_xmm1_xmm0
}


//-----main: encrypt_aesni
void encrypt_aesni(void)
{
  RIJNDAEL_context ctx;
  unsigned char b[64];
  unsigned char a_init[64] = {0x85,0x50,0x43,0xda, 0x06,0x99,0xd8,0x3b,
                              0x65,0xf7,0x1d,0xf7, 0x95,0xd4,0x34,0x5d,
                              0x6e,0x21,0x01,0xba, 0x2a,0xbd,0x7f,0xab,
                              0xa7,0x6d,0xe7,0xcd, 0x72,0xcf,0xce,0xa1,
                              0xa7,0x4a,0xb8,0x12, 0xef,0x2d,0x6b,0xd5,
                              0xdc,0x09,0xb9,0xdd, 0x09,0x27,0x7c,0x86,
                              0x35,0x60,0x99,0xea, 0x72,0xbb,0x93,0x9e,
                              0x9e,0x16,0x7b,0xd4, 0x8c,0x81,0x8a,0x53};
  unsigned char a[64];
  unsigned char test;
  int i,j,k;

  memcpy(a, a_init, 64 * sizeof(unsigned char));

  for(i=0; i<MAXROUNDS+1; i++)
    for(j=0; j<4; j++)
      for(k=0; k<4; k++){
        Frama_C_make_unknown((char*)&ctx.keyschenc[i][j][k], sizeof(byte));
      }
  ctx.rounds = 12;
  ctx.use_aesni = 1;

  do_aesni_enc(&ctx, b, a);

  test = 0x00U;
  for(i=0; i<64; i++)
    test ^= b[i];

  Frama_C_dump_each();
}
