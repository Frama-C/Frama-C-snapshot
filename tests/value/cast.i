/* run.config*
   STDOPT:
*/

/* These declarations are useful for the plugin 'variadic' */
//@ axiomatic String { predicate valid_read_string{L}(char *s); } // Beware that this predicate is not recognized by Eva as coming from the libc, and is thus not evaluated. The proper solution would be to enclose it inside __PUSH_FC_STDLIB pragmas.
struct __fc_FILE {
  unsigned int __fc_FILE_id;
  unsigned int __fc_FILE_data;
};
typedef struct __fc_FILE FILE;
extern FILE * __fc_stdout;
//@ assigns \result, __fc_stdout->__fc_FILE_data;
int printf(const char * restrict format, ...);


int G,H,K,L,i,b;
unsigned int I;
signed char c,d,e;
unsigned char uc,ud;
long long ll,gg;
unsigned long long ull, ugg;


int any_int_4(void)
{
  volatile int i = 0;
  return (i/4);
};

void main1() {
  G=258;
  H=any_int_4();

 if (H>=258) {if (H<=268) {G = H;};};
  G = G&128?0xFFFFFF00|(G&255):(G&255);
  G = (signed char)G; // 2..12

  K=-10;
  if (H>=-10) {if (H<=20) {K = H;};};
  c = (signed char)(K); // -10..20
  uc = c ; // (signed char)(K); // 0..255

  K = c;
  I = (unsigned int)(signed char)(int)(-1);
  printf("%ud\n",I);

  L=-19;
  if (H>=-2000) {if (H<=-10) {L = H;}}
  d = L; // top
  ull=1;
  L=0;
  if (H>=-2000) {if (H<=1) {L = 2*H;}}
  e = L; // top

}

void main2(void)
{
  int min = 130;
  int max = 135;
  int i;
  int G;
  for (i=min; i<=max; i++)
    {
      G = i&128?0xFFFFFF00|(i&255):(i&255);
      printf("cast:%d formule:%d\n",(int)(signed char) i,G);
      }
  printf("usc: %ud",(unsigned int)(signed char)(int)(-1));
}

void main() {
  main1();
  main2();
}
