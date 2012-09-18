void printf(const char*c, ...);

char * X= "NULL";
void MC3_COM_ARRET_68040(int i , char *c, int j) {
  X = c;
}

static void MC3_ANALYSER_CPTRENDU(int n , char num_station , 
                                  short cptrendu )
{

  MC3_COM_ARRET_68040(160, (char *)"mc3_mdb_emi_act.c", 506);

}


int G,H,K,L,i,b;
unsigned int I;
signed char c,d,e;
unsigned char uc,ud;
long long ll,gg;
unsigned long long ull, ugg;


int any_int(void)
{ volatile int i = 0;
 int j;
 j =(int*)0 + i;
 return (j/4);};

void all_cast() {
  G=258;
  H=any_int();

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

int main(void)
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


void f() {
  G=258;
  if (H>=258) {if (H<=268) {G = H;};};
  I = (unsigned char)G; // = 2..12

}

//int main(){all_cast();}
