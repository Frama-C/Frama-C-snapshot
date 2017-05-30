/* run.config*
   OPT: -no-autoload-plugins -load-module inout,value -val @VALUECONFIG@ -journal-disable -machdep gcc_x86_32
   OPT: -no-autoload-plugins -load-module inout,value -val @VALUECONFIG@ -lib-entry -journal-disable -machdep gcc_x86_32
   OPT: -no-autoload-plugins -load-module inout,value -val @VALUECONFIG@ -lib-entry -journal-disable
*/

char T[];
char U[0];
char V[][2];
char W[][0];

char T1[] = {};
char U1[0] = {};
char V1[][2] = {};
char W1[][0] = {};
char W2[2][1];


char *pW;

void main(int c, char **v) {
  unsigned sT = sizeof(T); // error
  unsigned sU = sizeof(U);
  //@assert sU == 0;
  unsigned sV = sizeof(V); // error
  unsigned sW = sizeof(W); // error
  unsigned sT1 = sizeof(T1);
  //@assert sT1 == 0;
  unsigned sU1 = sizeof(U1);
  //@assert sU1 == 0;
  unsigned sV1 = sizeof(V1);
  //@assert sV1 == 0;
  unsigned sW1 = sizeof(W1);
  //@assert sW1 == 0;
  unsigned sW2 = sizeof(W2);
  T[2]= 3;
  if (c&1)  T[1] = T[3] +3;

  if (c&8)  V[2][1] = 3;    
  if (c&16) V[1][1] = V[3][1] +3;

  if (c&32) W[2][1] = 3;    
  if (c&64) W[1][1] = W[3][1] +3;
  if (c&128) pW = &W[0][1];
}
