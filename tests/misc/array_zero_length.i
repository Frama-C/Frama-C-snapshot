/* run.config
   OPT: -val -journal-disable
   OPT: -val -lib-entry -journal-disable
*/

char T[];
char U[0];
char V[][2];
char W[][0];

char *pW;

void main(int c, char **v) {
  T[2]= 3;
  if (c&1)  T[1] = T[3] +3;

  if (c&2)  U[2] = 3;
  if (c&4)  U[1] = U[3] +3;

  if (c&8)  V[2][1] = 3;    
  if (c&16) V[1][1] = V[3][1] +3;

  if (c&32) W[2][1] = 3;    
  if (c&64) W[1][1] = W[3][1] +3;
  if (c&128) pW = &W[0][1];
}
