int T[2];
int**ppp;
int pp[2];
int p;
void main(int c) {
  pp [c] = &T[c];
  if (c) ppp = &pp; else ppp = &T[-1];
  **ppp=9;
 
  if (c>=0 && c<=5) T[c] = 4;
  
}
