int T[2];
int**ppp;
int pp[2];
int p;
void main(int c1, int c2, int c3) {
  pp [c1] = (int) &T[c1];
  if (c2) ppp = &pp; else ppp = &T[-1];
  **ppp=9;
 
  if (c2>=0 && c2<=5) T[c2] = 4;
  
}
