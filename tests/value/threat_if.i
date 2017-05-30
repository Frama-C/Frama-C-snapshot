int *p;
int l,m;
int i;
int X=-992;



int main(int i) {
  int G = 258+128;
  signed char c;


  if(i==0) p = &l;
  if(i==0) *p = 1;

  c = (signed char)G; // -126
  G = c;
  Frama_C_show_each(G);
  return G;

  for (i=-1000+8; i<2008; i+=100)
    X  = i;

}
