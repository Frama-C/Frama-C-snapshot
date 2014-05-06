int G;
int x,y;

int f() {
  return G?0:(-51);
}

int i,t[]={ 1, 2, 3, 4, 5, 6, 7, 8 },(*p)[8],z, R;

int main(int argc, char**argv)
{
  int r;
  int inRet = (0);
  char c = **argv;
  short s = argc;
  if(c < 0)
    x = c;
  if(s >= -10)
    y = s;
  r = f() ;
  if (r != (0))
    {
      inRet = (-51);

    }
    
  p = t;
  i = argc>=1?argc<=3?argc:1:1;	
  if ((*p)[i]==3) z = i;

  i = argc>=1?argc<=7?argc:1:1;	
  if (t[i]==4) R = i;

  unsigned u = unknf();
  if (u>=8) u = 8;	
  if (u!=3)
    Frama_C_show_each_2(u);
  Frama_C_show_each_3(u);

  unsigned v = 2 * u;
  if (v!=3)
    Frama_C_show_each_4(v);
  Frama_C_show_each_5(v);
  if (v!=10)
    Frama_C_show_each_6(v);
  Frama_C_show_each_7(v);

  return inRet;

}
