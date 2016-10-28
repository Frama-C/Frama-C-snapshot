int t[100]={1,1};


void main(int arg)
{
  int G=55;
  int i;
  for (i=0; i<=arg; i++)
    G += t[i];
  Frama_C_show_each(G,arg);
}

