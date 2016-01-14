
int x  ;
int t[13]  ;
extern void Frama_C_show_each_F(int  ) ;
void main(void)
{ int i ;

  {
    i = 0; // (&x+i)-&x;
  {
  {
  {
  while (1) {
    if (i <= 12) {

    } else {
      goto L;
    }
    Frama_C_show_each_F(i);
    i += 1;
  }
  }
  }
 L: ;
  }

  return;
}
}
