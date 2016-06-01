int i,j,k,n,r;

void main() {
  int i;
  r=0;
  n=50;
  for (i=0; i<n ; i+=2) {
    Frama_C_show_each_F(i);
    r = i+r; }
}

void main1() {
  r=2;
  k=j?0:1;/*  ICI 1 */
  if (k) i = 1; else i = 4;
  
  if (i<2) i+=r;

}

void main2() {
  int i,j,k;
  r = 0;
  n = 0;
  for (i=0; i<(n+1) ; i++)
/*  ICI 2 */
    for (j=0; j <(n+1) ;j /*  ICI 3 */ ++)
      for (k=0; k <(n+1); k++)
        r = i+j+k+r+1;/*  ICI 4 */
}

/* Infinite non trivial loop */
void main3() {
  int i;
  r = 0;
  n = 0;
  for (i=0; i<(n+1) ; )
    r = i+1;
}

void main4(void)
{
  i = 0;
  j= 0;
  while(1)
    {
      k = i;
      if (i < j) break;
      k = r;
      }
  /* k does not depend on r when exiting this loop. */
}

int G;

void main5(void) 
{ int i___0 ;

  {
    G = -1;
    r = 0;
    n = 2;
    i___0 = 1;
    {
  while (1)
{
    while_0_continue: /* CIL Label */ ;
    if (i___0 < n+1 ) {
      G=0;
      r = r+1;
      i___0 += 1;
    } else {
      G=1;
      goto while_0_break;
      }
  }
 while_0_break: /* CIL Label */ ;
    }
  
  return;
    }
}


void main6() {
  int i, b;
  r=0;
  n=5;

  for (i=0; i<n ;) {
    r = i+r;
    if (b) i--;
    b = b&&b;
    if (r<b) i+=3; else  i+=6;
    }
}
