int i,j,k,l,n,r;
struct T {int a,b;} G[5]={0};

void g(const int b) ;

void main() {
  n=5;
  for (i=0; i<n ; i++)
    {
    G[i].a = i+55;
    G[i].b = i+57;
    }
}


static char Reg5;
void g(const int b) {
  Reg5 = Reg5 & (~(0x80));
}

void main1(void) 
{ 

  {
  n = 1;
  i = 0;
  {
  {
  {


  if (i < n) {

  } else {
    goto while_0_break;
  }
  G[i].a = 1;
  i += 1;


  if (i < n) {

  } else {
    goto while_0_break;
  }
  G[i].a = 1;
  i += 1;
  while (1) {
    while_1_continue: /* CIL Label */ ;
    while_0_continue: /* CIL Label */ ;
    if (i < n) {

    } else {
      goto while_0_break;
    }
    G[i].a = 1;
    i += 1;
  }
  }
  while_1_break: /* CIL Label */ ;
  }
  while_0_break: /* CIL Label */ ;
  }

  return;
}
}


void main2(void)
{
  main();
 l1:
  main1();
 l2:
  g(0);
  if(i) goto l1;
  k=0;
  if(j) goto l2;
  l=0;

}
  
