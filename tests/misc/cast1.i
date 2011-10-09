short si=0;
int i=0;
int S[5]={1};
int I[5]={1};

void main(void) {
  for (si=0;si<2;si++) S[i]=2;
  for (i=0;i<2;i++) I[i]=2;
}


void with_if ()
{
  long x;
  short si=x?0:2;

  if ((unsigned short)si < 2) x=si; else x=3;
  
}

void with_if2 ()
{
  long x;
  short si=x?0:4;

  if ((signed short)si < 2) x=si; else x=3;
  
}
