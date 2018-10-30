void f(int);

int X;

void g(int x)
{
  X = x + 1;
}

void (*p)(int);

int main(int c){
  p = c&1? f : g;
  if (c&2) 
    f(**(int**)0); 
  else if (c&4)
    g(**(int**)0);
  else
    p(**(int**)0);
  return X;
}
