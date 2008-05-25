int X,c;
void main () {
  if (c) X=0;
    X=3;


  while (c) { X=0;}
  X=1;


  if (c) while(c) X=0;
  X=1;


  if (c) do {X=0; } while(c);
  X=1;
  //@ assert X == 1;

}
