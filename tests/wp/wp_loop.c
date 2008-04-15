int X,c;

void infinite (void) {
  int s = 0;
  if (c) {
    while (1)
      s++;
  }
  //@ assert c == 0;
}

void loops () {
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

void main (void) {
  loops ();
  infinite ();
}
