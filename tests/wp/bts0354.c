
union U { int a; int b; char c; } Gu;
union U Ggu; 
void fu2 () {
  Gu.a = 0;
  Gu.b = 1;
  //@ assert Gu.a == 1; 
  // this one is ok but need M3 (or maybe M2 ???)
  //@ assert Gu.a == 0; 
  // the last one is false !
  Gu.c = 2;
  Ggu.a=0;
  Ggu.b=1; 
  Ggu.c = 2;
  //  //@ assert SI_PAS_PADDING : Ggu==Gu; // <--- doit être prouvable 
}

int main (void) {return 0;}
