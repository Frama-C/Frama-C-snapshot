int Gx,r,x;
int main(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  if (x <= 0) {}
  else goto fin;
  r = x;
 fin:
  return r;
}

int main1(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  if (x <= 0) {goto fin;}
  else r=x;
  r = x;
 fin:
  return r;
}


int main2(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  Gx = -2;
  if (x <= 0) {Gx = x;}
  else goto fin;
  r = x;
 fin:
  return r;
}


int main3(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  Gx = -2;
  if (x <= 0) {goto fin;}
  r = x;
 fin:
  return r;
}


int main4(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  Gx = -2;
  if (x <= 0) {Gx=5;}
  r = x;
 fin:
  return r;
}

int main5(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  if (x <= 0) {}
  else {Gx=5;}
  r = x;
 fin:
  return r;
}

int main6(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  if (x <= 0) {Gx=5;}
  else r=x;
  r = x;
 fin:
  return r;
}


int main7(void) {
  r = -1;
  x = Gx ? 0 : 1 ;
  Gx = -2;
  if (x <= 0) {}
  else {}
  r = x;
 fin:
  return r;
}


