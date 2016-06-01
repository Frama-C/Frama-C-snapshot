int c[5][10];
void main() {
  char * d;
  d = (char*)c;
  d[2] = 'z';
  ((char*)c[2])[1] = (char)'y';
  ((char*)c)[1] = (char)'y';
//  ((long long*)c[2])[2] = (char)'y';
// ((char**)c)[1][0] = (char)'y'; // seg fault !

  *c[0] = (int)'x';
  
  int l;
  int *pl = &l;
  *pl = 0;
  *((char*)pl)= 2;
//  l = l & 0b11111111000000000;
}
