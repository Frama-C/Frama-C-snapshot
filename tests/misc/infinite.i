int G; void pause(int);
void main () {
  int count;
  G++;
  if (G==1)
    while(1) {
      G++;
      if(G==5) break;
      pause(3);
      G--;
      };
  G=0;
  return;
}
