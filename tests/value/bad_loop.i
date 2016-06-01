volatile int c;
void main () {
  int x; volatile int d=0,e=0;
  x = 2;
  
  while(1) {
 L1: if (c) goto FIN;
 if (d) goto L2;
 x = 0;
    }
  
  while (1) {
 L2: if (c) break;
 if (e) goto L1;
 x=1;
    }

 FIN: ;
}
