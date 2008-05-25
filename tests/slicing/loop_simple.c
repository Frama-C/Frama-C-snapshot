/* run.config
   OPT: -deps -slice-print -slice-return main
*/
int main() {
  int a,c; volatile int b = 0;
  a = 1;

  while (1) break ;
 
  for (c=0; c <= 5; c++) ;

  for (c=0; c <= 5; c++) { a = 2; }

  if (b) goto L;

  for (c=0; c <= 5; ) { 
    a+=2 ;
 L: a+=3;
    goto H;
    c++;
    }

  a++;
  

 H:
  if (a) c++;
  return a;
}
