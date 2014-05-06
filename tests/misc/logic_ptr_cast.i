/* run.config
   OPT: -val -print -journal-disable -no-results
*/
int *p;
int t[90];

main(){
  p = (int*) (((unsigned long)t + 7) & ~7UL);
  /*@ assert p == (int*)t || p == (int*)((char*)t+1) || 
    p == (int*)((char*)t+2) || p == (int*)((char*)t+3) ||
    p == (int*)((char*)t+4) || p == (int*)((char*)t+5) ||
    p == (int*)((char*)t+6) || p == (int*)((char*)t+7) 
; */
  Frama_C_show_each(p);
}
