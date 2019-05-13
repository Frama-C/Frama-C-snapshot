/* run.config
   STDOPT: +"-sparecode-analysis -main main_top"
   STDOPT: +"-sparecode-analysis -main main_call_top"
   STDOPT: +"-sparecode-analysis -main main_top_not_used"
*/

void print (int x);

int not_used_in_main_top (int x) {
  print (x);
  return x+2;
}


int f (int a) {
  return a+1;
}

int main_top (int nb, ...) {
  int x = 3;
  int y = f (2);
  return x;
}

int main_call_top (void) {
  int x = main_top (2, 0, 1);
  x = not_used_in_main_top (x);
  return x;
}

int main_top_not_used (void) {
  int a = main_top (2, 0, 1);
  int x = f (2);
  return x;
}


