/* run.config
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -journal-disable -undefined-pointer-comparison-propagate-all
*/
int x,y,*p;
main(){
  p = &x;
  if (p++ != &y) Frama_C_show_each_1t(p);
  else Frama_C_show_each_1e(p);
  if (p++ != &y) Frama_C_show_each_2(p);
  else Frama_C_show_each_2e(p);
  if (p++ != &y) Frama_C_show_each_3(p);
  else Frama_C_show_each_3e(p);
  if (p++ != &y) Frama_C_show_each_4(p);
  else Frama_C_show_each_4e(p);
  while (p++ != &y) Frama_C_show_each_5(p);
}
