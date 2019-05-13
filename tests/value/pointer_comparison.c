/* run.config*
   STDOPT: +" -load-module report -report-print-properties -eva-warn-undefined-pointer-comparison none -eva-msg-key pointer-comparison -then -report -then -eva-warn-undefined-pointer-comparison pointer -then -report -then -eva-warn-undefined-pointer-comparison all -then -report"
   STDOPT: +" -load-module report -report-print-properties -undefined-pointer-comparison-propagate-all -eva-warn-undefined-pointer-comparison none -eva-msg-key pointer-comparison -then -report -then -eva-warn-undefined-pointer-comparison pointer -then -report -then -eva-warn-undefined-pointer-comparison all -then -report"
*/
int x,y,*p;
int main(){
  p = &x;
  if (p++ != &y) Frama_C_show_each_1t(p);
  else Frama_C_show_each_1e(p);
  if (p++ != &y) Frama_C_show_each_2(p);
  else Frama_C_show_each_2e(p);
  if (p++ != &y) Frama_C_show_each_3(p);
  else Frama_C_show_each_3e(p);
  if (p++ != &y) Frama_C_show_each_4(p);
  else Frama_C_show_each_4e(p);
  if ((int)p != (int)&y) Frama_C_show_each_5((int)p);
  else Frama_C_show_each_5e((int)p);
  while (p++ != &y) Frama_C_show_each_6(p);
  return 0;
}
