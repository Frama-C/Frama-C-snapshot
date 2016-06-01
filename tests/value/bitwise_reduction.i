/* run.config*
   STDOPT: +"-big-ints-hex 255"
*/

volatile v;

void main1() {
  int t[2];
  t[0] = t[1] = v;
  //@ assert t[0] == 0xFF00 || t[0] == 0xF000;
  //@ assert t[1] == 0 || t[1] == 1 || t[1] == 0x3000 || t[1] == 0x3001 || t[1] == 0x3200 || t[1] == 0x3201 || t[1] == 0xF000;

  int i = v;
  if ((t[i] & 0xFF00) == 0x0) {
    Frama_C_show_each_then1(i, t[i]);
  } else {
    Frama_C_show_each_else1(i, t[i]);
    if ((t[i] & 0x3000) == 0x3000)
      Frama_C_show_each_else_then1(i, t[i]); // imprecise
  }

  int *p = &t[v];
  if ((*p & 0xFF00) == 0x0)  {
    Frama_C_show_each_then2(p, *p);
  } else {
    Frama_C_show_each_else2(p, *p);
    if ((t[i] & 0x3000) == 0x3000)
      Frama_C_show_each_else_then2(i, t[i]); // imprecise
  }
}

void main() {
  main1();
}
