//@ assigns \result \from \nothing;
extern int any_int(void);

void g() {
  int t;
  unsigned int G;
  t = any_int();
  G = t;
  t = t+1;

}

unsigned char G = (unsigned char)(-1);

void h() {
  G = -255;
}

void passcast() {
  int i = (char)(any_int());

  if ((char) i <= 100) {
    Frama_C_show_each(i);
  } else while(1);
}

void main() {
  g();
  h();
  passcast();
}
