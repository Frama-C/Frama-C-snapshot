/*@ requires x >= 0; */
int g(int x);

void main() {
  int c, x;
  if (c) { {
      if (c) { x = 1; };}
  } else {
    if (c) { if (c) x = 1;   } else x = 2;
  }
}

int f(int a, int b, int c, int d) {
  int ret;
  ret=0;
  if (a) {
    if (b) {ret=1;}
    else if (c) {ret=2;}
  }
  else {if (d) {ret=4;}}

  if (a) {{{{/*@ assert ret >= 0; */ ret = 5; }}}} else ret = 6;
  if (a) g(a); else g(a); // double GUI-bullets
  return ret;
} 
