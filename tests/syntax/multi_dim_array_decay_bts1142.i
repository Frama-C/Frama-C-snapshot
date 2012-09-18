struct P {
  int val[2][2];
};

int main() {
  struct P* pp;
  struct P p;
  pp = &p;
  *(pp->val);
  *(p.val);
}
