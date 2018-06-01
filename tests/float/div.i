volatile v;

void main() {
  double d1, d2;
  if (v) {
    d1 = v ? 0 : 4;
    d2 = v ? 0 : 3;
    //@ assert d1 / d2 >= 0; // Does not hold, but we want to test the division itself. In the logic this is tricky
    //@ assert !\is_finite((double)(d1 / d2));
    //@ assert \is_finite((double)(d1 / d2));
    //@ assert \false;
  }
}
