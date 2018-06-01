// checks that division with longs in constfold rounds towards zero
void main() {
  int t1 = (int)(-1/2L) ? 0 : 1;
  int t2 = (int)(-1/2LL) ? 0 : 1;
}
