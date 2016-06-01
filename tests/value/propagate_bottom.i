volatile int v;

void main() {
  if (v) {
    int x1 = 1 / 0;
  }
  if (v) {
    int x2 = (long)(1 / 0);
  }
  if (v) {
    int x3 = (float)(1 / 0);
  }
  if (v) {
    int x4 = 1 / 0 + 4;
  }
  if (v) {
    int x5 = -(1 / 0);
  }
  if (v) {
    int x6 = ~(1 / 0);
  }


  if (v) {
    double y1 = 1. / 0;
  }
  if (v) {
    double y2 = (int)(1. / 0);
  }
  if (v) {
    double y3 = (float)(1. / 0);
  }
  if (v) {
    double y4 = 1. / 0. + 4.;
  }
  if (v) {
    double y5 = -(1. / 0.);
  }
  if (v) {
    double y6 = !(1. / 0.);
  }
}
