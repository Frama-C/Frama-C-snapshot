typedef enum { Max, Min } kind;
int extremum (kind k, int x, int y) {
  return ((k == Max ? x > y : x < y) ? x: y);
}
/*@ requires k == Max || k == Min;
    assigns \nothing;
    ensures \result == x || \result == y;
    behavior is_max:
      assumes k == Max;
      ensures \result >= x && \result >= y;
    behavior is_min:
      assumes k == Min;
      ensures \result <= x && \result <= y;
    complete behaviors is_max, is_min;
    disjoint behaviors is_max, is_min;
    complete behaviors;
    disjoint behaviors;
*/
int extremum (kind k, int x, int y);
