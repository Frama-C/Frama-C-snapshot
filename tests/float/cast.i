
volatile double v;

/*@ requires \is_finite(d);
    requires (0.0 < d < 0x1p-1022);
    assigns \nothing; */
void assert_subnormal(double d);

/* Tests an edge case of backward_cast_float_to_double. */
void main () {
  float f = v;
  /* assert_subnormal reduces its double argument to a range of values without
     single precision values. The backward propagation of this reduction to the
     concrete argument [f] (which is a float) should lead to bottom. */
  assert_subnormal(f);
}
