/* run.config*
STDOPT: +"-kernel-msg-key printer:logic-coercions" +"-kernel-warn-key acsl-float-compare=active"
*/

/*@
  assigns \result \from \nothing;
  ensures \le_double(\result, (double)0.0);
  ensures \ge_double(\result, (double)-1.0);
  ensures \lt_double(\result, (double)1.0);
  ensures \gt_double(\result, (double)-2.0);
  ensures \ne_double(\result, (double)-0.5);
  ensures \eq_double(\result, (double)-1.0);
*/
double minus_one(void);


/*@
  assigns \result \from \nothing;
  ensures \le_float(\result, (float)0.0);
  ensures \ge_float(\result, (float)-1.0);
  ensures \lt_float(\result, (float)1.0);
  ensures \gt_float(\result, (float)-2.0);
  ensures \ne_float(\result, (float)-0.5);
  ensures \eq_float(\result, (float)-1.0);
*/
float minus_onef(void);

/*@ requires x <= y;
    assigns \result \from x,y;
    ensures x <= \result <= y;
*/
float test(float x, float y);

void main() {
  double mone = minus_one();
  float monef = minus_onef();
}

