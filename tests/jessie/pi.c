
/*@ axiomatic Pi {
  @   logic integer nth_of_pi(integer n);
  @   axiom range:
  @     \forall integer n; 0 <= nth_of_pi(n) <= 9;
  @   axiom infinite:
  @     \forall integer k, i;
  @        0 <= i <= 9 ==> \exists integer n; k <= n && nth_of_pi(n) == i;
  @ }
  @*/

