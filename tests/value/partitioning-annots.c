/* run.config*
   GCC:
   STDOPT: #"-main test_unroll -eva-default-loop-unroll 10"
   STDOPT: #"-main test_split"
   STDOPT: +"-main test_split -eva-partition-value k"
   STDOPT: #"-main test_loop_split -eva-partition-history 1"
   STDOPT: #"-main test_history -eva-partition-history 0"
   STDOPT: #"-main test_history -eva-partition-history 1"
   */

#include "__fc_builtin.h"

#define N 10


void test_unroll()
{
  int a[N], b[N], c[N], d[2*N], e[N];

  // The inner loop needs to be unrolled to allow strong updates
  // The outer loops doesn't need to be unrolled

  //@ loop unroll N;
  for (int i = 0; i < N; i++) {
    //@ loop unroll 1;
    for (int j = 0; j < N; j++) {
      a[i] = 42;
    }
  }

  // This time the outer loop needs unrolling but not the inner loop

  //@ loop unroll 1;
  for (int i = 0; i < N; i++) {
    //@ loop unroll N;
    for (int j = 0; j < N; j++) {
      b[j] = 42;
    }
  }

  // At the end, we must have both arrays a and b to be fully initialized at 42

  // Small loops can be unrolled without giving an unroll parameter
  //@ loop unroll N;
  for (int i = 0 ; i < N ; i++)
    c[i] = 0;

  // Longer loops won't be completely unrolled when not giving a parameter
  //@ loop unroll N;
  for (int i = 0 ; i < 2*N ; i++)
    d[i] = 0;

  // Variable unroll limits can be specified as long as they evaluate as
  // a singleton in each state
  //@ loop unroll N;
  for (int i = 0 ; i < N ; i++) {
    e[i] = 1;
    //@ loop unroll i-1;
    for (int j = i - 1 ; j > 0 ; j--) {
      e[j] += e[j-1];
    }
  }
}

int k;

void test_split()
{
  int i = Frama_C_interval(0,1);
  int j = Frama_C_interval(0,2);

  // The splits are done on i and j and undone in the same order
  // If global dynamic split is done on k, since it is equaly to i, merge i will
  // have no effects.

  Frama_C_show_each_before_first_split(i,j,k);
  //@ split i;
  k = i;
  Frama_C_show_each_before_second_split(i,j,k);
  //@ split j;
  Frama_C_show_each_before_first_merge(i,j,k);
  //@ merge i;
  Frama_C_show_each_before_second_merge(i,j,k);
  //@ merge j;
  Frama_C_show_each_end(i,j,k);
}

void test_loop_split()
{
  int A[N];
  int i;

  // In this example we can split on the value of the loop index in order to
  // keep the relation between i and the value A[i] found in the array to be
  // equal to 42.
  // However, since the split is not dynamic, an history partitioning must be
  // added to distinguish between the two states that share i = 9 : those who
  // left the loop at the break point and those who left after the loop test.

  // Init a random array
  for (i = 0 ; i < N ; i ++)
  {
    A[i] = Frama_C_interval(0,100);
  }

  // Search for some value
  for (i = 0 ; i < N ; i++)
  {
    //@ split i;
    if (A[i] == 42)
      break;
  }

  if (i < N) {
    Frama_C_show_each(i, A[i]);
    //@ assert A[i] == 42;
  }
  else {
    Frama_C_show_each("Value 42 not found");
  }
}

void test_history()
{
  int i = Frama_C_interval(0,1);
  int j = 0, k = 1;

  if (i)
    j = 1;

  Frama_C_show_each(i, j);

  if (i)
    k = k / j;
}

void main(void)
{
  test_unroll();
  test_split();
  test_loop_split();
}

