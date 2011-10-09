/* bug #5877 on gforge */

 int *t = 0;

  int main(void)
  {
    /* should lead to an alarm. */
    *t == 42;

    return 0;
  }
