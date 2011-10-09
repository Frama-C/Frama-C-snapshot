/* run.config
   COMMENT: running this test fails on purpose
   OPT: -simplify-cfg -typecheck
 */

/* small test cases to verify that break is accepted in while and switch */
int f () {

  while (1) {
    if (0) {
      while (1) break;
    }
    switch (3) {
    case 0: return 5;
    default:
      if (1) break; else break;
    }
    break;
  }

  return 0;
}

/* should abort with an error at type-checking */
int main (void) {
  break;
  return 0;
}
