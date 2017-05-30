int case2(int arg) {
  {
    int a2[arg];
    if (arg)
      goto L; // goto L is invalid, see 6.8.6.1§1
  }

  {
    int b2[arg];
L: ;
    /* When execution lands to this point it executes  __fc_vla_free(b2)
     * without executing __fc_vla_alloc first. */
  }
  return 0;
}
