int case2(int arg) {
  {
    int a2[arg];
    if (arg)
      goto L; // goto L is valid.
  }

  {
    L: ; // although we are jumping in a block where a vla is defined,
         // the target of the jump dominates the declaration of the vla
         // so that it is outside of the scope of the vla and the
         // program is correctly defined

    int b2[arg];
  }
  return 0;
}
