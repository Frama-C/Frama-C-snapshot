/* run.config
   COMMENT: test that local variables within a scope are removed from tracking
            even if the execution exists the scope early via goto, break or
            continue.
*/

/* Simple test case from BTS (#1740) */
int goto_bts() {
  int *p;
  {
    int a = 0;
    p = &a;
    /*@ assert \valid(p); */
    goto L;
  }

L:
  /*@ assert ! \valid(p); */
  return 0;
}

/* Make sure that when `goto` jumps over several scopes all locals
 * from those scopes are removed. */
int goto_valid() {
  int a = 9;
  int *p, *q, *r;
  {
    int a1 = 0;
    p = &a1;
    {
      int a2 = 0;
      q = &a2;
      {
        int a3 = 0;
        r = &a3;

        goto FIRST;
        /* Dead code */
        p = (void*)0;
        r = q = &a;
      }
    }
FIRST:
    /* At this point `a1` is still in scope, while `a2` and `a3` are not, thus
     * `q` and `r` become invalid, whereas `p` is still valid. */
    /*@ assert   \valid(p); */
    /*@ assert ! \valid(q); */
    /*@ assert ! \valid(r); */
    /* The following `goto` invalidates `p`. */
    goto SECOND;
    /* Dead code */
    p = r = q = &a;
  }

SECOND:
  /*@ assert ! \valid(p); */
  /*@ assert ! \valid(q); */
  /*@ assert ! \valid(r); */
  return 0;
}

/* Make sure that when a break statement is executed within a switch statement
 * then all local variables declared within that switch are removed. */
int switch_valid() {
  int i = 1;
  int *p, *q, *s;
  {
    s = &i;
    switch(i) {
      default: {
        int a1 = 0;
        p = &a1;
        {
          int a2 = 0;
          q = &a2;
          /*@ assert \valid(p); */
          /*@ assert \valid(q); */
          /*@ assert \valid(s); */
          break;
        }
        /* Dead code */
        p = q = &i;
        s = (void*)0;
      }
    }
    /* Break invalidates `p` and `q` but `s` is still in scope. */
    /*@ assert ! \valid(q); */
    /*@ assert ! \valid(p); */
    /*@ assert \valid(s); */
  }
  return 0;
}

/* Same as switch_valid but for a break statement in a body of a loop. */
int while_valid() {
  int *p, *q, *r;
  int i = 5;
  {
    int a0 = 0;
    r = &a0;
    while (--i) {
      {
        int a1 = 0;
        p = &a1;
        {
          int a2 = 0;
          q = &a2;
          /*@ assert \valid(p); */
          /*@ assert \valid(q); */
          /*@ assert \valid(r); */
          if (!i)
            break;
        }
      }
    }
    /*@ assert ! \valid(p); */
    /*@ assert ! \valid(q); */
    /*@ assert   \valid(r); */
  }
  return 0;
}

/* Make sure that when `continue` is executed then local variables in scope
 * are not recorded twice. */
void continue_valid() {
  int i = 0;
  int *p, *q;

  while (i++) {
    /*@ assert ! \valid(p); */
    /*@ assert ! \valid(q); */
    int a1 = 1;
    p = &a1;

    /*@ assert \valid(p); */
    /*@ assert ! \valid(q); */

    {
      int a2 = 1;
      q = &a2;
      /*@ assert \valid(p); */
      /*@ assert \valid(q); */
      continue;
    }

    if (i == 5)
      break;
  }

  /*@ assert ! \valid(p); */
  /*@ assert ! \valid(q); */
}

int main(int argc, const char *argv[]) {
  goto_bts();
  goto_valid();
  switch_valid();
  while_valid();
  continue_valid();
  return 0;
}
