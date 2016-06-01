/* run.config*
   STDOPT: #"-unspecified-access"
*/
/* based on an example from J. Regehr on the why list */
/* precondition: false */
int a[2]; volatile int foo;

int
multiple_update_wrong_1 (int *x, int *y)
{
  return (*x = 0) + (*x = 0);
}

/* precondition: false */
int
multiple_update_wrong_2 (int i)
{
  i = ++i + 1;
  return i;
}

/* precondition: false */
int
multiple_update_wrong_3 (int i)
{
  a[i++] = i;
  return i;
}

/* precondition: x != y */
int
multiple_update_unsafe (int *x, int *y)
{
  return (*x = 0) + (*y = 0);
}

/* precondition: true */
int
multiple_update_safe (int *x, int *y)
{
  if (x == y)
    {
      return 0;
    }
  else
    {
      return (*x = 0) + (*y = 0);
    }
}

int main () {
  int b,c;
  b = 0;
  c = 0;

  if (foo) { multiple_update_wrong_1(&b, &c); Frama_C_show_each_passed1(); }

  if (foo) { multiple_update_wrong_2(b); Frama_C_show_each_passed2(); }

  if (foo) { multiple_update_wrong_3(c); Frama_C_show_each_passed3(); }

  if (foo) { multiple_update_unsafe(&b,&c); /* does not lead to an alarm */ Frama_C_show_each_passed4(); }

  if (foo) { multiple_update_unsafe(&b, &b); Frama_C_show_each_passed5(); }

  if (foo) { multiple_update_safe(&b,&c); /* does not lead to an alarm */ Frama_C_show_each_passed6(); }

  if (foo) { multiple_update_safe(&c,&c); /* does not lead to an alarm */ Frama_C_show_each_passed7(); }

  return 0;
}
