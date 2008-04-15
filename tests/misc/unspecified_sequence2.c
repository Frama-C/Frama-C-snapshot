/* based on an example from J. Regehr on the why list */
/* precondition: false */
int a[2];

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

  multiple_update_wrong_1(&b, &c);

  multiple_update_wrong_2(b);

  multiple_update_wrong_3(c);

  multiple_update_unsafe(&b,&c); // does not lead to an alarm

  multiple_update_unsafe(&b, &b);

  multiple_update_safe(&b,&c); // does not lead to an alarm

  multiple_update_safe(&c,&c); // does not lead to an alarm

  return 0;
}
