static int a;

int f(void)
{
  static int b = 3;
  b *= 2;
  return a++;
}
