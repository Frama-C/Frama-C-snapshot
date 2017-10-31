union X {
        int a;
        short b;
        short c;
};
int main()
{
  union X u,v,w,x;
  v = (union X) u;
  int a;

  // GCC extension. See https://gcc.gnu.org/onlinedocs/gcc/Cast-to-Union.html
  w = (union X) a;
  short b;
  x = (union X) b;
}

