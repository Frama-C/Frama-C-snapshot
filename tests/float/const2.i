float f1 = 1e-40f;
float f2 = 1e40f;

int main()
{
  Frama_C_dump_each();
  double d1 = f1;
  double d2 = f2;
}
