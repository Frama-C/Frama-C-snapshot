int t[5] = { [2] = 3 };

struct { char a; int t[5]; } s = { 'a' , { [2] = 3 } };

int u[6] = { [4] = 4, [2] = 2 };

void main(void)
{
  Frama_C_dump_each();
}
