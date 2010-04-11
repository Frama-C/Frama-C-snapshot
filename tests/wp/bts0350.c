struct Ts {int x; int y; };
struct Tstr {int a; struct Ts s; int t[10]; struct Tstr * p; } S;

/*@ requires S.p == &S || \separated (S.p, &S);
  @ ensures S.p->a == x ;
  @ ensures \forall int i; (*(S.p)).t[i] == \old((*(S.p)).t[i]);
  */

void rw_ptr_field(int x )
{
  (S.p)->a = x;
  return;
}

int main (void) {return 0;}
