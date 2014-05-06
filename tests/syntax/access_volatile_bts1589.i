volatile int x;

int z;

typedef volatile int vi;

vi y;

struct volatile_struct {
  volatile int a;
  vi b;
  int c;
} vs;

int main(){
  x; y; z; vs.a; vs.b; vs.c;
}
