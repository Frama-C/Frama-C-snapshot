//@ requires p1 >= 1;
int __attribute__((tret1)) f(int __attribute__((arg1)) p1) __attribute__((f1));

//@ requires p2 >= 1; // identical to previous contract
int __attribute__((tret2)) f(int __attribute__((arg2)) const volatile p2) __attribute__((f2));

//@ requires p3 >= 3;
int __attribute__((tret3)) f(int __attribute__((arg3)) const p3)
// note: GCC forbids declaring function attributes in function definitions,
// so we cannot add '__attribute__((f3))' here
{
  return p3;
}

//@ requires p4 >= 4;
int __attribute__((tret4)) f(int __attribute__((arg4)) volatile p4) __attribute__((f4));

int __attribute__((tret5)) f() __attribute__((f5));

typedef int __attribute__((a1)) aint;

aint g();

aint g(const aint i1);

volatile aint g(volatile aint i2);

aint g(int __attribute__((a2)) i3) {
  return i3;
}


typedef int __attribute__((p1))* __attribute__((p2)) iptr;

iptr volatile h(const iptr ip1);

iptr const h();

iptr h(volatile iptr ip2) {
  return 0;
}

iptr volatile h(const iptr ip3);
