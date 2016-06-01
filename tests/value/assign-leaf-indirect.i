int x,y,z;
/*@ assigns y \from *p, p; */
void f(int*p);

/*@ assigns z \from *p, indirect:p; */
void g(int*p);
void main() {
  f(&x);
  g(&x);
}
