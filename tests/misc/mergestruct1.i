/* run.config
   DONTRUN:
*/
struct s { int a; } s1;

void f(void);

main()
{
  s1.a = 1;
  f();
}
