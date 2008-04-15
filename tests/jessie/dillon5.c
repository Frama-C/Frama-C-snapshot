
typedef struct { int a; } las2;
typedef struct { las2 s; } las1;

//@ predicate inv{L}(las1 * v) = \valid(v) ==> \valid(&(v->s));

las1 v1;

/*@
  @requires \valid(v2);
@assigns v2->a;
@ensures v2->a==3;
*/
void g(las2 * v2)
{
v2->a = 3;
}

/*@
requires \valid(&v1.s);
assigns v1.s.a;
ensures v1.s.a==3;
*/
int main()
{
g(&v1.s);
return v1.s.a;
}
