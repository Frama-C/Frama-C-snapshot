struct foo { char bar[4]; };

/*@ assigns x->bar[0..3] \from x->bar[0..3]; */
int f(struct foo* x);

typedef char baz[4];

struct bli { baz bli; };

/*@ assigns x[0..3] \from y->bli[0..3]; */
int g(baz x,struct bli* y);
