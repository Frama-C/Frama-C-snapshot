struct s1 { int a; };
struct s2 { int a; };

int f (struct s1);
int f (struct s2);

union u1 { int a; };
union u2 { int a; };

int g (union u1);
int g (union u2);
