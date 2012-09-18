typedef enum { E1_a, E1_b, E1_c } E1;
typedef enum { E2_a = E1_a, E2_b } E2;
int f (E2 e) { return e; }
