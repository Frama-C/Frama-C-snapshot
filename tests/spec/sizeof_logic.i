/*@ lemma bad: \forall integer x; sizeof(x) == sizeof(int); */

/*@ lemma bad2: sizeof(integer) == sizeof(int); */

/*@ lemma good: \forall short x; sizeof(x) <= sizeof(int); */

struct S { int f; };
typedef struct S volatile a;
/*@ ensures \result == sizeof(struct S volatile); */
int f(int a) { return sizeof(struct S volatile); }
