typedef union {
  char t;
  short s;
} un;

typedef struct {
  char c;
  un u;
} st;

extern st G1;
