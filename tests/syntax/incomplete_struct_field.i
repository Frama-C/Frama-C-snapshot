typedef struct _s {
  int i;
  struct _s v[12];
} ts;

ts s;

void main() {
  // JS: don't work yet --> stack overflow
  /*@ assert s.v[1].i == 42; */
};
