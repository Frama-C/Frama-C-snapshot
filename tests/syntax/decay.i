struct P {
  int val[2][2];
};

void multi_dim_array_decay() { // BTS 1142
  struct P* pp;
  struct P p;
  pp = &p;
  *(pp->val);
  *(p.val);
}

char t[8];

struct {
  int A[sizeof(t)]; 
  int i;
} A = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }; //Size of the array is 8

struct {
  int A[sizeof(0, t)];
  int i;
} S = {1, 2, 3, 4, 5}; // Size of the array is sizeof(char *), not sizeof(t), because in this case there is a decay; BTS 1774


struct {
  int A[sizeof(&t[0])];
  int i;
} V = {1, 2, 3, 4, 5}; // Size of the array is again sizeof(char*), not sizeof(t). Note 5077 in bts 1774
