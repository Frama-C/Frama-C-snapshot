

const int x = 0xFFFFFFFF;

struct s {
  long a : 40;
};

const int y = 0xFFFFFFF0;

struct t {
  long b : 44;
};

const int z = 0xFFFFFFF9;

struct u {
  long c : 48;
};

const int w = 0xFFFFFFF9;

struct v {
  long d : 50;
};


int f () {
  return x;
}
