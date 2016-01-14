const char STRS [2][7] = { {"ABCDEF"}, {"IJKLML"} };

enum bool { INVALID, VALID};

struct signal {
  float val;
  enum bool status;
};

struct signals {
  struct signal f1;
  struct signal f2;
  struct signal f3;
  struct signal f4;
  struct signal f5;
  struct signal f6;
  struct signal f7;
};

const struct signals signals = {
  { 0.0, VALID },
  { 0.0, VALID },
  { 0.0, VALID },
  { 0.0, VALID },
  { 0.0, INVALID },
  { 0.0, VALID },
};

struct signal tsig [3] = { 0.1, INVALID, 3, VALID };


void main() {
  static unsigned char STR [] = { "123456" };

  int i = STR[2];
}
