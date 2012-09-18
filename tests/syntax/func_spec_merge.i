int f() { return 3; }

int g () { return 4; }

/*@ requires p == &f || p == &g; */
int main (int (*p)(void)) { return (*p)(); }
