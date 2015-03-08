/*@ ensures \result == i - (int) (j == 1) ; */
int bug(int i, int j) { return i - (j == 1); }
