int X,Y,FOO;

int main(){
  int foo, x, y;
  foo ? x : y; // should be kept
  foo ? X : y; // should be kept
  foo ? X : Y; // only foo should be kept
  FOO ? X : Y; // should disappear
  return 0;
}
