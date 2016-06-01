%:define N 5

#define xstr(s) str(s)
%:define str(s) #s

%:define get_first1(a) (a[0])
#define get_first2(a) (a<:0:>)

#define conc(a,b) a ## b
#define conc2(a,b) a %:%: b

int main() <%
  int a<:N:> = <%2,1,0%>; // implicit initialization of remaining elements
  char *s = "a<:"; // digraph inside quotes
  char *b = xstr(get_first1(a));
  char *c = xstr(get_first2(a));
  int d1, d2;
  conc(d, 1) = 7;
  conc2(d, 2) = 9;
  return a<:4:>;
%>
