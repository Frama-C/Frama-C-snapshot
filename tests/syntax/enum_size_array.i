typedef enum {
ONE,
TWO,
EN_NB} T_E;

typedef int T_TAB[EN_NB];

int f_return_last(T_TAB tab)
{
return tab[EN_NB-1];
}
