/* run.config
   OPT: -check -slice-undef-functions -slice-return send1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -slice-undef-functions -slice-return send2 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -slice-undef-functions -slice-return send3 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -slice-undef-functions -slice-return send4 -journal-disable -then-on 'Slicing export' -print
   */

/* Small example derived from examples given for UNRAVEL tool : */
/* Slicing and Dicing example */

/* Notice that scanf result is from TOP : have to use -slice-undef-functions
 * if we don't want to propagate this imprecision... */
/*@ assigns *p \from \empty;
    assigns \result ; */
int scanf (char const *, int * p);
int printf (char const *, int);

int send1 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send2 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send3 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send4 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int nb_fetch = 0;
int fetch(void) {
    int value; nb_fetch++;
    scanf ("%d",&value);
    return value;
}

int main(void) {
    int red, green, blue, yellow;
    int sweet,sour,salty,bitter;
    int i;

    red = fetch();
    blue = fetch();
    green = fetch();
    yellow = fetch();

    red = 2*red;
    sweet = red*green;
    sour = 0;
    for (i = 0; i < red; i++)
        sour += green;
    salty = blue + yellow;
    green = green + 1;
    bitter = yellow + green;

    send1 (sweet);
    send2 (sour);
    send3 (salty);
    send4 (bitter);
    return 1;
}
