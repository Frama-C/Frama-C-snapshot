volatile v;

void main1() {
  int c = 0;
  if (v)
    goto L2;
  
 L1:
  if (c >= 1000000)
    return;
  Frama_C_show_each(c);
 L2:
  c++;
  goto L1;
}


void duff1(int *to, int *from, int count) {
  register n = (count + 7) / 8;
  switch(count % 8) {
    case 0: do {
              Frama_C_show_each(to);
              *to++ = *from++;
    case 7:   *to++ = *from++;
    case 6:   *to++ = *from++;
    case 5:   *to++ = *from++;
    case 4:   *to++ = *from++;
    case 3:   *to++ = *from++;
    case 2:   *to++ = *from++;
    case 1:   *to++ = *from++;
    } while(--n > 0);
  }
}

void duff2(int *to, int *from, int count) {
  register n = (count + 7) / 8;
  switch(count % 8) {
  case 0: L: { Frama_C_show_each(to);
                    *to++ = *from++;
    case 7:         *to++ = *from++;
    case 6:         *to++ = *from++;
    case 5:         *to++ = *from++;
    case 4:         *to++ = *from++;
    case 3:         *to++ = *from++;
    case 2:         *to++ = *from++;
    case 1:         *to++ = *from++;
  } if (--n > 0) goto L;
  }
}

void main2() {
  int p1[100002];
  int p2[100000];
  int o = v;
  if (0 <= o && o < 100002)
    *(p1+o) = 1;
  duff1(p2, p1, 100000);
    *(p1+o) = 2;
  duff2(p2, p1, 100000);
}

void main() {
  main1();
  main2();
}
