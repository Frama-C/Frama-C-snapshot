extern unsigned char x;

void g() {
  int y;

  x = x / 16;

  switch((int)x) {
  case 0:
    y = x;
    break;
  case 1:
    y = x * 2;
    break;
  case 2:
    y = x * 3;
    break;
  case 3:
    y = x * 4;
    break;
  case 4:
    y = x * 5;
    break;
  case 5:
    y = x * 6;
    break;
  case 6:
    y = x * 7;
    break;
  case 7:
    y = x * 8;
    break;
  case 8:
    y = x * 9;
    break;
  case 9:
    y = x * 10;
    break;
  case 10:
    y = x * 11;
    break;
  case 11:
    y = x * 12;
    break;
  case 12:
    y = x * 13;
    break;
  case 13:
    y = x * 14;
    break;
  case 14:
    y = x * 15;
    break;
  case 15:
    y = x * 16;
    break;
  case 16:
    y = x * 17;
    break;
  case 17:
    y = x * 18;
    break;
  default:
    break;
  }
  y += 1;
  Frama_C_show_each(y);
}

void main() {
  g();
}
