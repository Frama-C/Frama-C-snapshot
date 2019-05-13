/* run.config*
   STDOPT: #"-lib-entry"
*/

unsigned short AutoTest[1000]={0};

int X;
int* T[]={&X,0,};


int X1;
int X2;
int X3;
int X4,X5,X6,X7,X8,X9;

void main(int c){


  int count = 0;

//  int *p=T[c];
//   X = *p;

  while(count<10) {
    Frama_C_show_each_F(X,count);
    switch (count) {
      case 0: X = X1;
      break;
      case 1: X = X2;
      break;
      case 2: X = X3;
      break;
      case 3: X = X4;
      break;
      case 4: X = X5;
      break;
      case 5: X = X6;
      break;
      case 6: X = X7;
      break;
      case 7: X = X8;
      break;
      case 8: X = X9;
      break;
      }
    count++;
    }
}

void main1(int c){

  int X1;
  int* X2;
  X1 = (int) X2;

}
