extern int Tab[] ;
/*@ behavior default:
      ensures (Tab[\at(i,Old)] == \at(v,Old));
        */
extern void writeTab(int i , int v ) ;
/*@ behavior default:
      ensures (Tab[\at(j,Old)] == \at(x,Old));
        */
void main(int j , int x )
{
  writeTab(j,x);
  return;
}
