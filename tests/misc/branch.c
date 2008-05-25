int a, b,c,d,e;
void main()
{
 L: a=0;
 if (c) goto L2;
 L3: b=0;
 goto L;
 L2: d=0;
 if (d) goto L;
 if (e) goto L4;
 goto L3;
 L4: return;
}
