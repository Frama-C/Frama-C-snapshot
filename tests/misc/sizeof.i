int sz_str,sz_typ,align_str,align_typ;

void main()
{
  sz_str= sizeof("ONE");
  //@ assert sz_str == sizeof("ONE");
  align_str= __alignof("FOO");
  // assert align_str == __alignof("FOO");
  sz_typ= sizeof(char);
  //@ assert sz_typ == sizeof(char);
  align_typ= __alignof(char*);
  // assert align_typ == __alignof((char*));
  //@ assert sizeof("BLA") != sizeof("FOOBAR");
  return;
}
