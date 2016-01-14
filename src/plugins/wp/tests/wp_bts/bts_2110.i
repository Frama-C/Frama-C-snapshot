/* run.config
   CMD: @frama-c@ -wp -wp-log shell,cluster -wp-gen -wp-share ./share
   OPT:
*/

/* run.config_qualif
   DONTRUN:
*/

struct FD {
	int pos;
	int *adr;
};

struct A { int dummy; };

/*@
	//requires \valid(fd);
	//requires \valid(a);
	//requires \separated(a,fd);
	assigns fd->pos;
	assigns *a;
	ensures fd->pos != \old(fd->pos);
*/
int myRead(struct FD* fd,struct A* a);

/*@
	//requires \valid(fd);
	//requires \valid(a);
	//requires \separated(a,fd);
	ensures KO: *a == \old(*a);
*/
void myMain(struct FD* fd,struct A* a)
{
	//@ assigns KO: *a;
	myRead(fd,a);
}
