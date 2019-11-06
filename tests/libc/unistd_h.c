/*run.config
  STDOPT: #"-slevel 12" #"-val-split-return auto"
  STDOPT: #"-variadic-no-translation" #"-slevel 12" #"-val-split-return auto"
*/
#define _GNU_SOURCE
#define _XOPEN_SOURCE 600
#include <unistd.h>

volatile int nondet;

int main() {
  int r = usleep(123);
  r = usleep(456);
  char hostname[256];
  r = gethostname(hostname, 256);
  // Note: the value set by gethostname is NOT guaranteed to be null-terminated
  execl("/bin/sh", "sh", "-c", "ls", (char*)0);

  r = access("/tmp", R_OK | W_OK | X_OK);
  //@ assert r == -1 || r == 0;

  int fd = dup(1);
  //@ assert fd == -1 || fd >= 0;
  if (fd == -1) return 1;

  off_t offset = 42;
  offset = lseek(fd, offset, SEEK_SET);
  if (offset == -1) return 1;

  int fd2 = dup2(2, fd);
  if (nondet) {
    dup2(2, -2);
    //@ assert unreachable: \false;
  }

  int pid = fork(); // note: process creation not modeled by Eva
  //@ assert pid == -1 || pid >= 0;

  r = setsid();

  sync();

  long l = sysconf(ARG_MAX);

  char cwd[64];
  char *res_getcwd = getcwd(cwd, 64);
  if (res_getcwd) {
    //@ assert res_getcwd == cwd;
    //@ assert valid_read_string((char*)cwd); // currently imprecise
  }

  long pconf = pathconf("/tmp/conf.cfg", _PC_NAME_MAX);

  uid_t ruid, euid, suid;
  r = getresuid(&ruid, &euid, &suid);
  if (!r) {
    r = setresuid(ruid, euid, suid);
    //@ assert r == 0 || r == -1;
  }
  gid_t rgid, egid, sgid;
  r = getresgid(&rgid, &egid, &sgid);
  if (!r) {
    r = setresgid(rgid, egid, sgid);
    //@ assert r == 0 || r == -1;
  }
  pid_t p = getpid();
  p = getppid();
  p = getsid(0);
  ruid = getuid();
  rgid = getgid();
  euid = geteuid();
  egid = getegid();
  r = setegid(egid);
  r = seteuid(euid);
  r = setgid(rgid);
  r = setuid(ruid);
  r = setregid(rgid, egid);
  r = setreuid(ruid, euid);
  r = setpgid(p, getpgid(0));
  r = getpgrp();

  r = unlink("/tmp/test_unlink");

  r = isatty(1);
  //@ assert r == 0 || r == 1;
  char *tty = ttyname(1);

  r = chown("/tmp/a.txt", 01000, 01000);

  r = chdir("/tmp/");

  r = chroot("/tmp");

  if (nondet) {
    pipe(0); // invalid fildes
    //@ assert unreachable:\false;
  }
  int halfpipe;
  if (nondet) {
    pipe(&halfpipe); // invalid fildes
    //@ assert unreachable:\false;
  }
  int pipefd[2];
  r = pipe(pipefd);
  //@ check ok: r == 0 || r == -1;

  return 0;
}
