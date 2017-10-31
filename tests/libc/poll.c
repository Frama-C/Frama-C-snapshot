#include <stdio.h>
#include <poll.h>

#define GOT_TIMEOUT 127

int main() {
  struct pollfd ufds;

  ufds.fd = 0; // stdin
  ufds.events = POLLIN | POLLPRI;
  int r = poll(&ufds, 1, 1000);
  if (r == -1) perror("poll");
  if (r == 0) return GOT_TIMEOUT;
  int can_read = ufds.revents & POLLIN;
  int can_read_out_of_band = ufds.revents & POLLPRI;
  int invalid_fd = ufds.revents & POLLNVAL;

  return can_read | can_read_out_of_band | invalid_fd;
}
