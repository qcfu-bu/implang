#include <stdio.h>

extern "C" void print_bool(bool b, void *) {
  printf("%s\n", b ? "true" : "false");
  return;
}

extern "C" void print_int(int n, void *) {
  printf("%d\n", n);
  return;
}