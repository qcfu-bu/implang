#include <stdbool.h>
#include <stdio.h>

typedef struct {
  void *data;
  void *func;
} closure_t;

extern void print_bool(bool b, closure_t c) {
  printf("%s\n", b ? "true" : "false");
  return;
}

extern void print_int(int n, closure_t c) {
  printf("%d\n", n);
  return;
}