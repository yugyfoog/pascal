#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "machine.h"
#include "pascal.h"

#define MAX_ERRORS 16

int error_count = 0;

void location(void);

void syntax_error(int n) {
  error("syntax error (%d) near %s", n, token);
}

void warning(char *fmt, ...) {
  va_list args;

  printf("warning: ");
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
}

void error(char *fmt, ...) {
  va_list args;

  printf("error: ");
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  if (error_count++ >= MAX_ERRORS)
    fatal_error("too many errors");
}

void fatal_error(char *fmt, ...) {
  va_list args;

  printf("fatal error: ");
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}

void location() {
  printf("%s: %d: ", input_name, line_number);
}

void x_undefined(char *file, int line, char const *func) {
  fatal_error("%s() -- undefined in %s at line %d", func, file, line);
}

void x_internal_error(char *file, int line, char const *func) {
  fatal_error("internal error in %s() at %s, line %d", func, file, line);
}
