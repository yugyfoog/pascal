#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pascal.h"

int error_count;

void location() {
  printf("%s: %d: ", input_name, line_number);
}

void warning(char *fmt, ...) {
  va_list args;
  
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
  va_end(args);
}

void error(char *fmt, ...) {
  va_list args;
  
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
  va_end(args);
  if (++error_count > MAX_ERRORS)
    fatal_error("too many errors");
}

void fatal_error(char *fmt, ...) {
  va_list args;
  
  location();
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
  va_end(args);
  exit(1);
}

void x_undefined(char *file, char const *func, int line) {
  fatal_error("%s() undefined at line %d and file %s", func, line, file);
}

void x_internal_error(char *file, char const *func, int line) {
  fatal_error("internal error in %s() at line %d in file %s", func, line, file);
}
