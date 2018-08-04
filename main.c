#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

char *program_name;
char *input_name;
char *output_name;
FILE *input;
FILE *output;

void options(int, char **);
void usage(void);

int main(int argc, char **argv) {
  Symbol *prog;
  
  options(argc, argv);
  input = fopen(input_name, "r");
  if (input == 0)
    fatal_error("unable to open %s", input_name);
  initialize_symbols();
  prog = parse();
  fclose(input);
  if (error_count == 0) {
    output = fopen(output_name, "w");
    if (output == 0)
      fatal_error("unable to open %s", output_name);
    code_program(prog);
    fclose(output);
  }
  else
    printf("%d error%s found.\n", error_count,
	   error_count == 1 ? "" : "s");
  return error_count != 0;
}

void options(int argc, char **argv) {
  char *extension;
  
  program_name = argv[0];
  if (argc != 2)
    usage();
  input_name = argv[1];
  output_name = strdup(input_name);
  extension = strrchr(output_name, '.');
  if (extension == 0 || strcmp(extension, ".p") != 0)
    fatal_error("source file must have '.p' extension");
  strcpy(extension, ".s");
}

void usage() {
  printf("usage: %s source.p\n", program_name);
  exit(1);
}
