#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"
#include "code.h"

char *program_name;
char *input_name;
char *output_name;

FILE *input;
FILE *output;

void options(int, char **);
void usage(void);

int main(int argc, char **argv) {
  Symbol *program;
  
  options(argc, argv);
  
  initialize_symbols();

  input = fopen(input_name, "r");
  if (input == 0)
    fatal_error("unable to open %s", input_name);
  
  program = parse();
  
  fclose(input);

  if (error_count == 0) {
    output = fopen(output_name, "w");
    if (output == 0)
      fatal_error("unable to open %s", output_name);
    
    code_program(program);
    fclose(output);
  }
  
  if (error_count) {
    printf("%d error%s found\n", error_count,
	   error_count == 1 ? "" : "s");
    return 1;
  }
  return 0;
}

void options(int argc, char **argv) {
  char *extension;

  program_name = argv[0];
  /* only 1 option for new */
  if (argc != 2)
    usage();
  input_name = argv[1];
  output_name = strdup(input_name);
  extension = strrchr(output_name, '.');
  if (extension == 0
      || ((strcmp(extension, ".p") != 0)
	  && (strcmp(extension, ".pas") != 0)))
    fatal_error("source file must have a '.p' or '.pas' extension");
  strcpy(extension, ".s");
}

void usage() {
  printf("usage: %s source.p\n", program_name);
  exit(1);
}

