#include <stdio.h>
#include "pascal.h"

Symbol *program(void);

Symbol *parse() {
  Symbol *prog;
  
  next_token();
  prog = program();
  if (token_type != END_OF_FILE_TOKEN)
    warning("text following end of program ignored");
  return prog;
}

Symbol *program() {
  XXX();
  return 0;
}
