#include <stdio.h>
#include "pascal.h"

void statement_sequence(void);

void compound_statement() {
  need(BEGIN_TOKEN);
  statement_sequence();
  need(END_TOKEN);
}

void statement_sequence() {
  XXX();
}

