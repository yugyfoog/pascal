#include <stdio.h>
#include "pascal.h"

void statement_sequence(void);
void statement(void);
void label(void);
void assignment_statement(void);
void procedure_statement(void);
void standard_procedure_statement(void);
void rewrite_procedure(void);
void put_procedure(void);
void reset_procedure(void);
void get_procedure(void);
void read_procedure(void);
void readln_procedure(void);
void write_procedure(void);
void writeln_procedure(void);
void write_parameter(void);
void get_procedure(void);
void new_procedure(void);
void page_procedure(void);
void dispose_procedure(void);
void pack_procedure(void);
void unpack_procedure(void);
void goto_statement(void);
void if_statement(void);
void case_statement(void);
void repeat_statement(void);
void while_statement(void);
void for_statement(void);
void with_statement(void);

void compound_statement() {
  need(BEGIN_TOKEN);
  statement_sequence();
  need(END_TOKEN);
}

void statement_sequence() {
  do
    statement();
  while (match(SEMICOLON_TOKEN));
}

void statement() {
  Symbol *sym;
  
  if (check(INTEGER_TOKEN))
    label();
  switch (token_type) {
  case END_TOKEN:
  case SEMICOLON_TOKEN:
    /* empty statement */
    next_token();
    break;
  case IDENTIFIER_TOKEN:
    sym = lookup(token);
    if (sym) {
      switch (sym->class) {
      case FUNCTION_SYMBOL:
      case VARIABLE_SYMBOL:
      case VARARG_SYMBOL:
      case VALARG_SYMBOL:
	assignment_statement();
	break;
      case PROCEDURE_SYMBOL:
      case PROCARG_SYMBOL:
	procedure_statement();
	break;
      case STDPROC_SYMBOL:
	standard_procedure_statement();
	break;
      default:
	error("illegal statement near %s", token);
      }
    }
    else
      error("%s is not defined", token);
    break;
  case GOTO_TOKEN:
    goto_statement();
    break;
  case BEGIN_TOKEN:
    compound_statement();
    break;
  case IF_TOKEN:
    if_statement();
    break;
  case CASE_TOKEN:
    case_statement();
    break;
  case REPEAT_TOKEN:
    repeat_statement();
    break;
  case WHILE_TOKEN:
    while_statement();
    break;
  case FOR_TOKEN:
    for_statement();
    break;
  case WITH_TOKEN:
    with_statement();
    break;
  default:
    error("illegal statement near %s", token);
  }
}

void assignment_statement() {
  XXX();
}

void procedure_statement() {
  XXX();
}

void standard_procedure_statement() {
  Symbol *sym = lookup(token);
  switch (sym->stdproc) {
  case REWRITE_PROCEDURE:
    rewrite_procedure();
    break;
  case PUT_PROCEDURE:
    put_procedure();
    break;
  case RESET_PROCEDURE:
    reset_procedure();
    break;
  case GET_PROCEDURE:
    get_procedure();
    break;
  case READ_PROCEDURE:
    read_procedure();
    break;
  case READLN_PROCEDURE:
    readln_procedure();
    break;
  case WRITE_PROCEDURE:
    write_procedure();
    break;
  case WRITELN_PROCEDURE:
    writeln_procedure();
    break;
  case PAGE_PROCEDURE:
    page_procedure();
    break;
  case NEW_PROCEDURE:
    new_procedure();
    break;
  case DISPOSE_PROCEDURE:
    dispose_procedure();
    break;
  case PACK_PROCEDURE:
    pack_procedure();
    break;
  case UNPACK_PROCEDURE:
    unpack_procedure();
    break;
  }
}

void rewrite_procedure() {
  XXX();
}

void put_procedure() {
  XXX();
}

void reset_procedure() {
  XXX();
}

void get_procedure() {
  XXX();
}

void read_procedure() {
  XXX();
}

void readln_procedure() {
  XXX();
}

void write_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  do
    write_parameter();
  while (match(COMMA_TOKEN));
  need(RPAREN_TOKEN);
}

void write_parameter() {
  expression();
  if (match(COLON_TOKEN)) {
    expression();
    if (match(COLON_TOKEN)) {
      expression();
    }
  }
}

void writeln_procedure() {
  XXX();
}

void page_procedure() {
  XXX();
}

void new_procedure() {
  XXX();
}

void dispose_procedure() {
  XXX();
}

void pack_procedure() {
  XXX();
}

void unpack_procedure() {
  XXX();
}

void goto_statement() {
  warning("tisk, tisk");
  XXX();
}

void if_statement() {
  XXX();
}

void case_statement() {
  XXX();
}

void repeat_statement() {
  XXX();
}

void while_statement() {
  XXX();
}

void for_statement() {
  XXX();
}

void with_statement() {
  XXX();
}
