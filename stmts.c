#include <stdio.h>
#include "pascal.h"

void statement_sequence(void);
void statement(void);
void label(void);
void assignment_statement(void);
void procedure_statement(Symbol *);
void actual_parameter_list(Symbol_List *);
void actual_parameter(Symbol *);
void standard_procedure(Standard_Procedure);
void rewrite_procedure(void);
void put_procedure(void);
void reset_procedure(void);
void get_procedure(void);
void read_procedure(void);
void readln_procedure(void);
void read_parameter_list(void);
void write_procedure(void);
void writeln_procedure(void);
void write_parameter_list(void);
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
void case_list_elements(void);
void case_list_element(void);
void case_constant_list(void);
void repeat_statement(void);
void while_statement(void);
void for_statement(void);
void with_statement(void);
void record_variable_list(void);

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
  
  if (check(INTEGER_TOKEN)) {
    label();
    need(COLON_TOKEN);
  }
  switch (token_type) {
  case SEMICOLON_TOKEN:
  case END_TOKEN:
  case UNTIL_TOKEN:
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
	procedure_statement(sym);
	break;
      case STDPROC_SYMBOL:
	standard_procedure(sym->stdproc);
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
  variable_access();
  need(ASSIGN_TOKEN);
  expression();
}

void procedure_statement(Symbol *sym) {
  next_token();
  /* follow the definition of procedure for syntax */
  if (sym->proc.params) {
    need(LPAREN_TOKEN);
    actual_parameter_list(sym->proc.params);
    need(RPAREN_TOKEN);
  }
}

void actual_parameter_list(Symbol_List *syms) {
  for (;;) {
    actual_parameter(syms->sym);
    syms = syms->next;
    if (syms == 0)
      break;
    need(COMMA_TOKEN);
  }
}

void actual_parameter(Symbol *sym) {
  switch (sym->class) {
  case VARARG_SYMBOL:
    variable_access();
    break;
  case VALARG_SYMBOL:
    expression();
    break;
  case PROCARG_SYMBOL:
  case FUNCARG_SYMBOL:
    identifier();
    break;
  default:
    internal_error();
  }
}

void standard_procedure(Standard_Procedure stdproc) {
  switch (stdproc) {
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
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  need(RPAREN_TOKEN);
}

void put_procedure() {
  XXX();
}

void reset_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  need(RPAREN_TOKEN);
}

void get_procedure() {
  XXX();
}

void read_procedure() {
  XXX();
}

void readln_procedure() {
  next_token();
  if (match(LPAREN_TOKEN)) {
    read_parameter_list();
    need(RPAREN_TOKEN);
  }
}

void read_parameter_list() {
  do {
    variable_access();
  } while (match(COMMA_TOKEN));
}

void write_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  write_parameter_list();
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
  next_token();
  if (match(LPAREN_TOKEN)) {
    write_parameter_list();
    need(RPAREN_TOKEN);
  }
}

void write_parameter_list() {
  do
    write_parameter();
  while (match(COMMA_TOKEN));
}

void page_procedure() {
  XXX();
}

void new_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  /* cast constant list */
  need(RPAREN_TOKEN);
}

void dispose_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  /* cast constant list */
  need(RPAREN_TOKEN);
}

/*
  a z variable_access
  i expression

  pack(a,i,z)
  unpack(z,a,i)
*/

void pack_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  need(COMMA_TOKEN);
  expression();
  need(COMMA_TOKEN);
  variable_access();
  need(RPAREN_TOKEN);
}

void unpack_procedure() {
  next_token();
  need(LPAREN_TOKEN);
  variable_access();
  need(COMMA_TOKEN);
  variable_access();
  need(COMMA_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void goto_statement() {
  warning("tisk, tisk");
  match(GOTO_TOKEN);
  label();
}

void if_statement() {
  next_token();
  expression();
  need(THEN_TOKEN);
  statement();
  if (match(ELSE_TOKEN))
    statement();
}

void case_statement() {
  next_token();
  expression();
  need(OF_TOKEN);
  case_list_elements();
  need(END_TOKEN);
}

void case_list_elements() {
  for (;;) {
    case_list_element();
    if (check(END_TOKEN))
      return;
    need(SEMICOLON_TOKEN);
    if (check(END_TOKEN))
      return;
  }
}

void case_list_element() {
  case_constant_list();
  need(COLON_TOKEN);
  statement();
}

/*
   this function is used in parsing variant records,
   case statements, new procedure, and dispose procedure
*/

void case_constant_list() {
  do
    constant();
  while (match(COMMA_TOKEN));
}

void repeat_statement() {
  next_token();
  statement_sequence();
  need(UNTIL_TOKEN);
  expression();
}

void while_statement() {
  next_token();
  expression();
  need(DO_TOKEN);
  statement();
}

void for_statement() {
  next_token();
  variable_access();
  need(ASSIGN_TOKEN);
  expression();
  if (match(TO_TOKEN))
    ;
  else if (match(DOWNTO_TOKEN))
    ;
  else
    error("'to' or 'downto' expected near %s", token);
  expression();
  need(DO_TOKEN);
  statement();
}

/* I hate with statements! */

void with_statement() {
  next_token();
  record_variable_list();
  need(DO_TOKEN);
  statement();
}

void record_variable_list() {
  do
    variable_access();
  while (match(COMMA_TOKEN));
}
