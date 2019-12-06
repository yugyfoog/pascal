#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

int lexical_level = 0;

Symbol *program(void);
Symbol *program_heading(void);
Symbol_List *program_parameter_list(void);
void program_block(Symbol *);
void procedure(void);
void function(void);
Block *block(void);
void label_declarations(void);
void label(void);
void constant_definitions(void);
void constant_definition(void);
void type_definitions(void);
Symbol_List *variable_declarations(void);
Symbol_List *procedure_and_function_declaration_part(void);
Identifier_List *identifier_list(void);
char *identifier(void);

Symbol *parse() {
  Symbol *prog;
  
  next_token();
  need(PROGRAM_TOKEN);
  prog = program();
  if (token_type != END_OF_FILE_TOKEN)
    warning("text following end of program ignored");
  return prog;
}

Symbol *program() {
  Symbol *prog;

  prog = program_heading();
  need(SEMICOLON_TOKEN);
  program_block(prog);
  need(PERIOD_TOKEN);
  return prog;
}

Symbol *program_heading() {
  char *name;
  Identifier_List *params;
  
  name = identifier();
  if (match(LPAREN_TOKEN)) {
    params = identifier_list();
    need(RPAREN_TOKEN);
  }
  else
    params = 0;  
  return new_program_symbol(name, params);
}

void procedure() {
  XXX();
}

void function() {
  XXX();
}

void program_block(Symbol *prog) {
  lexical_level++;
  push_symbol_table();
  prog->prog.block = block();
  pop_symbol_table();
  lexical_level--;
}

Block *block() {
  if (match(LABEL_TOKEN))
    label_declarations();
  if (match(CONST_TOKEN))
    constant_definitions();
  if (match(TYPE_TOKEN))
    type_definitions();
  if (match(VAR_TOKEN))
    variable_declarations();
  for (;;) {
    if (match(PROCEDURE_TOKEN))
      procedure();
    else if (match(FUNCTION_TOKEN))
      function();
    else
      break;
  }
  compound_statement();
  return 0;
}

void label_declarations() {
  do
    label();
  while (match(COMMA_TOKEN));
  need(SEMICOLON_TOKEN);
}

void label() {
  need(INTEGER_TOKEN);
}

void constant_definitions() {
  while (check(IDENTIFIER_TOKEN))
    constant_definition();
}

void constant_definition() {
  Constant *cnst;
  char *name;

  name = identifier();
  need(EQ_TOKEN);
  cnst = constant();
  need(SEMICOLON_TOKEN);
  insert(new_constant_symbol(name, cnst));
}

void type_definitions() {
  XXX();
}

Symbol_List *variable_declarations() {
  XXX();
  return 0;
}

Symbol_List *procedure_and_function_declaration_part() {
  XXX();
  return 0;
}

Identifier_List *identifier_list() {
  Identifier_List *ids = new(Identifier_List);

  ids->id = identifier();
  if (match(COMMA_TOKEN))
    ids->next = identifier_list();
  else
    ids->next = 0;
  return ids;
}

char *identifier() {
  char *id;
  
  if (token_type != IDENTIFIER_TOKEN) {
    error("missing identifier near %s", token);
    id = strdup("%error%");
  }
  else {
    id = strdup(token);
    next_token();
  }
  return id;
}
