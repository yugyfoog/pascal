#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

int lexical_level = 0;

Symbol *program(void);
Symbol *program_heading(void);
Symbol_List *program_parameter_list(void);
void program_block(Symbol *);
Block *block(void);
void label_declaration_part(void);
void constant_definition_part(void);
void type_definition_part(void);
Symbol_List *variable_declaration_part(void);
Symbol_List *procedure_and_function_declaration_part(void);
Id_List *identifier_list(void);
char *identifier(void);

Symbol *parse() {
  Symbol *prog;
  
  next_token();
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
  Symbol_List *params;
  
  need(PROGRAM_TOKEN);
  name = identifier();
  if (match(LPAREN_TOKEN)) {
    params = program_parameter_list();
    need(RPAREN_TOKEN);
  }
  else
    params = 0;  
  return new_program_symbol(name, params);
}

Symbol_List *program_parameter_list() {
  Symbol_List *syms;
  char *id;
  
  id = identifier();
  if (strcmp(id, "input") == 0
      || strcmp(id, "output") == 0
      || strcmp(id, "error") == 0) {
    syms = new(Symbol_List);
    syms->sym = new_val_param_symbol(id, text_type);
    if (match(COMMA_TOKEN))
      syms->next = program_parameter_list();
    else
      syms->next = 0;
    return syms;
  }
  warning("program parameter %s ignored", id);
  if (match(COMMA_TOKEN))
    return program_parameter_list();
  return 0;
}

void program_block(Symbol *prog) {
  lexical_level++;
  push_symbol_table();
  insert_parameters(prog->prog.params);
  prog->prog.block = block();
  pop_symbol_table();
  lexical_level--;
}

Block *block() {
  Block *blck = new(Block);
  
  label_declaration_part();
  constant_definition_part();
  type_definition_part();
  blck->variables = variable_declaration_part();
  blck->procfuncs = procedure_and_function_declaration_part();
  blck->stmt = statement_part();
  return blck;
}

void label_declaration_part() {
  XXX();
}

void constant_definition_part() {
  XXX();
}

void type_definition_part() {
  XXX();
}

Symbol_List *variable_declaration_part() {
  XXX();
  return 0;
}

Symbol_List *procedure_and_function_declaration_part() {
  XXX();
  return 0;
}

Id_List *identifier_list() {
  Id_List *ids = new(Id_List);

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
