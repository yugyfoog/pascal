#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

Symbol *program(void);
Symbol *program_heading(void);
Symbol_List *program_parameter_list(void);
Symbol *program_parameter(void);
Symbol *procedure_declaration(void);
Symbol *procedure_heading(void);
Symbol *function_declaration(void);
Symbol *function_heading(void);
Directive directive(void);
Symbol_List *formal_parameter_list(void);
Symbol_List *variable_parameters(void);
Symbol_List *value_parameters(void);
Symbol_List *parameter_id_list(Symbol_Class);
Symbol_List *procedure_parameter(void);
Symbol_List *function_parameter(void);
void block(Symbol *);
void label_declaration_part(Symbol *);
void label_declarations(Symbol *);
void label_declaration(Symbol *);
void constant_definition_part(void);
void constant_definitions(void);
void constant_definition(void);
void type_definition_part(void);
void type_definitions(void);
void type_definition(void);
void variable_declaration_part(void);
void variable_declarations(void);
void variable_declaration(void);
Symbol_List *procedure_and_function_declaration_part(void);
Statement *statement_part(void);

Symbol *parse() {
  Symbol *prog;

  next_token();  /* prime the pump */
  prog = program();
  if (!check(END_OF_FILE_TOKEN))
    warning("text following program ignored");
  return prog;
}

Symbol *program() {
  Symbol *prog;

  prog = program_heading();
  need(SEMICOLON_TOKEN);
  block(prog);
  need(PERIOD_TOKEN);
  return prog;
}

Symbol *program_heading() {
  Symbol_List *parameters;
  char *id;

  need(PROGRAM_TOKEN);
  id = identifier();
  if (match(LPAREN_TOKEN)) {
    parameters = program_parameter_list();
    need(RPAREN_TOKEN);
  }
  else
    parameters = 0;
  return new_program_symbol(id, parameters);
}

Symbol_List *program_parameter_list() {
  Symbol_List *params = new(Symbol_List);

  params->symbol = program_parameter();
  if (match(COMMA_TOKEN))
    params->next = program_parameter_list();
  else
    params->next = 0;
  return params;
}

Symbol *program_parameter() {
  Symbol *param;

  param = new_parameter_symbol(identifier(), VALUE_PARAMETER);
  param->variable.type = text_type;
  return param;
}
  
void block(Symbol *sym) {
  int save_variable_offset;
  
  variable_offset = 0;
  push_symbol_table();
  insert_symbols(sym->algorithm.parameters);
  label_declaration_part(sym);
  constant_definition_part();
  type_definition_part();
  variable_declaration_part();
  if (sym->class == FUNCTION_SYMBOL)
    /* add variable for function return value */
    sym->algorithm.return_value = new_variable_symbol(sym->name, sym->algorithm.type);
  save_variable_offset = variable_offset;

  block_level++;
  sym->algorithm.algorithms = procedure_and_function_declaration_part();
  block_level--;

  variable_offset = save_variable_offset;
  sym->algorithm.statement = statement_part();
  variable_offset &= -8;
  sym->algorithm.local_size = -variable_offset;
  pop_symbol_table();
}

void label_declaration_part(Symbol *algo) {
  if (match(LABEL_TOKEN))
    label_declarations(algo);
}

void label_declarations(Symbol *algo) {
  do
    label_declaration(algo);
  while (match(COMMA_TOKEN));
  need(SEMICOLON_TOKEN);
}

void label_declaration(Symbol *algo) {
  char *lab = label();
  Symbol *sym = new_label_symbol(lab, algo);
  insert_symbol(sym);
}

void constant_definition_part() {
  if (match(CONST_TOKEN))
    constant_definitions();
}

void constant_definitions() {
  do {
    constant_definition();
    need(SEMICOLON_TOKEN);
  } while (check(IDENTIFIER_TOKEN));
}

void constant_definition() {
  Constant *cnst;
  char *id;
  
  id = identifier();
  need(EQ_TOKEN);
  cnst = constant();
  insert_symbol(new_constant_symbol(id, cnst));
}
  
void type_definition_part() {
  if (match(TYPE_TOKEN))
    type_definitions();
  if (undefined_pointer_list) {
    error("pointers undefined");
    while (undefined_pointer_list) {
      printf("\t%s\n", undefined_pointer_list->id);
      undefined_pointer_list = undefined_pointer_list->next;
    }
    undefined_pointer_list = 0;
  }
}

void type_definitions() {
  do {
    type_definition();
    need(SEMICOLON_TOKEN);
  } while (check(IDENTIFIER_TOKEN));
}

void type_definition() {
  Type *type;
  char *id;

  id = identifier();
  need(EQ_TOKEN);
  type = type_denoter();
  insert_symbol(new_type_symbol(id, type));
}

void variable_declaration_part() {
  if (match(VAR_TOKEN))
    variable_declarations();
}

void variable_declarations() {
  while (check(IDENTIFIER_TOKEN)) {
    variable_declaration();
    need(SEMICOLON_TOKEN);
  }
}

void variable_declaration() {
  Type *type;
  Identifier_List *ids;
  
  ids = identifier_list();
  need(COLON_TOKEN);
  type = type_denoter();
  if (type)
    insert_variable_symbols(ids, type);
}

Symbol_List *procedure_and_function_declaration_part() {
  Symbol_List *algorithms;
  Symbol_List *more;
  Symbol *subroutine;
  Directive defined_flag;
  
  if (match(PROCEDURE_TOKEN))
    subroutine = procedure_declaration();
  else if (match(FUNCTION_TOKEN))
    subroutine = function_declaration();
  else
    return 0;
  defined_flag = subroutine->algorithm.declared;
  more = procedure_and_function_declaration_part();
  if (defined_flag == DEFINED) {
    algorithms = new(Symbol_List);
    algorithms->symbol = subroutine;
    algorithms->next = more;
    return algorithms;
  }
  return more;
}

Symbol *procedure_declaration() {
  Symbol *proc;
  
  parameter_offset = 0;
  proc = procedure_heading();
  need(SEMICOLON_TOKEN);
  if (check(IDENTIFIER_TOKEN))
    proc->algorithm.declared = directive();
  else {
    proc->algorithm.declared = DEFINED;
    block(proc);
  }
  need(SEMICOLON_TOKEN);
  return proc;
}

Symbol *procedure_heading() {
  Symbol *proc;
  Symbol_List *parameters;
  char *id;

  id = identifier();
  proc = lookup_symbol(id);
  if (proc != 0 && proc->class == PROCEDURE_SYMBOL) {
    if (proc->algorithm.declared != FORWARD)
      error("function %s already declared", id);
  }
  else {
    if (match(LPAREN_TOKEN)) {
      parameters = formal_parameter_list();
      need(RPAREN_TOKEN);
    }
    else
      parameters = 0;
    proc = new_procedure_symbol(id, parameters);
    insert_symbol(proc);
  }
  return proc;
}

Symbol *function_declaration() {
  Symbol *func;
  
  parameter_offset = 0;
  func = function_heading();
  need(SEMICOLON_TOKEN);
  if (check(IDENTIFIER_TOKEN))
    func->algorithm.declared = directive();
  else {
    func->algorithm.declared = DEFINED;
    block(func);
  }    
  need(SEMICOLON_TOKEN);
  return func;
}

Symbol *function_heading() {
  Symbol *func;
  Symbol_List *parameters;
  char *id;
  
  id = identifier();
  func = lookup_symbol(id);
  if (func != 0 && func->class == FUNCTION_SYMBOL) {
    if (func->algorithm.declared != FORWARD)
      error("function %s already declared", id);
  }
  else {
    if (match(LPAREN_TOKEN)) {
      parameters = formal_parameter_list();
      need(RPAREN_TOKEN);
    }
    else
      parameters = 0;
    need(COLON_TOKEN);
    func = new_function_symbol(id, parameters, result_type());
    insert_symbol(func);
  }
  return func;
}

Symbol_List *formal_parameter_list() {
  switch (token_type) {
  case VAR_TOKEN:
    return variable_parameters();
  case IDENTIFIER_TOKEN:
    return value_parameters();
  case PROCEDURE_TOKEN:
    return procedure_parameter();
  case FUNCTION_TOKEN:
    return function_parameter();
  default:
    error("illegal parameter list");
  }
  return 0;
}

Symbol_List *variable_parameters() {
  next_token(); /* skip 'var' */
  return parameter_id_list(VARIABLE_PARAMETER);
}

Symbol_List *value_parameters() {
  return parameter_id_list(VALUE_PARAMETER);
}

Symbol_List *parameter_id_list(Symbol_Class class) {
  Symbol_List *syms = new(Symbol_List);
  char *id = identifier();
  syms->symbol = new_parameter_symbol(id, class);
  switch (token_type) {
  case COMMA_TOKEN:
    next_token();
    syms->next = parameter_id_list(class);
    syms->symbol->variable.type = syms->next->symbol->variable.type;
    break;
  case COLON_TOKEN:
    next_token();
    syms->symbol->variable.type = type_identifier();
    if (match(SEMICOLON_TOKEN))
      syms->next = formal_parameter_list();
    else
      syms->next = 0;
    break;
  default:
    syntax_error(3);
    syms->next = 0;
  }
  return syms;
}

Symbol_List *procedure_parameter() {
  Symbol_List *syms;
  Symbol_List *parameters;
  char *id;
  
  next_token();
  id = identifier();
  if (match(LPAREN_TOKEN)) {
    parameters = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  else
    parameters = 0;
  
  syms = new(Symbol_List);
  syms->symbol = new_procedure_parameter_symbol(id, parameters);
  if (match(SEMICOLON_TOKEN))
    syms->next = formal_parameter_list();
  else
    syms->next = 0;
  return syms;
}

Symbol_List *function_parameter() {
  Symbol_List *syms;
  Symbol_List *parameters;
  Type *type;
  char *id;

  next_token();
  id = identifier();
  if (match(LPAREN_TOKEN)) {
    parameters = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  else
    parameters = 0;
  need(COLON_TOKEN);
  type = result_type();

  syms = new(Symbol_List);
  syms->symbol = new_function_parameter_symbol(id, parameters, type);
  if (match(SEMICOLON_TOKEN))
    syms->next = formal_parameter_list();
  else
    syms->next = 0;
  return syms;
}

Directive directive() {
  char *direc = identifier();
  if (strcmp(direc, "forward") == 0)
    return FORWARD;
  if (strcmp(direc, "external") == 0)
    return EXTERNAL;
  error("illegal procedure/function directive: %s", direc);
  return EXTERNAL;
}

Statement *statement_part() {
  return compound_statement(0);
}

char *identifier() {
  char *id = 0;

  if (!check(IDENTIFIER_TOKEN))
    /* it might be useful to generate a unique identifier here */
    error("identifier expected near %s", token);
  else {
    id = strdup(token);
    next_token();
  }
  return id;
}

Identifier_List *identifier_list() {
  Identifier_List *ids;

  ids = new(Identifier_List);
  ids->id = identifier();
  if (match(COMMA_TOKEN))
    ids->next = identifier_list();
  else
    ids->next = 0;
  return ids;
}

char *label() {
  int ilab;
  char buff[24];

  if (!check(INTEGER_TOKEN))
    error("label must be a integer");
  ilab = atoi(token);
  next_token();
  sprintf(buff, "%d", ilab);
  return strdup(buff);
}
