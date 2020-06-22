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
Symbol *procedure_heading(void);
void directive(Symbol *);
void procedure_block(Symbol *);
void function(void);
Symbol *function_heading(void);
void function_block(Symbol *);
Symbol_List *formal_parameter_list(void);
Symbol_List *parameters(Symbol_Class);
Symbol_List *procedural_parameter_specification(void);
Symbol *procedure_parameter(void);
Symbol_List *functional_parameter_specification(void);
Symbol *function_parameter(void);
Block *block(void);
void label_declarations(void);
void label(void);
void constant_definitions(void);
void constant_definition(void);
void type_definitions(void);
void type_definition(void);
Type *type(void);
Type *subrange_type(void);
Type *enumerated_type(void);
Type *pointer_type(void);
Type *type_identifier(char *);
Type *structured_type(void);
Type *unpacked_structured_type(void);
Type *array_type(void);
Type *array_type_middle(void);
Type *array_type_end(void);
Type *record_type(void);
Type *field_list(void);
Symbol_List *fixed_part(void);
Symbol_List *record_sections(void);
Variant_Part *variant_part(void);
Symbol *variant_selector(void);
Variant_List *variant_list(void);
Variant *variant(void);
Type *set_type(void);
Type *file_type(void);
Symbol_List *variable_declarations(void);
void variable_declaration(void);
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

void program_block(Symbol *prog) {
  lexical_level++;
  push_symbol_table();
  prog->prog.block = block();
  pop_symbol_table();
  lexical_level--;
}

void procedure() {
  Symbol *proc = procedure_heading();
  
  need(SEMICOLON_TOKEN);
  if (check(IDENTIFIER_TOKEN))
    directive(proc);
  else
    procedure_block(proc);
  need(SEMICOLON_TOKEN);
}

Symbol *procedure_heading() {
  Symbol_List *args = 0;
  Symbol *sym;
  char *id;
  
  id = identifier();

  /* check for forward declaration */

  sym = lookup(id);
  if (sym && sym->class == PROCEDURE_SYMBOL && sym->func.state == FORWARD)
    return sym;

  /* not declared */
  
  if (match(LPAREN_TOKEN)) {
    args = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  sym = new_procedure_symbol(id, args);
  insert(sym);
  return sym;
}

void procedure_block(Symbol *proc) {
  lexical_level++;
  push_symbol_table();
  add_parameters(proc->proc.params);
  block();
  pop_symbol_table();
  lexical_level--;
}

void function() {
  Symbol *func = function_heading();
  need(SEMICOLON_TOKEN);
  if (check(IDENTIFIER_TOKEN))
    directive(func);
  else
    function_block(func);
  need(SEMICOLON_TOKEN);
}

void function_block(Symbol *func) {
  lexical_level++;
  push_symbol_table();
  add_parameters(func->func.params);
  block();
  pop_symbol_table();
  lexical_level--;
}

Symbol *function_heading() {
  Symbol_List *args = 0;
  Symbol *sym;
  Type *t;
  char *id;

  id = identifier();
  
  /* check for forward declaration */
  
  sym = lookup(id); 
  if (sym && sym->class == FUNCTION_SYMBOL && sym->func.state == FORWARD)
    return sym;

  /* not declared */
  
  if (match(LPAREN_TOKEN)) {
    args = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  need(COLON_TOKEN);
  t = type_identifier(identifier());
  sym = new_function_symbol(id, args, t);
  insert(sym);
  return sym;
}

void directive(Symbol *sym) {
  if (strcmp(token, "forward") == 0) {
    if (sym->class == PROCEDURE_SYMBOL)
      sym->proc.state = FORWARD;
    else if (sym->class == FUNCTION_SYMBOL)
      sym->func.state = FORWARD;
    else
      internal_error();
  }
  else if (strcmp(token, "external") == 0) {
    if (sym->class == PROCEDURE_SYMBOL)
      sym->proc.state = EXTERNAL;
    else if (sym->class == FUNCTION_SYMBOL)
      sym->func.state = EXTERNAL;
    else
      internal_error();
  }
  next_token();
}

Symbol_List *formal_parameter_list() {
  if (match(PROCEDURE_TOKEN))
    return procedural_parameter_specification();
  if (match(FUNCTION_TOKEN))
    return functional_parameter_specification();
  if (match(VAR_TOKEN))
    return parameters(VARARG_SYMBOL);
  return parameters(VALARG_SYMBOL);
}

Symbol_List *parameters(Symbol_Class class) {
  Symbol_List *syms = new(Symbol_List);
  char *id, *tid;

  id = identifier();
  if (match(COMMA_TOKEN)) {
    syms->next = parameters(class);
    syms->sym = new_parameter_symbol(id, syms->next->sym->type, class);
  }
  else if (match(COLON_TOKEN)) {
    tid = identifier();
    syms->sym = new_parameter_symbol(id, type_identifier(tid), class);
    if (match(SEMICOLON_TOKEN))
      syms->next = formal_parameter_list();
    else
      syms->next = 0;
  }
  else
    error("syntax error in formal parameters near %s", token);
  return syms;
}

Symbol_List *procedural_parameter_specification() {
  Symbol_List *syms = new(Symbol_List);
  syms->sym = procedure_parameter();
  if (match(SEMICOLON_TOKEN))
    syms->next = formal_parameter_list();
  else
    syms->next = 0;
  return syms;
}

Symbol *procedure_parameter() {
  Symbol_List *args = 0;
  char *id;

  id = identifier();
  if (match(LPAREN_TOKEN)) {
    args = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  return new_procedure_parameter_symbol(id, args);
}

Symbol_List *functional_parameter_specification() {
  Symbol_List *syms = new(Symbol_List);
  syms->sym = function_parameter();
  if (match(SEMICOLON_TOKEN))
    syms->next = formal_parameter_list();
  else
    syms->next = 0;
  return syms;
}

Symbol *function_parameter() {
  Symbol_List *args = 0;
  Type *t;
  char *id;
  
  
  id = identifier();
  if (match(LPAREN_TOKEN)) {
    args = formal_parameter_list();
    need(RPAREN_TOKEN);
  }
  need(COLON_TOKEN);
  t = type_identifier(identifier());
  return new_function_parameter_symbol(id, args, t);
}
    
Block *block() {
  Symbol_List *syms;
  
  if (match(LABEL_TOKEN))
    label_declarations();
  if (match(CONST_TOKEN))
    constant_definitions();
  if (match(TYPE_TOKEN))
    type_definitions();
  if (match(VAR_TOKEN)) {
    syms = variable_declarations();
    insert_symbols(syms);
  }
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
  char *id;

  id = identifier();
  need(EQ_TOKEN);
  cnst = constant();
  need(SEMICOLON_TOKEN);
  insert(new_constant_symbol(id, cnst));
}

void type_definitions() {
  while (check(IDENTIFIER_TOKEN))
    type_definition();
}

void type_definition() {
  Symbol *sym;
  Type *t;
  char *id;
  
  id = identifier();
  need(EQ_TOKEN);
  t = type();
  need(SEMICOLON_TOKEN);
  sym = lookup(id);
  if (sym && sym->class == TYPE_SYMBOL && sym->type->class == FORWARD_TYPE)
    sym->type = t;
  else
    insert(new_type_symbol(id, t));
}

Type *type() {
  Symbol *sym;

  switch (token_type) {
  case IDENTIFIER_TOKEN:
    sym = lookup(token);
    if (sym) {
      if (sym->class == TYPE_SYMBOL) {
	next_token();
	return sym->type;
      }
      else if (sym->class == CONSTANT_SYMBOL)
	return subrange_type();
    }
    error("%s is not a type", token);
    break;
  case LPAREN_TOKEN:
    return enumerated_type();
  case INTEGER_TOKEN:
  case CHAR_TOKEN:
  case PLUS_TOKEN:
  case MINUS_TOKEN:
    return subrange_type();
  case ARROW_TOKEN:
    return pointer_type();
  case PACKED_TOKEN:
  case ARRAY_TOKEN:
  case RECORD_TOKEN:
  case SET_TOKEN:
  case FILE_TOKEN:
    return structured_type();
  default:
    ;
  }
  error("type expected near %s", token);
  return 0;
}

Type *subrange_type() {
  Constant *c1, *c2;

  c1 = constant();
  need(ELLIPSIS_TOKEN);
  c2 = constant();
  return new_subrange_type(c1, c2);
}

Type *enumerated_type() {
  Identifier_List *ids;

  need(LPAREN_TOKEN);
  ids = identifier_list();
  need(RPAREN_TOKEN);
  return new_enumerated_type(ids);
}

Type *pointer_type() {
  Type *t;
  char *id;

  need(ARROW_TOKEN);
  id = identifier();
  t = type_identifier(id);
  if (t == 0) {
    t = forward_type;
    insert(new_type_symbol(id, t));
  }
  return new_pointer_type(t);
}

Type *type_identifier(char *id) {
  Symbol *sym = lookup(id);

  if (sym && sym->class == TYPE_SYMBOL)
    return sym->type;
  return 0;
}

Type *structured_type() {
  if (match(PACKED_TOKEN))
    return make_packed(unpacked_structured_type());
  return unpacked_structured_type();
}

Type *unpacked_structured_type() {
  if (match(ARRAY_TOKEN))
    return array_type();
  if (match(RECORD_TOKEN))
    return record_type();
  if (match(SET_TOKEN))
    return set_type();
  if (match(FILE_TOKEN))
    return file_type();
  return 0;
}

Type *array_type() {
  need(LBRACK_TOKEN);
  return array_type_middle();
}

Type *array_type_middle() {
  Type *it, *ct;

  it = type();
  if (match(COMMA_TOKEN))
    ct = array_type_middle();
  else
    ct = array_type_end();
  return new_array_type(it, ct);
}

Type *array_type_end() {
  need(RBRACK_TOKEN);
  need(OF_TOKEN);
  return type();
}

Type *record_type() {
  Type *rt;

  rt = field_list();
  need(END_TOKEN);
  return rt;
}

Type *field_list() {
  Type *t = new_type(RECORD_TYPE);

  if (check(CASE_TOKEN)) {
    t->record.fixed = 0;
    t->record.variant = variant_part();
  }
  else {
    t->record.fixed = fixed_part();
    if (check(CASE_TOKEN))
      t->record.variant = variant_part();
    else
      t->record.variant = 0;
  }
  return t;
}

Symbol_List *fixed_part() {
  if (check(END_TOKEN) || check(RPAREN_TOKEN) || check(CASE_TOKEN))
    return 0;
  return record_sections();
}

Symbol_List *record_sections() {
  Symbol_List *recs = new(Symbol_List);
  char *id = identifier();
  if (match(COMMA_TOKEN)) {
    recs->next = record_sections();
    recs->sym = new_field_symbol(id, recs->next->sym->type);
    return recs;
  }
  need(COLON_TOKEN);
  recs->sym = new_field_symbol(id, type());
  if (match(SEMICOLON_TOKEN)) {
    recs->next = fixed_part();
    return recs;
  }
  recs->next = 0;
  return recs;
}

Variant_Part *variant_part() {
  Variant_Part *vp = new(Variant_Part);
  need(CASE_TOKEN);
  vp->selector = variant_selector();
  need(OF_TOKEN);
  vp->variants = variant_list();
  return vp;
}

Symbol *variant_selector() {
  char *id1, *id2;
  Type *t;

  id1 = 0;
  id2 = identifier();
  if (match(COLON_TOKEN)) {
    id1 = id2;
    id2 = identifier();
  }
  t = type_identifier(id2);
  if (t == 0)
    error("%s is not a type", id2);
  return new_field_symbol(id1, t);
}

Variant_List *variant_list() {
  Variant_List *vl;
  if (check(END_TOKEN) || check(RPAREN_TOKEN) || check(CASE_TOKEN))
    return 0;
  vl = new(Variant_List);
  vl->variant = variant();
  if (match(SEMICOLON_TOKEN))
    vl->next = variant_list();
  else
    vl->next = 0;
  return vl;
}

Variant *variant() {
  Variant *var = new(Variant);
  var->cnsts = constant_list();
  need(COLON_TOKEN);
  need(LPAREN_TOKEN);
  var->fields = field_list();
  need(RPAREN_TOKEN);
  return var;
}

Type *set_type() {
  need(OF_TOKEN);
  return new_set_type(type());
}

Type *file_type() {
  need(OF_TOKEN);
  return new_file_type(type());
}

Symbol_List *variable_declarations() {
  Symbol_List *syms = 0;
  char *id;
  
  if (check(IDENTIFIER_TOKEN)) {
    syms = new(Symbol_List);
    id = identifier();
    if (match(COMMA_TOKEN)) {
      syms->next = variable_declarations();
      syms->sym = new_variable_symbol(id, syms->next->sym->var.type);
    }
    else if (match(COLON_TOKEN)) {
      syms->sym = new_variable_symbol(id, type());
      need(SEMICOLON_TOKEN);
      syms->next = variable_declarations();
    }
    else
      error("syntax error near %s", token);
  }
  return syms;
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
