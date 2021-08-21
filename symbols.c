#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

#define HASH_SIZE 1021

Type *boolean_type;
Type *char_type;
Type *integer_type;
Type *real_type;
Type *text_type;
Type *nil_type;
Type *empty_set_type;
Constant *nil_constant;

int symbol_level = 0;
long parameter_offset;

Symbol_List *symbol_table[HASH_SIZE];

Symbol *new_algorithm_symbol(char *, Symbol_Class, Symbol_List *, Type *);
Symbol *new_type_symbol(char *, Type *);
Symbol *new_standard_procedure(char *, Standard_Procedure);
Symbol *new_standard_function(char *, Standard_Function);
Constant *new_nil_constant(void);
unsigned hash(char *);
void calculate_parameter_offsets(Symbol_List *);
void insert_fields(Symbol_List *);

void initialize_symbols() {
  boolean_type = new_ordinal_type(0, 0, 1);
  insert_symbol(new_type_symbol("boolean", boolean_type));
  char_type = new_ordinal_type(0, 0, CHAR_MAXIMUM);
  insert_symbol(new_type_symbol("char", char_type));
  integer_type = new_ordinal_type(0, INTEGER_MINIMUM, INTEGER_MAXIMUM);
  insert_symbol(new_type_symbol("integer", integer_type));
  real_type = new_real_type();
  insert_symbol(new_type_symbol("real", real_type));
  text_type = new_file_type(true, char_type);
  insert_symbol(new_type_symbol("text", text_type));
  empty_set_type = new_set_type(false, 0);
  nil_type = new_pointer_type(0);
  nil_constant = new_nil_constant();
  
  insert_symbol(new_constant_symbol("false", new_ordinal_constant(boolean_type, 0)));
  insert_symbol(new_constant_symbol("true", new_ordinal_constant(boolean_type, 1)));
  insert_symbol(new_constant_symbol("maxint", new_ordinal_constant(integer_type, INTEGER_MAXIMUM)));

  insert_symbol(new_standard_procedure("rewrite", REWRITE_PROCEDURE));
  insert_symbol(new_standard_procedure("reset", RESET_PROCEDURE));
  insert_symbol(new_standard_procedure("put", PUT_PROCEDURE));
  insert_symbol(new_standard_procedure("get", GET_PROCEDURE));
  insert_symbol(new_standard_procedure("read", READ_PROCEDURE));
  insert_symbol(new_standard_procedure("readln", READLN_PROCEDURE));
  insert_symbol(new_standard_procedure("write", WRITE_PROCEDURE));
  insert_symbol(new_standard_procedure("writeln", WRITELN_PROCEDURE));
  insert_symbol(new_standard_procedure("page", PAGE_PROCEDURE));
  insert_symbol(new_standard_procedure("new", NEW_PROCEDURE));
  insert_symbol(new_standard_procedure("dispose", DISPOSE_PROCEDURE));
  insert_symbol(new_standard_procedure("pack", PACK_PROCEDURE));
  insert_symbol(new_standard_procedure("unpack", UNPACK_PROCEDURE));
  insert_symbol(new_standard_procedure("argv", ARGV_PROCEDURE));
  insert_symbol(new_standard_procedure("flush", FLUSH_PROCEDURE));
  insert_symbol(new_standard_procedure("close", CLOSE_PROCEDURE));
  
  insert_symbol(new_standard_function("abs", ABS_FUNCTION));
  insert_symbol(new_standard_function("sqr", SQR_FUNCTION));
  insert_symbol(new_standard_function("sin", SIN_FUNCTION));
  insert_symbol(new_standard_function("cos", COS_FUNCTION));
  insert_symbol(new_standard_function("exp", EXP_FUNCTION));
  insert_symbol(new_standard_function("ln", LN_FUNCTION));
  insert_symbol(new_standard_function("sqrt", SQRT_FUNCTION));
  insert_symbol(new_standard_function("arctan", ARCTAN_FUNCTION));
  insert_symbol(new_standard_function("trunc", TRUNC_FUNCTION));
  insert_symbol(new_standard_function("round", ROUND_FUNCTION));
  insert_symbol(new_standard_function("ord", ORD_FUNCTION));
  insert_symbol(new_standard_function("chr", CHR_FUNCTION));
  insert_symbol(new_standard_function("succ", SUCC_FUNCTION));
  insert_symbol(new_standard_function("pred", PRED_FUNCTION));
  insert_symbol(new_standard_function("odd", ODD_FUNCTION));
  insert_symbol(new_standard_function("eof", EOF_FUNCTION));
  insert_symbol(new_standard_function("eoln", EOLN_FUNCTION));
  /* non-standard functions */
  insert_symbol(new_standard_function("argc", ARGC_FUNCTION));
}

void push_symbol_table() {
  symbol_level++;
}

void pop_symbol_table() {
  Symbol_List *sptr;
  int i;
  
  symbol_level--;

  for (i = 0; i < HASH_SIZE; i++) {
    for (sptr = symbol_table[i]; sptr; sptr = sptr->next) {
      if (sptr->level <= symbol_level) {
	break;
      }
    }
    symbol_table[i] = sptr;
  }
}
  
Symbol *new_program_symbol(char *name, Symbol_List *parameters) {
  return new_algorithm_symbol(name, PROGRAM_SYMBOL, parameters, 0);
}

Symbol *new_procedure_symbol(char *name, Symbol_List *parameters) {
  return new_algorithm_symbol(name, PROCEDURE_SYMBOL, parameters, 0);
}

Symbol *new_procedure_parameter_symbol(char *name, Symbol_List *parameters) {
  return new_algorithm_symbol(name, PROCEDURE_PARAMETER, parameters, 0);
}

Symbol *new_function_parameter_symbol(char *name, Symbol_List *parameters, Type *type) {
  return new_algorithm_symbol(name, FUNCTION_PARAMETER, parameters, type);
}

Symbol *new_function_symbol(char *name, Symbol_List *parameters, Type *type) {
  return new_algorithm_symbol(name, FUNCTION_SYMBOL, parameters, type);
}

Symbol *new_algorithm_symbol(char *name, Symbol_Class class, Symbol_List *parameters, Type *type) {
  Symbol *sym = new_symbol(name, class);
  sym->algorithm.local_size = 0;
  sym->algorithm.parameter_size = 0;
  sym->algorithm.declared = UNKNOWN;
  parameter_offset = 0;
  calculate_parameter_offsets(parameters);
  sym->algorithm.parameter_size = parameter_offset;
  sym->algorithm.parameters = parameters;
  sym->algorithm.algorithms = 0;
  sym->algorithm.statement = 0;
  sym->algorithm.type = type;
  sym->algorithm.return_value = 0;
  return sym;
}

void calculate_parameter_offsets(Symbol_List *params) {
  while (params) {
    params->symbol->variable.offset = parameter_offset;
    if (params->symbol->class == VALUE_PARAMETER)
      parameter_offset = align_up(parameter_offset, params->symbol->variable.type->size);
    else
      parameter_offset = align_up(parameter_offset, POINTER_SIZE);
    params = params->next;
  }
}

Symbol *new_constant_symbol(char *name, Constant *cnst) {
  Symbol *sym = new_symbol(name, CONSTANT_SYMBOL);
  sym->constant = cnst;
  return sym;
}

Symbol *new_type_symbol(char *name, Type *type) {
  Symbol *sym;

  define_undefined_pointer(name, type);
  sym = new_symbol(name, TYPE_SYMBOL);
  sym->type = type;
  return sym;
}

Symbol *new_field_symbol(char *name) {
  Symbol *sym = new_symbol(name, FIELD_SYMBOL);
  sym->variable.type = 0; /* filled in later */
  sym->variable.offset = 0;
  return sym;
}

Symbol *new_standard_procedure(char *name, Standard_Procedure sp) {
  Symbol *sym = new_symbol(name, STANDARD_PROCEDURE);
  sym->stdproc = sp;
  return sym;
}

Symbol *new_standard_function(char *name, Standard_Function sf) {
  Symbol *sym = new_symbol(name, STANDARD_FUNCTION);
  sym->stdfunc = sf;
  return sym;
}

Constant *new_nil_constant() {
  Constant *cnst = new(Constant);
  cnst->type = nil_type;
  return cnst;
}

void insert_symbols(Symbol_List *syms) {
  while (syms) {
    insert_symbol(syms->symbol);
    syms = syms->next;
  }
}

void insert_symbol(Symbol *sym) {
  Symbol_List *syms = new(Symbol_List);
  unsigned h = hash(sym->name);
  syms->symbol = sym;
  syms->level = symbol_level;
  syms->next = symbol_table[h];
  symbol_table[h] = syms;
}

void insert_variable_symbols(Identifier_List *ids, Type *type) {
  Symbol *sym;
  
  while (ids) {
    sym = new_variable_symbol(ids->id, type);
    insert_symbol(sym);
    ids = ids->next;
  }
}


void insert_record_fields(Type *t) {
  Variant_List *variants;
  
  insert_fields(t->record.fields);
  if (t->record.variant) {
    if (t->record.variant->selector->name)
      insert_symbol(t->record.variant->selector);
    for (variants = t->record.variant->variants; variants; variants = variants->next)
      insert_record_fields(variants->fields);
  }
}

void insert_fields(Symbol_List *fields) {
  while (fields) {
    insert_symbol(fields->symbol);
    fields = fields->next;
  }
}

/*
three "levels" of symbols:
block level -- which stack display                        (part of Symbol)
statement level -- stack used for with statements
symbol level -- level of symbol for adding and removing   (part of Symbol_List)
                symbols from the symbol table
*/


Symbol *lookup_symbol(char *name) {
  Symbol_List *syms;
  unsigned h = hash(name);
  
  for  (syms = symbol_table[h]; syms; syms = syms->next) {
    if (strcmp(syms->symbol->name, name) == 0)
      return syms->symbol;
  }
  return 0;
}

Symbol *lookup_symbol_local(char *name) {
  Symbol *sym = lookup_symbol(name);

  if (sym)
    if (sym->block_level == block_level)
      return sym;
  return 0;
}

unsigned hash(char *s) {
  unsigned h = 0;

  while (*s)
    h = ((h<<24)|(h>>8))^*s++;
  return h % HASH_SIZE;
}
