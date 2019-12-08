#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

#define HASH_SIZE 1021

int symbol_level = 0;

Symbol_List *symbol_table[HASH_SIZE];

Type *nil_type;
Type *boolean_type;
Type *char_type;
Type *integer_type;
Type *real_type;
Type *text_type;
Type *forward_type;

Constant *nil_constant;

unsigned hash(char *);
void insert(Symbol *);

Symbol *new_type_symbol(char *, Type *);
Symbol *new_constant_symbol(char *, Constant *);
Symbol *new_standard_procedure(char *, Standard_Procedure);
Symbol *new_standard_function(char *, Standard_Function);

void initialize_symbols() {
  forward_type = new_type(FORWARD_TYPE);
  nil_type = new_type(NIL_TYPE);

  boolean_type = new_ordinal_type(0, 0, 1);
  insert(new_type_symbol("boolean", boolean_type));
  insert(new_constant_symbol("false", new_ordinal_constant(boolean_type, 0)));
  insert(new_constant_symbol("true", new_ordinal_constant(boolean_type, 1)));

  char_type = new_ordinal_type(0, 0, CHAR_MAX);
  insert(new_type_symbol("char", char_type));

  integer_type = new_ordinal_type(0, INTEGER_MIN, INTEGER_MAX);
  insert(new_type_symbol("integer", integer_type));
  insert(new_constant_symbol("maxint", new_ordinal_constant(integer_type, INTEGER_MAX)));

  real_type = new(Type);
  real_type->class = REAL_TYPE;
  insert(new_type_symbol("real", real_type));

  text_type = new(Type);
  text_type->class = TEXT_TYPE;
  insert(new_type_symbol("text", text_type));

  insert(new_standard_procedure("rewrite", REWRITE_PROCEDURE));
  insert(new_standard_procedure("put", PUT_PROCEDURE));
  insert(new_standard_procedure("reset", RESET_PROCEDURE));
  insert(new_standard_procedure("get", GET_PROCEDURE));
  insert(new_standard_procedure("read", READ_PROCEDURE));
  insert(new_standard_procedure("readln", READLN_PROCEDURE));
  insert(new_standard_procedure("write", WRITE_PROCEDURE));
  insert(new_standard_procedure("writeln", WRITELN_PROCEDURE));
  insert(new_standard_procedure("page", PAGE_PROCEDURE));
  insert(new_standard_procedure("new", NEW_PROCEDURE));
  insert(new_standard_procedure("dispose", DISPOSE_PROCEDURE));
  insert(new_standard_procedure("pack", PACK_PROCEDURE));
  insert(new_standard_procedure("unpack", UNPACK_PROCEDURE));

  insert(new_standard_function("abs", ABS_FUNCTION));
  insert(new_standard_function("sqr", SQR_FUNCTION));
  insert(new_standard_function("sin", SIN_FUNCTION));
  insert(new_standard_function("cos", COS_FUNCTION));
  insert(new_standard_function("exp", EXP_FUNCTION));
  insert(new_standard_function("ln", LN_FUNCTION));
  insert(new_standard_function("sqrt", SQRT_FUNCTION));
  insert(new_standard_function("arctan", ARCTAN_FUNCTION));
  insert(new_standard_function("trunc", TRUNC_FUNCTION));
  insert(new_standard_function("round", ROUND_FUNCTION));
  insert(new_standard_function("ord", ORD_FUNCTION));
  insert(new_standard_function("chr", CHR_FUNCTION));
  insert(new_standard_function("succ", SUCC_FUNCTION));
  insert(new_standard_function("pred", PRED_FUNCTION));
  insert(new_standard_function("odd", ODD_FUNCTION));
  insert(new_standard_function("eoln", EOLN_FUNCTION));
  insert(new_standard_function("eof", EOF_FUNCTION));
}

void push_symbol_table() {
  symbol_level++;
}

void pop_symbol_table() {
  Symbol_List *sptr;
  int i;

  symbol_level--;
  for (i = 0; i < HASH_SIZE; i++) {
    for (sptr = symbol_table[i]; sptr; sptr = sptr->next)
      if (sptr->level <= symbol_level)
	break;
    symbol_table[i] = sptr;
  }
}

Symbol *new_program_symbol(char *name, Identifier_List *params) {
  Symbol *prog = new(Symbol);
  
  prog->class = PROGRAM_SYMBOL;
  prog->name = name;
  prog->prog.params = params;
  return prog;
}

Symbol *new_constant_symbol(char *name, Constant *cnst) {
  Symbol *sym = new(Symbol);

  sym->class = CONSTANT_SYMBOL;
  sym->name = name;
  sym->cnst = cnst;
  return sym;
}

Symbol *new_type_symbol(char *name, Type *t) {
  Symbol *sym = new(Symbol);
  sym->class = TYPE_SYMBOL;
  sym->name = name;
  sym->type = t;
  return sym;
}   

Symbol *new_val_param_symbol(char *name, Type *type) {
  Symbol *sym = new(Symbol);
  sym->class = VALARG_SYMBOL;
  sym->name = name;
  sym->var.level = lexical_level;
  sym->var.type = type;
  return sym;
}

Symbol *new_standard_procedure(char *name, Standard_Procedure proc) {
  Symbol *sym = new(Symbol);

  sym->class = STDPROC_SYMBOL;
  sym->name = name;
  sym->stdproc = proc;
  return sym;
}

Symbol *new_standard_function(char *name, Standard_Function func) {
  Symbol *sym = new(Symbol);

  sym->class = STDFUNC_SYMBOL;
  sym->name = name;
  sym->stdfunc = func;
  return sym;
}

void insert_parameters(Symbol_List *syms) {
  for (; syms; syms = syms->next)
    insert(syms->sym);
}

void insert(Symbol *sym) {
  Symbol_List *sptr = new(Symbol_List);
  unsigned h = hash(sym->name);
  
  sptr->sym = sym;
  sptr->level = symbol_level;
  sptr->next = symbol_table[h];
  symbol_table[h] = sptr;
}

Symbol *lookup(char *id) {
  Symbol_List *syms;
  unsigned h;

  h = hash(id);
  for (syms = symbol_table[h]; syms; syms = syms->next)
    if (strcmp(syms->sym->name, id) == 0)
      return syms->sym;
  return 0;
}

unsigned hash(char *s) {
  unsigned h = 0;

  for (; *s; s++) {
    h = (h<<4) + *s;
    h = 0xffffff&(h^(h>>24));
  }
  return h%HASH_SIZE;
}

