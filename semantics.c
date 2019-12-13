#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

Type *define_enumerated(Identifier_List *, Ordinal);

Constant *new_ordinal_constant(Type *t, Ordinal i) {
  Constant *cnst = new(Constant);

  cnst->type = t;
  cnst->ordinal = i;
  return cnst;
}

Constant *new_real_constant(Real r) {
  Constant *cnst = new(Constant);

  cnst->type = real_type;
  cnst->real = r;
  return cnst;
}

Type *new_type(Type_Class t) {
  Type *type = new(Type);
  type->class = t;
  return type;
}

Type *new_ordinal_type(Type *s, Ordinal low, Ordinal high) {
  Type *t = new(Type);

  t->class = ORDINAL_TYPE;
  if (s == 0)
    t->ordinal.base = t;
  else
    t->ordinal.base = s;
  t->ordinal.low = low;
  t->ordinal.high = high;
  return t;
}

Type *new_subrange_type(Constant *c1, Constant *c2) {
  if (c1->type->class == ORDINAL_TYPE && c2->type->class == ORDINAL_TYPE)
    return new_ordinal_type(c1->type, c1->ordinal, c2->ordinal);
  error("illegal types in subrange");
  return 0;
}

Type *new_enumerated_type(Identifier_List *ids) {
  return define_enumerated(ids, 0);
}

Type *define_enumerated(Identifier_List *ids, Ordinal count) {
  Type *t;

  if (ids == 0)
    return new_ordinal_type(0, 0, count-1);
  t = define_enumerated(ids->next, count+1);
  insert(new_constant_symbol(ids->id, new_ordinal_constant(t, count)));
  return t;
}

Type *make_string_type(char *s) {
  return make_packed(new_array_type(new_ordinal_type(integer_type, 0, strlen(s)-1),
				    char_type));
}

Type *new_array_type(Type *i, Type *c) {
  Type *a = new_type(ARRAY_TYPE);
  a->array.index = i;
  a->array.component = c;
  return a;
}

Type *new_pointer_type(Type *t) {
  Type *pt = new_type(POINTER_TYPE);
  pt->subtype = t;
  return pt;
}

Type *new_set_type(Type *t) {
  Type *st = new_type(SET_TYPE);
  st->subtype = t;
  return st;
}

Type *new_file_type(Type *t) {
  Type *ft = new_type(FILE_TYPE);
  ft->subtype = t;
  return ft;
}

Type *make_packed(Type *t) {
  Type *pt;

  switch (t->class) {
  case ARRAY_TYPE:
    pt = new_type(PACKED_ARRAY_TYPE);
    pt->array.index = t->array.index;
    pt->array.component = pt->array.component;
    break;
  case RECORD_TYPE:
    pt = new_type(PACKED_RECORD_TYPE);
    pt->record.fixed = t->record.fixed;
    pt->record.variant = t->record.variant;
    break;
  case SET_TYPE:
    pt = new_type(PACKED_SET_TYPE);
    pt->subtype = t->subtype;
    break;
  case FILE_TYPE:
    pt = new_type(PACKED_FILE_TYPE);
    pt->subtype = t->subtype;
    break;
  default:
    error("illegal packed type");
    pt = 0;
  }
  return pt;
}

Symbol *new_symbol(char *id, Symbol_Class class) {
  Symbol *sym = new(Symbol);
  sym->class = class;
  sym->name = id;
  return sym;
}

Symbol *new_procedure_symbol(char *id, Symbol_List *args) {
  Symbol *sym = new(Symbol);

  sym->class = PROCEDURE_SYMBOL;
  sym->name = id;
  sym->proc.params = args;
  sym->proc.block = 0;
  sym->proc.state = DEFINED;
  return sym;
}

Symbol *new_function_symbol(char *id, Symbol_List *args, Type *t) {
  Symbol *sym = new(Symbol);

  sym->class = FUNCTION_SYMBOL;
  sym->name = id;
  sym->func.params = args;
  sym->func.block = 0;
  sym->func.state = DEFINED;
  sym->func.type = t;
  return sym;
}

Symbol *new_parameter_symbol(char *id, Type *type, Symbol_Class class) {
  Symbol *sym = new(Symbol);
  sym->class = class;
  sym->name = id;
  sym->var.level = lexical_level;
  sym->var.type = type;
  return sym;
}

  
Symbol *new_field_symbol(char *id, Type *t) {
  Symbol *sym = new_symbol(id, FIELD_SYMBOL);
  sym->name = id;
  sym->type = t;
  return sym;
}

Symbol *new_variable_symbol(char *id, Type *t) {
  Symbol *sym = new_symbol(id, VARIABLE_SYMBOL);
  sym->var.level = lexical_level;
  sym->var.type = t;
  return sym;
}
