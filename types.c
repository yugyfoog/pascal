#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

Type *new_file_type(bool, Type *);
Type *subrange_type(void);
Type *enumerated_type(void);
Type *new_enumerated_type(Identifier_List *);
long count_ids(Identifier_List *);
Type *pointer_type(void);
void add_undefined_pointer(char *, Type *);
Undefined_Pointer_List *search_undefined_pointers(Undefined_Pointer_List *ups,
						       char *, Type *);
Type *structured_type(void);
Type *packed_structured_type(void);
Type *array_type(void);
Type *array_index(void);
Type *index_type(void);
Type *array_component(void);
Type *record_type(void);
void set_parents(Type *, Type *);
void set_field_parents(Symbol_List *, Type *);
Type *record_body(void);
Symbol_List *field_list(void);
Variant_Part *variant_part(void);
Symbol *variant_selector(void);
Symbol *new_variant_selector(char *, char *);
Variant_List *variant_list(void);
Variant_Part *new_variant_part(Symbol *, Variant_List *);
Type *set_type(void);
Type *file_type(void);

long calculate_record_offsets(Type *, long);
long calculate_field_offsets(Symbol_List *, long);
long calculate_variant_offsets(Variant_Part *, long);

Undefined_Pointer_List *undefined_pointer_list = 0;

/* the result types for functions
   can only be a simple-type
   or a pointer-type. */
 
Type *result_type() {
  Type *type = type_identifier();
  if (type->class != ORDINAL_TYPE
      && type->class != REAL_TYPE
      && type->class != POINTER_TYPE)
    error("illegal type for function");
  return type;
}
  
Type *type_denoter() {
  Symbol *sym;
  
  switch (token_type) {
  case IDENTIFIER_TOKEN:
    sym = lookup_symbol(token);
    if (sym == 0)
      error("%s undefined", token);
    else {
      if (sym->class == TYPE_SYMBOL) {
	next_token();
	return sym->type;
      }
      if (sym->class == CONSTANT_SYMBOL)
	return subrange_type();
    }
    break;
  case PACKED_TOKEN:
    return packed_structured_type();
  case INTEGER_TOKEN:
  case REAL_TOKEN:
  case STRING_TOKEN: /* includes character constants */
  case PLUS_TOKEN:
  case MINUS_TOKEN:
    return subrange_type();
  case LPAREN_TOKEN:
    return enumerated_type();
  case ARROW_TOKEN:
    return pointer_type();
  case ARRAY_TOKEN:
  case RECORD_TOKEN:
  case SET_TOKEN:
  case FILE_TOKEN:
    return structured_type();
  default:
    error("illegal type near %s", token);
  }
  return 0;
}

Type *subrange_type() {
  Constant *start, *end;

  start = constant();
  need(ELLIPSIS_TOKEN);
  end = constant();
  return new_subrange_type(start, end);
}

Type *enumerated_type() {
  Identifier_List *ids;
  
  next_token();
  ids = identifier_list();
  need(RPAREN_TOKEN);
  return new_enumerated_type(ids);
}

Type *new_enumerated_type(Identifier_List *ids) {
  Type *type;
  int value;
  int size;

  size = count_ids(ids);
  type = new_ordinal_type(0, 0, size-1);
  value = 0;
  while (ids) {
    insert_symbol(new_constant_symbol(ids->id, new_ordinal_constant(type, value)));
    value++;
    ids = ids->next;
  }
  return type;
}

long count_ids(Identifier_List *ids) {
  long count = 0;
  while (ids) {
    count++;
    ids = ids->next;
  }
  return count;
}

Type *pointer_type() {
  Symbol *sym;
  char *id;
  
  next_token();
  id = identifier();
  sym = lookup_symbol_local(id);
  if (sym && sym->class == TYPE_SYMBOL)
    return new_pointer_type(sym->type);
  return new_undefined_pointer_type(id);
}

Type *structured_type() {
  switch (token_type) {
  case ARRAY_TOKEN:
    return array_type();
  case RECORD_TOKEN:
    return record_type();
  case SET_TOKEN:
    return set_type();
  case FILE_TOKEN:
    return file_type();
  default:
    internal_error();
  }
  return 0;
}

Type *packed_structured_type() {
  Type *type;
  
  next_token();
  type = structured_type();
  switch (type->class) {
  case ARRAY_TYPE:
    type->array.packed = true;
    break;
  case RECORD_TYPE:
    type->record.packed = true;
    break;
  case SET_TYPE:
    type->set.packed = true;
    break;
  case FILE_TYPE:
    type->file.packed = true;
    break;
  default:
    internal_error();
  }
  return type;
}

Type *array_type() {
  next_token();
  need(LBRACK_TOKEN);
  return array_index();
}

Type *array_index() {
  Type *type = new_type(ARRAY_TYPE);
  type->array.packed = false;
  type->array.index_type = index_type();
  if (match(COMMA_TOKEN))
    type->array.component_type = array_index();
  else
    type->array.component_type = array_component();
  type->size = (type->array.index_type->ordinal.high
		- type->array.index_type->ordinal.low + 1)
    * type->array.component_type->size;
  return type;
}

Type *index_type() {
  Type *type = type_denoter();
  if (type->class != ORDINAL_TYPE)
    error("illegal type for array index");
  return type;
}

Type *array_component() {
  need(RBRACK_TOKEN);
  need(OF_TOKEN);
  return type_denoter();
}

Type *record_type() {
  Type *record;
  
  next_token();
  record = record_body();
  set_parents(record, record);
  need(END_TOKEN);
  return record;
}

void set_parents(Type *record, Type *parent) {
  Variant_List *vptr;
  
  set_field_parents(record->record.fields, parent);
  if (record->record.variant) {
    record->record.variant->selector->variable.parent = parent;
    for (vptr = record->record.variant->variants; vptr; vptr = vptr->next)
      set_parents(vptr->fields, parent);
  }
}

void set_field_parents(Symbol_List *fields, Type *parent) {
  while (fields) {
    fields->symbol->variable.parent = parent;
    fields = fields->next;
  }
}

Type *record_body() {
  Symbol_List *fields;
  Variant_Part *variant;
  
  fields = field_list();
  if (match(CASE_TOKEN))
    variant = variant_part();
  else
    variant = 0;
  return new_record_type(fields, variant);
}

Symbol_List *field_list() {
  Symbol_List *syms;
  char *id;
  
  if (!check(IDENTIFIER_TOKEN))
    return 0;
  syms = new(Symbol_List);
  id = identifier();
  syms->symbol = new_field_symbol(id);
  switch (token_type) {
  case COMMA_TOKEN:
    next_token();
    syms->next = field_list();
    syms->symbol->variable.type = syms->next->symbol->variable.type;
    break;
  case COLON_TOKEN:
    next_token();
    syms->symbol->variable.type = type_denoter();
    if (match(SEMICOLON_TOKEN))
      syms->next = field_list();
    else
      syms->next = 0;
    break;
  default:
    syntax_error(100);
    syms->next = 0;
  }
  return syms;
}

Variant_Part *variant_part() {
  Symbol *selector;
  Variant_List *variants;
  
  selector = variant_selector();
  need(OF_TOKEN);
  variants = variant_list();
  return new_variant_part(selector, variants);
}

Symbol *variant_selector() {
  char *id1, *id2;

  id1 = identifier();
  if (match(COLON_TOKEN))
    id2 = identifier();
  else {
    id2 = id1;
    id1 = 0;
  }
  return new_variant_selector(id1, id2);
}

Symbol *new_variant_selector(char *id1, char *id2) {
  Symbol *sym = new_field_symbol(id1);
  Symbol *type_symbol = lookup_symbol(id2);
  if (type_symbol == 0)
    error("%s in not defined", id2);
  else if (type_symbol->class != TYPE_SYMBOL)
    error("%s is not a type", id2);
  else
    sym->variable.type = type_symbol->type;
  return sym;
}

Variant_List *variant_list() {
  Variant_List *variants;
  
  if (check(RPAREN_TOKEN) || check(END_TOKEN))
    return 0;
  variants = new(Variant_List);
  variants->cnsts = constant_list();
  need(COLON_TOKEN);
  need(LPAREN_TOKEN);
  variants->fields = record_body();
  need(RPAREN_TOKEN);
  if (match(SEMICOLON_TOKEN))
    variants->next = variant_list();
  else
    variants->next = 0;
  return variants;
}

Variant_Part *new_variant_part(Symbol *selector, Variant_List *variants) {
  Variant_Part *variant = new(Variant_Part);
  variant->selector = selector;
  variant->variants = variants;
  return variant;
}

Type *set_type() {
  Type *type;
  
  next_token();
  need(OF_TOKEN);
  type = type_denoter();
  return new_set_type(false, type);
}

Type *file_type() {
  Type *type;
  next_token();
  need(OF_TOKEN);
  type = type_denoter();
  return new_file_type(false, type);
}

Type *new_type(Type_Class class) {
  Type *type = new(Type);
  type->class = class;
  type->size = 0;
  return type;
}

/*
  ordinal types can be signed or unsigned
  if low < 0 then the type is signed
  otherwise it's unsigned.

  the size is 1, 2, 4 or 8 bytes.

  this function assumes a 64-bit system
*/

Type *new_ordinal_type(Type *base, Ordinal low, Ordinal high) {
  Type *type = new_type(ORDINAL_TYPE);
  if (base == 0)
    type->ordinal.base = type;
  else
    type->ordinal.base = base;
  type->ordinal.low = low;
  type->ordinal.high = high;

  if (low < 0) {
    if (-128 <= low && high < 128)
      type->size = 1;
    else if (-32768 <= low && high < 32768)
      type->size = 2;
    else if (-2147483648 <= low && high < 2147483648)
      type->size = 4;
    else
      type->size = 8;
  }
  else {
    if (high < 256)
      type->size = 1;
    else if (high < 65536)
      type->size = 2;
    else if (high < 4294967296)
      type->size = 4;
    else
      type->size = 8;
  }
  return type;
}

Type *new_real_type() {
  Type *type = new_type(REAL_TYPE);
  type->size = REAL_SIZE;
  return type;
}

Type *new_file_type(bool packed, Type *base) {
  Type *type = new_type(FILE_TYPE);
  type->size = POINTER_SIZE;
  type->file.packed = packed;
  type->file.base = base;
  return type;
}

Type *new_set_type(bool packed, Type *base) {
  Type *type = new_type(SET_TYPE);
  type->size = SET_SIZE;
  type->set.packed = packed;
  type->set.base = base;
  return type;
}

Type *new_pointer_type(Type *base) {
  Type *type = new_type(POINTER_TYPE);
  type->size = POINTER_SIZE;
  type->pointer = base;
  return type;
}

Type *new_undefined_pointer_type(char *id) {
  Type *type = new_type(POINTER_TYPE);
  type->size = 0;
  type->pointer = 0;
  add_undefined_pointer(id, type);
  return type;
}

void add_undefined_pointer(char *id, Type *type) {
  Undefined_Pointer_List *ups = new(Undefined_Pointer_List);
  ups->id = id;
  ups->type = type;
  ups->next = undefined_pointer_list;
  undefined_pointer_list = ups;
}

void define_undefined_pointer(char *id, Type *type) {
  undefined_pointer_list = search_undefined_pointers(undefined_pointer_list,
						     id, type);
}

Undefined_Pointer_List *search_undefined_pointers(Undefined_Pointer_List *ups,
						  char *id, Type *type) {
  if (ups == 0)
    return 0;
  if (strcmp(ups->id, id) == 0) {
    ups->type->size = POINTER_SIZE;
    ups->type->pointer = type;
    ups = search_undefined_pointers(ups->next, id, type);
  }
  else
    ups->next = search_undefined_pointers(ups->next, id, type);
  return ups;
}

Type *new_record_type(Symbol_List *fields, Variant_Part *variant) {
  Type *type = new_type(RECORD_TYPE);

  type->record.packed = false;
  type->record.fields = fields;
  type->record.variant = variant;
  type->size = calculate_record_offsets(type, 0);
  return type;
}

long calculate_record_offsets(Type *type, long offset) {
  offset = calculate_field_offsets(type->record.fields, offset);
  if (type->record.variant)
    offset = calculate_variant_offsets(type->record.variant, offset);
  return offset;
}

long calculate_field_offsets(Symbol_List *fields, long offset) {
  while (fields) {
    fields->symbol->variable.offset = offset;
    offset = align_up(offset, fields->symbol->variable.type->size);
    fields = fields->next;
  }
  return offset;
}

long calculate_variant_offsets(Variant_Part *variant, long offset) {
  Variant_List *variants;
  long maximum_offset = 0;
  long trial_offset;
  
  if (variant->selector->name) {
    /* allocate the variant selector only if it is named */
    variant->selector->variable.offset = offset;
    offset = align_up(offset, variant->selector->variable.type->size);
  }
  maximum_offset = 0;
  for (variants = variant->variants; variants; variants = variants->next) {
    trial_offset = calculate_record_offsets(variants->fields, offset);
    if (trial_offset > maximum_offset)
      maximum_offset = trial_offset;
  }
  return maximum_offset;
}

Type *type_identifier() {
  Symbol *sym;
  
  char *id = identifier();
  sym = lookup_symbol(id);
  if (sym->class == TYPE_SYMBOL)
    return sym->type;
  error("type identifier expected at %d", id);
  return 0;
}
