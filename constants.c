#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

Constant *constant_identifier(void);
Constant *integer_constant(void);
Constant *real_constant(void);
Constant *string_constant(void);

Constant_List *constant_list() {
  Constant_List *cnsts = new(Constant_List);
  cnsts->cnst = constant();
  if (match(COMMA_TOKEN))
    cnsts->next = constant_list();
  else
    cnsts->next = 0;
  return cnsts;
}

Constant *constant() {
  Constant *cnst;
  
  if (match(PLUS_TOKEN)) {
    cnst = unsigned_constant();
    if (!is_integer(cnst->type) && !is_real(cnst->type))
      error("illegal constant for plus operator");
  }
  else if (match(MINUS_TOKEN)) {
    cnst = unsigned_constant();
    if (is_integer(cnst->type))
      cnst->ordinal = -cnst->ordinal;
    else if (is_real(cnst->type))
      cnst->real = -cnst->real;
    else
      error("illegal constant for minus operator");
  }
  else
    cnst = unsigned_constant();
  return cnst;
}

Constant *unsigned_constant() {
  Constant *cnst;
  
  switch (token_type) {
  case IDENTIFIER_TOKEN:
    return constant_identifier();
  case INTEGER_TOKEN:
    return integer_constant();
  case REAL_TOKEN:
    return real_constant();
  case STRING_TOKEN:
    if (strlen(token) == 3 || strcmp("''''", token) == 0) {
      cnst = new_ordinal_constant(char_type, token[1]);
      next_token();
    }
    else
      cnst = string_constant();
    return cnst;
  case NIL_TOKEN:
    next_token();
    return nil_constant;
  default:
    error("illegal constant near %s", token);
  }
  return 0;
}

Constant *integer_constant() {
  Constant *cnst;
  
  cnst = new_ordinal_constant(integer_type, atol(token));
  next_token();
  return cnst;
}

Constant *real_constant() {
  Constant *cnst;

  cnst = new_real_constant(atof(token));
  next_token();
  return cnst;
}

Constant *string_constant() {
  Constant *cnst;

  cnst = new_string_constant(token);
  next_token();
  return cnst;
}

Constant *constant_identifier() {
  Symbol *sym;
  char *id = identifier();
  
  sym = lookup_symbol(id);
  if (sym == 0)
    error("%s undefined", id);
  else if (sym->class != CONSTANT_SYMBOL)
    error("%s is not a constant", id);
  else
    return sym->constant;
  return 0;
}

Constant *new_ordinal_constant(Type *type, Ordinal x) {
  Constant *cnst = new(Constant);
  cnst->type = type;
  cnst->ordinal = x;
  return cnst;
}

Constant *new_real_constant(double x) {
  Constant *cnst = new(Constant);
  cnst->type = real_type;
  cnst->real = x;
  return cnst;
}

Constant *new_string_constant(char *s) {
  Constant *cnst = new(Constant);
  cnst->string = fix_string(s);
  cnst->type = new_type(ARRAY_TYPE);
  cnst->type->array.packed = true;
  cnst->type->array.component_type = char_type;
  cnst->type->array.index_type = new_ordinal_type(integer_type, 1, strlen(cnst->string));
  cnst->type->size = strlen(cnst->string);
  return cnst;
}

/* remove quotes from string */

char *fix_string(char *s) {
  char *t, *u;

  t = u = malloc(strlen(s)-1);
  while (*s++) {
    if (*s == '\'')
      s++;
    *u++ = *s;
  }
  return t;
}



