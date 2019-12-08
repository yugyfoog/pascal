#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

Constant *unsigned_constant(void);
Constant *integer_constant(void);
Constant *real_constant(void);
Constant *string_constant(void);
char *fix_string(char *);
Constant *identified_constant(void);

Constant_List *constant_list(void) {
  Constant_List *cnsts = new(Constant_List);
  cnsts->constant = constant();
  if (match(COMMA_TOKEN))
    cnsts->next = constant_list();
  else
    cnsts->next = 0;
  return cnsts;
}

Constant *constant() {
  Constant *cnst;
  int sign = 0;

  if (match(PLUS_TOKEN))
    sign = 1;
  else if (match(MINUS_TOKEN))
    sign = -1;
  cnst = unsigned_constant();
  if (sign) {
    if (cnst->type->class == ORDINAL_TYPE) {
      if (sign == -1)
	cnst = new_ordinal_constant(cnst->type, -cnst->ordinal);
    }
    else if (cnst->type->class == REAL_TYPE) {
      if (sign == -1)
	cnst = new_real_constant(-cnst->real);
    }
    else
      error("%s operator on non-numeric type", sign == 1 ? "+" : "-");
  }
  return cnst;
}

Constant *unsigned_constant() {
  switch (token_type) {
  case INTEGER_TOKEN:
    return integer_constant();
  case REAL_TOKEN:
    return real_constant();
  case STRING_TOKEN:
    return string_constant();
  case IDENTIFIER_TOKEN:
    return identified_constant();
  default:
    error("constant expected near %s", token);
  }
  return 0;
}

Constant *integer_constant() {
  Constant *cnst = new(Constant);

  cnst->type = integer_type;
  cnst->ordinal = atol(token);
  next_token();
  return cnst;
}

Constant *real_constant() {
  Constant *cnst = new(Constant);
  
  cnst->type = real_type;
  cnst->real = atof(token);
  next_token();
  return cnst;
}

Constant *string_constant() {
  Constant *cnst = new(Constant);

  if (strlen(token) == 3 || strcmp(token, "''''") == 0) {
    cnst->type = char_type;
    cnst->ordinal = token[1];
  }
  else {
    cnst->string = fix_string(token);
    cnst->type = make_string_type(cnst->string);
  }
  next_token();
  return cnst;
}

char *fix_string(char *s2) {
  char *s1;
  char *buf = malloc(strlen(s2)+1);

  s1 = buf;
  if (*s2 == '\'')
    s2++;
  for (;;) {
    if (*s2 == '\'')
      s2++;
    if (*s2 == '\0')
      break;
    *s1++ = *s2++;
  }
  *s1 = '\0';
  return buf;
}

Constant *identified_constant() {
  Symbol *sym;
  char *id;

  id = identifier();
  sym = lookup(id);
  if (sym) {
    if (sym->class == CONSTANT_SYMBOL)
      return sym->cnst;
  }
  error("%s is not a constant", id);
  return 0;
}
