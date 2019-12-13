#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

void simple_expression(void);
void term(void);
void factor(void);
void set_constructor(void);
void function_designator(void);
void standard_function(void);
Constant *unsigned_constant(void);
Constant *integer_constant(void);
Constant *real_constant(void);
Constant *char_constant(void);
Constant *string_constant(void);
char *fix_string(char *);
Constant *identified_constant(void);

void expression() {
  simple_expression();
  if (match(EQ_TOKEN))
    simple_expression();
  else if (match(NE_TOKEN))
    simple_expression();
  else if (match(LT_TOKEN))
    simple_expression();
  else if (match(LE_TOKEN))
    simple_expression();
  else if (match(GT_TOKEN))
    simple_expression();
  else if (match(GE_TOKEN))
    simple_expression();
  else if (match(IN_TOKEN))
    simple_expression();
}

void simple_expression() {
  if (match(PLUS_TOKEN))
    ;
  else if (match(MINUS_TOKEN))
    ;
  term();
  for (;;) {
    if (match(PLUS_TOKEN))
      term();
    else if (match(MINUS_TOKEN))
      term();
    else if (match(OR_TOKEN))
      term();
    else
      break;
  }
}

void term() {
  factor();
  for (;;) {
    if (match(MULTIPLY_TOKEN))
      factor();
    else if (match(DIVIDE_TOKEN))
      factor();
    else if (match(DIV_TOKEN))
      factor();
    else if (match(MOD_TOKEN))
      factor();
    else if (match(AND_TOKEN))
      factor();
    else
      break;
  }
}

void factor() {
  Symbol *sym;
  
  switch (token_type) {
  case NOT_TOKEN:
    next_token();
    factor();
    break;
  case LPAREN_TOKEN:
    next_token();
    expression();
    need(RPAREN_TOKEN);
    break;
  case LBRACK_TOKEN:
    set_constructor();
    break;
  case INTEGER_TOKEN:
  case REAL_TOKEN:
  case CHAR_TOKEN:
  case STRING_TOKEN:
    unsigned_constant();
    break;
  case IDENTIFIER_TOKEN:
    sym = lookup(token);
    if (sym) {
      switch (sym->class) {
      case VARIABLE_SYMBOL:
      case VARARG_SYMBOL:
      case VALARG_SYMBOL:
      case FIELD_SYMBOL:
	variable_access();
	break;
      case CONSTANT_SYMBOL:
	unsigned_constant();
	break;
      case FUNCTION_SYMBOL:
      case FUNCARG_SYMBOL:
	function_designator();
	break;
      case STDFUNC_SYMBOL:
	standard_function();
	break;
      default:
	error("illegal expression near %s", token);
      }
    }
    else
      error("%s is not defined", token);
    break;
  default:
    error("illegal expression near %s", token);
  }
}

void set_constructor() {
  XXX();
}

void variable_access() {
  identifier();
  for (;;) {
    if (match(LBRACK_TOKEN)) {
      do
	expression();
      while (match(COMMA_TOKEN));
      need(RBRACK_TOKEN);
    }
    else if (match(PERIOD_TOKEN))
      identifier();
    else if (match(ARROW_TOKEN))
      ;
    else
      break;
  }
}

void function_designator() {
  XXX();
}

void standard_function() {
  XXX();
}

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
  case CHAR_TOKEN:
    return char_constant();
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

Constant *char_constant() {
  Constant *cnst = new(Constant);
  cnst->type = char_type;
  cnst->ordinal = token[1];
  next_token();
  return cnst;
}

Constant *string_constant() {
  Constant *cnst = new(Constant);

  cnst->string = fix_string(token);
  cnst->type = make_string_type(cnst->string);
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
