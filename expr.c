#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal.h"

void simple_expression(void);
void term(void);
void factor(void);
void set_constructor(void);
void member_designator_list(void);
void member_designator(void);
void function_designator(Symbol *);
void standard_function(Standard_Function);
Constant *unsigned_constant(void);
Constant *integer_constant(void);
Constant *real_constant(void);
Constant *char_constant(void);
Constant *string_constant(void);
char *fix_string(char *);
Constant *identified_constant(void);
void abs_function(void);
void sqr_function(void);
void sin_function(void);
void cos_function(void);
void exp_function(void);
void ln_function(void);
void sqrt_function(void);
void arctan_function(void);
void trunc_function(void);
void round_function(void);
void ord_function(void);
void chr_function(void);
void succ_function(void);
void pred_function(void);
void odd_function(void);
void eoln_function(void);
void eof_function(void);


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
  case NIL_TOKEN:
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
	function_designator(sym);
	break;
      case STDFUNC_SYMBOL:
	standard_function(sym->stdfunc);
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
  next_token();
  if (match(RBRACK_TOKEN))
    return;
  member_designator_list();
  need(RBRACK_TOKEN);
}

void member_designator_list() {
  do
    member_designator();
  while (match(COMMA_TOKEN));
}

void member_designator() {
  expression();
  if (match(ELLIPSIS_TOKEN))
    expression();
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

void function_designator(Symbol *sym) {
  next_token();
  if (sym->func.params) {
    need(LPAREN_TOKEN);
    actual_parameter_list(sym->func.params);
    need(RPAREN_TOKEN);
  }
}

void standard_function(Standard_Function func) {
  switch(func) {
  case ABS_FUNCTION:
    abs_function();
    break;
  case SQR_FUNCTION:
    sqr_function();
    break;
  case SIN_FUNCTION:
    sin_function();
    break;
  case COS_FUNCTION:
    cos_function();
    break;
  case EXP_FUNCTION:
    exp_function();
    break;
  case LN_FUNCTION:
    ln_function();
    break;
  case SQRT_FUNCTION:
    sqrt_function();
    break;
  case ARCTAN_FUNCTION:
    arctan_function();
    break;
  case TRUNC_FUNCTION:
    trunc_function();
    break;
  case ROUND_FUNCTION:
    round_function();
    break;
  case ORD_FUNCTION:
    ord_function();
    break;
  case CHR_FUNCTION:
    chr_function();
    break;
  case SUCC_FUNCTION:
    succ_function();
    break;
  case PRED_FUNCTION:
    pred_function();
    break;
  case ODD_FUNCTION:
    odd_function();
    break;
  case EOLN_FUNCTION:
    eoln_function();
    break;
  case EOF_FUNCTION:
    eof_function();
    break;
  }
}

void abs_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void sqr_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void sin_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void cos_function(void) {
  XXX();
}

void exp_function() {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void ln_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void sqrt_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void arctan_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void trunc_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void round_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void ord_function() {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void chr_function() {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void succ_function() {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void pred_function() {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void odd_function(void) {
  next_token();
  need(LPAREN_TOKEN);
  expression();
  need(RPAREN_TOKEN);
}

void eoln_function(void) {
  XXX();
}

void eof_function(void) {
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
  case NIL_TOKEN:
    next_token();
    return nil_constant;
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
