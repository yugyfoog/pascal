#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

Expression *simple_expression(void);
Expression *term(void);
Expression *factor(void);
Expression *indexed_expression(Expression *);
Expression *constant_expression(void);
Expression *function_designator(Symbol *);
Expression_List *value_parameter(Symbol_List *);
Expression_List *variable_parameter(Symbol_List *);
Expression_List *procedural_parameter(Symbol_List *);
Expression_List *functional_parameter(Symbol_List *);
Expression *standard_function(Symbol *);
Expression *set_constructor(void);
Expression_List *set_constructor_list(void);
Expression *member_designator(void);
Expression *abs_function(void);
Expression *sqr_function(void);
Expression *sin_function(void);
Expression *cos_function(void);
Expression *exp_function(void);
Expression *ln_function(void);
Expression *sqrt_function(void);
Expression *arctan_function(void);
Expression *trunc_function(void);
Expression *round_function(void);
Expression *ord_function(void);
Expression *chr_function(void);
Expression *succ_function(void);
Expression *pred_function(void);
Expression *odd_function(void);
Expression *eof_function(void);
Expression *eoln_function(void);
Expression *argc_function(void);

Expression *expression() {
  Expression *e;

  e = simple_expression();
  if (match(EQ_TOKEN))
    e = new_eq_expression(e, simple_expression());
  else if (match(NE_TOKEN))
    e = new_ne_expression(e, simple_expression());
  else if (match(LT_TOKEN))
    e = new_lt_expression(e, simple_expression());
  else if (match(LE_TOKEN))
    e = new_le_expression(e, simple_expression());
  else if (match(GT_TOKEN))
    e = new_gt_expression(e, simple_expression());
  else if (match(GE_TOKEN))
    e = new_ge_expression(e, simple_expression());
  else if (match(IN_TOKEN))
    e = new_in_expression(e, simple_expression());
  return e;
}

Expression *simple_expression() {
  Expression *e;

  if (match(PLUS_TOKEN))
    e = new_plus_expression(term());
  else if (match(MINUS_TOKEN))
    e = new_minus_expression(term());
  else
    e = term();
  for (;;) {
    if (match(PLUS_TOKEN))
      e = new_add_expression(e, term());
    else if (match(MINUS_TOKEN))
      e = new_subtract_expression(e, term());
    else if (match(OR_TOKEN))
      e = new_or_expression(e, term());
    else
      break;
  }
  return e;
}

Expression *term() {
  Expression *e;

  e = factor();
  for (;;) {
    if (match(MULTIPLY_TOKEN))
      e = new_multiply_expression(e, factor());
    else if (match(DIVIDE_TOKEN))
      e = new_divide_expression(e, factor());
    else if (match(DIV_TOKEN))
      e = new_div_expression(e, factor());
    else if (match(MOD_TOKEN))
      e = new_mod_expression(e, factor());
    else if (match(AND_TOKEN))
      e = new_and_expression(e, factor());
    else
      break;
  }
  return e;
}

Expression *factor() {
  Symbol *sym;
  Expression *e;
  
  switch (token_type) {
  case IDENTIFIER_TOKEN:
    sym = lookup_symbol(token);
    if (sym == 0)
      error("%s undefined", token);
    else {
      switch (sym->class) {
      case CONSTANT_SYMBOL:
	return constant_expression(); /* unsigned-constant in standard */
      case FUNCTION_SYMBOL:
      case FUNCTION_PARAMETER:
	return function_designator(sym);
      case VARIABLE_SYMBOL:
      case VALUE_PARAMETER:
      case VARIABLE_PARAMETER:
      case FIELD_SYMBOL:
	return variable_access();
      case STANDARD_FUNCTION:
	return standard_function(sym);
      default:
	syntax_error(1);
      }
    }
    break;
  case INTEGER_TOKEN:
  case REAL_TOKEN:
  case STRING_TOKEN:
  case NIL_TOKEN:
    return constant_expression();
  case LBRACK_TOKEN:
    return set_constructor();
  case LPAREN_TOKEN:
    next_token();
    e = expression();
    need(RPAREN_TOKEN);
    return e;
  case NOT_TOKEN:
    next_token();
    return new_not_expression(factor());
  default:
    syntax_error(2);
  }
  return 0;
}

Expression *constant_expression() {
  return new_constant_expression(unsigned_constant());
}

Expression *standard_function(Symbol *func) {
  next_token();
  switch (func->stdfunc) {
  case ABS_FUNCTION:
    return abs_function();
  case SQR_FUNCTION:
    return sqr_function();
  case SIN_FUNCTION:
    return sin_function();
  case COS_FUNCTION:
    return cos_function();
  case EXP_FUNCTION:
    return exp_function();
  case LN_FUNCTION:
    return ln_function();
  case SQRT_FUNCTION:
    return sqrt_function();
  case ARCTAN_FUNCTION:
    return arctan_function();
  case TRUNC_FUNCTION:
    return trunc_function();
  case ROUND_FUNCTION:
    return round_function();
  case ORD_FUNCTION:
    return ord_function();
  case CHR_FUNCTION:
    return chr_function();
  case SUCC_FUNCTION:
    return succ_function();
  case PRED_FUNCTION:
    return pred_function();
  case ODD_FUNCTION:
    return odd_function();
  case EOF_FUNCTION:
    return eof_function();
  case EOLN_FUNCTION:
    return eoln_function();
  case ARGC_FUNCTION:
    return argc_function();
  }
  return 0;
}

Expression *set_constructor() {
  Expression *e;
  next_token();
  if (match(RBRACK_TOKEN))
    return new_empty_set();
  e = new_set_constructor(set_constructor_list());
  need(RBRACK_TOKEN);
  return e;
}

Expression_List *set_constructor_list() {
  Expression_List *exprs = new(Expression_List);
  exprs->expr = member_designator();
  if (match(COMMA_TOKEN))
    exprs->next = set_constructor_list();
  else
    exprs->next = 0;
  return exprs;
}

Expression *member_designator() {
  Expression *e;

  e = expression();
  if (match(ELLIPSIS_TOKEN))
    e = new_set_range_expression(e, expression());
  else
    e = new_set_expression(e);
  return e;
}

Expression *variable_access() {
  Expression *e;
  Symbol *sym;
  char *id = identifier();

  sym = lookup_symbol(id);
  if (sym == 0)
    error("%d is not defined", id);
  if (sym->class == FIELD_SYMBOL)
    e = new_field_designator_expression(new_variable_expression(find_with_variable(sym->variable.parent)),
					sym);
  else
    e = new_variable_expression(sym);
  for (;;) {
    if (match(LBRACK_TOKEN))
      e = indexed_expression(e);
    else if (match(PERIOD_TOKEN))
      e = new_field_designator_expression(e, find_field(e->type, identifier()));
    else if (match(ARROW_TOKEN))
      e = new_indirect_expression(e);
    else
      break;
  }
  return e;
}

Expression *indexed_expression(Expression *e) {
  do {
    e = new_index_expression(e, expression());
  } while (match(COMMA_TOKEN));
  need(RBRACK_TOKEN);
  return e;
}

Expression *function_designator(Symbol *sym) {
  Expression_List *exprs = 0;
  
  next_token();
  if (match(LPAREN_TOKEN)) {
    if (sym->algorithm.parameters == 0)
      error("function parameters don't match function definition");
    else
      exprs = actual_parameter_list(sym->algorithm.parameters);
    need(RPAREN_TOKEN);
  }
  else if (sym->algorithm.parameters != 0)
    error("function parameters don't match function definition");
  return new_function_call(sym, exprs);
}

Expression_List *actual_parameter_list(Symbol_List *params) {
  switch (params->symbol->class) {
  case VALUE_PARAMETER:
    return value_parameter(params);
  case VARIABLE_PARAMETER:
    return variable_parameter(params);
  case PROCEDURE_PARAMETER:
    return procedural_parameter(params);
  case FUNCTION_PARAMETER:
    return functional_parameter(params);
  default:
    error("syntax error in parameter list");
  }
  return 0;
}

Expression_List *value_parameter(Symbol_List *params) {
  Expression_List *exprs = new(Expression_List);
  exprs->expr = expression();
  if (match(COMMA_TOKEN))
    exprs->next = actual_parameter_list(params->next);
  else
    exprs->next = 0;
  return exprs;
}

Expression_List *variable_parameter(Symbol_List *params) {
  Expression_List *exprs = new(Expression_List);
  exprs->expr = variable_access();
  if (match(COMMA_TOKEN))
    exprs->next = actual_parameter_list(params->next);
  else
    exprs->next = 0;
  return exprs;
}

Expression_List *procedural_parameter(Symbol_List *params) {
  Expression_List *exprs = new(Expression_List);

  exprs->expr = new_procedural_parameter_expression(lookup_symbol(identifier()), params->symbol);
  if (match(COMMA_TOKEN))
    exprs->next = actual_parameter_list(params->next);
  else
    exprs->next = 0;
  return exprs;
}

Expression_List *functional_parameter(Symbol_List *params) {
  Expression_List *exprs = new(Expression_List);
  exprs->expr = new_functional_parameter_expression(lookup_symbol(identifier()), params->symbol);
  if (match(COMMA_TOKEN))
    exprs->next = actual_parameter_list(params->next);
  else
    exprs->next = 0;
  return exprs;
}

Expression *abs_function() {
  Expression *e;
  
  need(LPAREN_TOKEN);
  e = new_abs_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *sqr_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_sqr_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *sin_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_sin_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *cos_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_cos_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *exp_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_exp_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *ln_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_ln_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *sqrt_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_sqrt_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *arctan_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_arctan_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *trunc_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_trunc_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *round_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_round_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *ord_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_ord_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *chr_function() {
  Expression *e;
  need(LPAREN_TOKEN);
  e = new_chr_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *succ_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_succ_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *pred_function() {
  Expression *e;
  
  need(LPAREN_TOKEN);
  e = new_pred_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *odd_function() {
  Expression *e;

  need(LPAREN_TOKEN);
  e = new_odd_function(expression());
  need(RPAREN_TOKEN);
  return e;
}

Expression *eof_function() {
  Expression *e;
  
  if (match(LPAREN_TOKEN)) {
    e = expression();
    need(RPAREN_TOKEN);
  }
  else
    e = new_variable_expression(lookup_symbol("input"));
  return new_eof_function(e);
}

Expression *eoln_function() {
  Expression *e;

  if (match(LPAREN_TOKEN)) {
    e = expression();
    need(RPAREN_TOKEN);
  }
  else
    e = new_variable_expression(lookup_symbol("input"));
  return new_eoln_function(e);
}

Expression *argc_function() {
  return new_argc_function();
}

  
