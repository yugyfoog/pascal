#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "machine.h"
#include "pascal.h"

#define FILE_VARIABLE "_file"

int for_level = -1;
  
int block_level;
long variable_offset;

Symbol_List *with_symbol_stack = 0;

Symbol *find_field(Type *, char *);
Symbol *search_field_list(Symbol_List *, char *);
Symbol *search_variant(Variant_Part *, char *);
Symbol *search_variant_list(Variant_List *, char *);
Expression *new_integer_to_real_expression(Expression *);
Expression *new_unary_expression(Expression_Class, Type *,
				 Expression *);
Expression *new_binary_expression(Expression_Class, Type *,
				  Expression *, Expression *);

bool compatible(Type *, Type *);
bool assignment_compatible(Type *, Type *);

bool is_integer(Type *t) {
  if (t->class == ORDINAL_TYPE)
    return t->ordinal.base == integer_type;
  return false;
}

bool is_char(Type *t) {
  if (t->class == ORDINAL_TYPE)
    return t->ordinal.base == char_type;
  return false;
}

bool is_boolean(Type *t) {
  if (t->class == ORDINAL_TYPE)
    return t->ordinal.base == boolean_type;
  return false;
}

bool is_real(Type *t) {
  return t == real_type;
}

bool is_file(Type *t) {
  return t->class == FILE_TYPE;
}

bool is_string(Type *t) {
  if (t->class != ARRAY_TYPE)
    return false;
  if (t->array.packed == false)
    return false;
  if (t->array.component_type != char_type)
    return false;
  if (!is_integer(t->array.index_type))
    return false;
  if (t->array.index_type->ordinal.low != 1)
    return false;
  return true;
}

bool is_set(Type *t) {
  return t->class == SET_TYPE;
}

Symbol *new_symbol(char *name, Symbol_Class class) {
  Symbol *sym = new(Symbol);
  sym->class = class;
  sym->name = name;
  sym->block_level = block_level;
  return sym;
}

Symbol *new_variable_symbol(char *name, Type *type) {
  Symbol *sym = new_symbol(name, VARIABLE_SYMBOL);
  sym->variable.type = type;
  variable_offset = align_down(variable_offset, type->size);
  sym->variable.offset = variable_offset;
  sym->variable.parent = 0;
  return sym;
}

Symbol *new_parameter_symbol(char *name, Symbol_Class class) {
  Symbol *sym = new_symbol(name, class);
  sym->variable.type = 0;
  sym->variable.offset = 0;
  return sym;
}

Symbol *new_label_symbol(char *name, Symbol *algo) {
  Symbol *sym = new_symbol(name, LABEL_SYMBOL);
  sym->label.algorithm = algo;
  return sym;
}

Type *new_subrange_type(Constant *start, Constant *end) {
  if (start->type->class != ORDINAL_TYPE
      || end->type->class != ORDINAL_TYPE)
    error("illegal types for subrange");
  else if (start->type != end->type)
    error("subrange bounds must be the same type");
  else if (start->ordinal > end->ordinal) /* yes, equal is allowed */
    error("start value greater than end value in subrange");
  else
    return new_ordinal_type(start->type, start->ordinal, end->ordinal);
  return 0;
}

Symbol *find_with_variable(Type *type) {
  Symbol_List *sptr;
  
  for (sptr = with_symbol_stack; sptr; sptr = sptr->next) {
    if (sptr->symbol->variable.type == type)
      return sptr->symbol;
  }
  return 0;
}

Statement *new_statement(Statement_Class class, Symbol *label) {
  Statement *stmt = new(Statement);
  stmt->class = class;
  stmt->label = label;
  return stmt;
}

Statement *new_assignment_statement(Expression *lval, Expression *rval,
				    Symbol *label) {
  Statement *stmt = new_statement(ASSIGNMENT_STATEMENT, label);
  stmt->assignment.lval = lval;
  stmt->assignment.rval = rval;
  return stmt;
}

Statement *new_procedure_call(Symbol *sym, Expression_List *params, Symbol *label) {
  Statement *stmt = new_statement(PROCEDURE_CALL, label);
  stmt->call.sym = sym;
  stmt->call.params = params;
  return stmt;
}

Statement *new_reset_statement(Expression *file, Expression *name, Symbol *label) {
  Statement *stmt = new_statement(RESET_STATEMENT, label);
  stmt->reset_rewrite.file = file;
  stmt->reset_rewrite.name = name;
  return stmt;
}

Statement *new_rewrite_statement(Expression *file, Expression *name, Symbol *label) {
  Statement *stmt = new_statement(REWRITE_STATEMENT, label);
  stmt->reset_rewrite.file = file;
  stmt->reset_rewrite.name = name;
  return stmt;
}

Statement *new_get_statement(Expression *file, Symbol *label) {
  Statement *stmt = new_statement(GET_STATEMENT, label);
  stmt->parameter = file;
  return stmt;
}

Statement *new_put_statement(Expression *file, Symbol *label) {
  Statement *stmt = new_statement(PUT_STATEMENT, label);
  stmt->parameter = file;
  return stmt;
}

Statement *new_read_statement(Symbol *file, Expression *e) {
  Statement *stmt = new_statement(READ_STATEMENT, 0);
  stmt->read.file = file;
  stmt->read.expression = e;
  return stmt;
}

Statement *new_read_text_statement(Symbol *file, Expression *e) {
  Statement *stmt = new_statement(READ_TEXT_STATEMENT, 0);
  stmt->read.file = file;
  stmt->read.expression = e;
  return stmt;
}

Statement *new_readln_statement(Symbol *file) {
  Statement *stmt = new_statement(READLN_STATEMENT, 0);
  stmt->readln = file;
  return stmt;
}

Statement *new_write_statement(Symbol *file, Expression *e) {
  Statement *stmt = new_statement(WRITE_STATEMENT, 0);
  stmt->write.file = file;
  stmt->write.expression = e;
  stmt->write.field_width = 0;
  stmt->write.fractional_digits = 0;
  return stmt;
}

Statement *new_write_text_statement(Symbol *file, Expression *e, Expression *f1, Expression *f2) {
  Statement *stmt = new_statement(WRITE_TEXT_STATEMENT, 0);
  stmt->write.file = file;
  stmt->write.expression = e;
  stmt->write.field_width = f1;
  stmt->write.fractional_digits = f2;
  return stmt;
}

/*
  a writeln_statement is an internal statement.
  It is only used as the final part of a writeln
  procedure that sends a newline to output file.
*/

Statement *new_writeln_statement(Symbol *file) {
  Statement *stmt = new_statement(WRITELN_STATEMENT, 0);
  stmt->writeln = file;
  return stmt;
}

/*
  create a new variable for storing the results
  of the file parameter to read/readln/write/writeln
  procedures.

  If one already exists for this function return that one.

*/

Symbol *create_tempory_file_variable() {
  Symbol *sym = lookup_symbol_local(FILE_VARIABLE);
  if (sym == 0) {
    /* this variable is not any specific
       file type. I only use text_type
       as a place holder */
    sym = new_variable_symbol(FILE_VARIABLE, text_type);
    insert_symbol(sym);
  }
  return sym;
}

void push_for_variable_stack() {
  for_level++;
  if (for_level >= 20)
    fatal_error("for loops nested too deep");
}

void pop_for_variable_stack() {
  for_level--;
}

Symbol *create_for_variable(int n) {
  char name[20];

  sprintf(name, "_for%d%c", for_level, 'a' + n - 1);
  Symbol *sym = lookup_symbol_local(name);
  if (sym == 0) {
    sym = new_variable_symbol(name, integer_type);
    insert_symbol(sym);
  }
  return sym;
}

Expression *new_expression(Expression_Class class, Type *type) {
  Expression *e = new(Expression);
  e->class = class;
  e->type = type;
  return e;
}

Expression *new_constant_expression(Constant *cnst) {
  Expression *e = new_expression(CONSTANT_EXPRESSION, cnst->type);
  e->constant = cnst;
  return e;
}

Expression *new_variable_expression(Symbol *var) {
  Expression *e = new_expression(VARIABLE_EXPRESSION, var->type);
  e->variable = var;
  return e;
}

Expression *new_eq_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_EQ_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_EQ_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_EQ_EXPRESSION, boolean_type, e, f);
    if (is_set(e->type))
      return new_binary_expression(SET_EQ_EXPRESSION, boolean_type, e, f);
    if (e->type->class == POINTER_TYPE)
      return new_binary_expression(POINTER_EQ_EXPRESSION, boolean_type, e, f);
    error("illegal type for = operator");
    return 0;
  }
  /* we still can have mixed integer/real arguments */
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_EQ_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_EQ_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for = operator");
  return 0;
}

Expression *new_ne_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_NE_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_NE_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_NE_EXPRESSION, boolean_type, e, f);
    if (is_set(e->type))
      return new_binary_expression(SET_NE_EXPRESSION, boolean_type, e, f);
    if (e->type->class == POINTER_TYPE)
      return new_binary_expression(POINTER_NE_EXPRESSION, boolean_type, e, f);
    error("illegal type for <> operator");
    return 0;
  }
  /* we still can have mixed integer/real arguments */
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_NE_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_NE_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for <> operator");
  return 0;
}

Expression *new_lt_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_LT_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_LT_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_LT_EXPRESSION, boolean_type, e, f);
    error("illegal type for < operator");
    return 0;
  }
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_LT_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_LT_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for < operator");
  return 0;
}

Expression *new_le_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_LE_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_LE_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_LE_EXPRESSION, boolean_type, e, f);
    if (is_set(e->type))
      return new_binary_expression(SET_LE_EXPRESSION, boolean_type, e, f);
    error("illegal type for <= operator");
    return 0;
  }
  /* we still can have mixed integer/real arguments */
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_GE_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_GE_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for <= operator");
  return 0;
}

Expression *new_gt_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_GT_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_GT_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_GT_EXPRESSION, boolean_type, e, f);
    error("illegal type for > operator");
    return 0;
  }
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_GT_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_GT_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for > operator");
  return 0;
}

Expression *new_ge_expression(Expression *e, Expression *f) {
  if (compatible(e->type, f->type)) {
    if (e->type->class == ORDINAL_TYPE)
      return new_binary_expression(ORDINAL_GE_EXPRESSION, boolean_type, e, f);
    if (is_real(e->type))
      return new_binary_expression(REAL_GE_EXPRESSION, boolean_type, e, f);
    if (is_string(e->type))
      return new_binary_expression(STRING_GE_EXPRESSION, boolean_type, e, f);
    if (is_set(e->type))
      return new_binary_expression(SET_GE_EXPRESSION, boolean_type, e, f);
    error("illegal type for >= operator");
    return 0;
  }
  /* we still can have mixed integer/real arguments */
  if (is_integer(e->type) && is_real(f->type))
    return new_binary_expression(REAL_GE_EXPRESSION, boolean_type, new_integer_to_real_expression(e), f);
  if (is_real(e->type) && is_integer(f->type))
    return new_binary_expression(REAL_GE_EXPRESSION, boolean_type, e, new_integer_to_real_expression(f));
  error("incompatible types for >= operator");
  return 0;
}

Expression *new_in_expression(Expression *e, Expression *f) {
  if (e->type->class != ORDINAL_TYPE)
    error("left type of in operator must be ordinal");
  if (f->type->class != SET_TYPE) {
    printf("class = %d\n", f->type->class);
    error("right type of in operator must be a set");
  }
  return new_binary_expression(IN_EXPRESSION, boolean_type, e, f);
}

Expression *new_integer_to_real_expression(Expression *e) {
  Expression *f = new_expression(INTEGER_TO_REAL_EXPRESSION, real_type);
  f->val = e;
  return f;
}

Expression *new_plus_expression(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(INTEGER_PLUS_EXPRESSION, integer_type, e);
  if (is_real(e->type))
    return new_unary_expression(REAL_PLUS_EXPRESSION, real_type, e);
  error("illegal type for plus operator");
  return 0;
}

Expression *new_minus_expression(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(INTEGER_MINUS_EXPRESSION, integer_type, e);
  if (is_real(e->type))
    return new_unary_expression(REAL_PLUS_EXPRESSION, real_type, e);
  error("illegal type for minus operator");
  return 0;
}

Expression *new_add_expression(Expression *e, Expression *f) {
  if (is_integer(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(INTEGER_ADD_EXPRESSION, integer_type, e, f);
    else if (is_real(f->type))
      return new_binary_expression(REAL_ADD_EXPRESSION, real_type,
				   new_integer_to_real_expression(e), f);
  }
  else if (is_real(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(REAL_ADD_EXPRESSION, real_type,
				   e, new_integer_to_real_expression(f));
    else if (is_real(f->type))
      return new_binary_expression(REAL_ADD_EXPRESSION, real_type, e, f);
  }
  else if (is_set(e->type) && is_set(f->type))
    return new_binary_expression(SET_UNION_EXPRESSION, e->type, e, f);

  error("illegal type for addition operator");
  return 0;
}

Expression *new_subtract_expression(Expression *e, Expression *f) {
  if (is_integer(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(INTEGER_SUBTRACT_EXPRESSION, integer_type, e, f);
    else if (is_real(f->type))
      return new_binary_expression(REAL_SUBTRACT_EXPRESSION, real_type,
				   new_integer_to_real_expression(e), f);
  }
  else if (is_real(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(REAL_SUBTRACT_EXPRESSION, real_type,
				   e, new_integer_to_real_expression(f));
    else if (is_real(f->type))
      return new_binary_expression(REAL_SUBTRACT_EXPRESSION, real_type, e, f);
  }
  else if (is_set(e->type) && is_set(f->type))
    return new_binary_expression(SET_DIFFERENCE_EXPRESSION, e->type, e, f);
  
  error("illegal type for subtraction operator");
  return 0;
}

Expression *new_multiply_expression(Expression *e, Expression *f) {
  if (is_integer(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(INTEGER_MULTIPLY_EXPRESSION, integer_type, e, f);
    else if (is_real(f->type))
      return new_binary_expression(REAL_MULTIPLY_EXPRESSION, real_type,
				   new_integer_to_real_expression(e), f);
  }
  else if (is_real(e->type)) {
    if (is_integer(f->type))
      return new_binary_expression(REAL_MULTIPLY_EXPRESSION, real_type,
				   e, new_integer_to_real_expression(f));
    else if (is_real(f->type))
      return new_binary_expression(REAL_MULTIPLY_EXPRESSION, real_type, e, f);
  }
  else if (is_set(e->type) && is_set(f->type))
    return new_binary_expression(SET_INTERSECTION_EXPRESSION, e->type, e, f);

  error("illegal type for multiplication operator");
  return 0;
}

Expression *new_divide_expression(Expression *e, Expression *f) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_integer(f->type))
    f = new_integer_to_real_expression(f);
  if (is_real(e->type) && is_real(f->type))
    return new_binary_expression(REAL_DIVIDE_EXPRESSION, real_type, e, f);
  error("illegal type for division");
  return 0;
}

Expression *new_div_expression(Expression *e, Expression *f) {
  if (is_integer(e->type) && is_integer(f->type))
    return new_binary_expression(INTEGER_DIVIDE_EXPRESSION, integer_type, e, f);
  error("illegal type for div operator");
  return 0;
}

Expression *new_mod_expression(Expression *e, Expression *f) {
  if (is_integer(e->type) && is_integer(f->type))
    return new_binary_expression(INTEGER_MODULUS_EXPRESSION, integer_type, e, f);
  error("illegal type for mod operator");
  return 0;
}

Expression *new_or_expression(Expression *e, Expression *f) {
  if (is_boolean(e->type) && is_boolean(f->type))
    return new_binary_expression(OR_EXPRESSION, boolean_type, e, f);
  error("illegal type for or operator");
  return 0;
}

Expression *new_and_expression(Expression *e, Expression *f) {
  if (is_boolean(e->type) && is_boolean(f->type))
    return new_binary_expression(AND_EXPRESSION, boolean_type, e, f);
  error("illegal type for and operator");
  return 0;
}

Expression *new_not_expression(Expression *e) {
  if (is_boolean(e->type))
    return new_unary_expression(NOT_EXPRESSION, boolean_type, e);
  error("illegal type for not operator");
  return 0;
}

Expression *new_index_expression(Expression *e, Expression *f) {
  if (assignment_compatible(e->type->array.index_type, f->type))
    return new_binary_expression(INDEX_EXPRESSION,
				 e->type->array.component_type,
				 e, f);
  error("array index type is not compatible with index");
  return 0;
}

Expression *new_field_designator_expression(Expression *base, Symbol *field) {
  Expression *e = new_expression(FIELD_EXPRESSION, field->variable.type);
  e->field.base = base;
  e->field.field = field;
  return e;
}

Symbol *find_field(Type *type, char *field) {
  Symbol *sym;

  if (type->class != RECORD_TYPE) {
    error("not a record type");
    return 0;
  }
  sym = search_field_list(type->record.fields, field);
  if (sym == 0)
    sym = search_variant(type->record.variant, field);
  return sym;
}

Symbol *search_field_list(Symbol_List *fields, char *field) {
  while (fields) {
    if (strcmp(fields->symbol->name, field) == 0)
      return fields->symbol;
    fields = fields->next;
  }
  return 0;
}

Symbol *search_variant(Variant_Part *variant_part, char *field) {
  if (strcmp(variant_part->selector->name, field) == 0)
    return variant_part->selector;
  return search_variant_list(variant_part->variants, field);
}

Symbol *search_variant_list(Variant_List *variants, char *field) {
  Symbol *sym = 0;

  while (variants) {
    sym = search_field_list(variants->fields->record.fields, field);
    if (sym)
      return sym;
    variants = variants->next;
  }
  return sym;
}

Symbol *create_tempory_with_variable(Type *type) {
  return new_variable_symbol(0, type);
}

/* e could be a file or a pointer */
Expression *new_indirect_expression(Expression *e) {
  if (e->type->class == POINTER_TYPE)
    return new_unary_expression(INDIRECT_EXPRESSION, e->type->pointer, e);
  if (e->type->class == FILE_TYPE)
    return new_unary_expression(FILE_ACCESS, e->type->file.base, e);
  error("illegal type for ^ operator");
  return 0;
}

Expression *new_function_call(Symbol *sym, Expression_List *params) {
  Expression *e = new_expression(FUNCTION_CALL, sym->algorithm.type);
  e->call.sym = sym;
  e->call.params = params;
  return e;
}

Expression *new_set_constructor(Expression_List *exprs) {
  Expression *e;
  Type *check_type;
  Expression_List *check_exprs;

  if (exprs == 0)
    e = new_expression(SET_CONSTRUCTOR, empty_set_type);
  else {
    /* e is a type set of some ordinal type */

    
    if (exprs->expr->type->set.base->class != ORDINAL_TYPE)
      error("non ordinal types not allowed in sets");
    else {
      check_type = exprs->expr->type->set.base;
      check_exprs = exprs->next;
      while (check_exprs) {
	if (check_exprs->expr->type->set.base->class != ORDINAL_TYPE)
	  error("non ordinal types not allowed in sets");
	else if (check_type->ordinal.base != check_exprs->expr->type->set.base->ordinal.base)
	  error("types in set constructor not the same");
	check_exprs = check_exprs->next;
      }
    }
  }
  e = new_expression(SET_CONSTRUCTOR, new_set_type(false, check_type));
  e->set = exprs;
  return e;
}

Expression *new_set_expression(Expression *e) {
  if (e->type->class != ORDINAL_TYPE) {
    error("set type must be ordinal");
    return 0;
  }
  return new_unary_expression(SET_EXPRESSION, new_set_type(false, e->type->ordinal.base), e);
}

Expression *new_empty_set() {
  return new_expression(EMPTY_SET_EXPRESSION, empty_set_type);
}

Expression *new_set_range_expression(Expression *e, Expression *f) {
  if (e->type->class != ORDINAL_TYPE || f->type->class != ORDINAL_TYPE)
    error("set types must be ordinal");
  else {
    if (e->type->ordinal.base != f->type->ordinal.base)
      error("types in set range not the same");
  }
  return new_binary_expression(SET_RANGE_EXPRESSION, new_set_type(false, e->type->ordinal.base), e, f);
}

Expression *new_procedural_parameter_expression(Symbol *proc, Symbol *check) {
  /* I should be checking to see if proc and check have the same types */
  Expression *e = new_expression(PROCEDURAL_PARAMETER_EXPRESSION, proc->algorithm.type);
  e->variable = proc;
  return e;
}

Expression *new_functional_parameter_expression(Symbol *func, Symbol *check) {
  Expression *e = new_expression(FUNCTIONAL_PARAMETER_EXPRESSION, func->algorithm.type);
  e->variable = func;
  return e;
}

Expression *new_abs_function(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(INTEGER_ABS_EXPRESSION, integer_type, e);
  if (is_real(e->type))
    return new_unary_expression(REAL_ABS_EXPRESSION, real_type, e);
  error("illegal type for abs function");
  return 0;
}

Expression *new_sqr_function(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(INTEGER_SQR_EXPRESSION, integer_type, e);
  if (is_real(e->type))
    return new_unary_expression(REAL_SQR_EXPRESSION, real_type, e);
  error("illegal type for sqr function");
  return 0;
}

Expression *new_sqrt_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(SQRT_EXPRESSION, real_type, e);
  error("illegal type for sqrt function");
  return 0;
}

Expression *new_sin_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(SIN_EXPRESSION, real_type, e);
  error("illegal type for sin function");
  return 0;
}

Expression *new_cos_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(COS_EXPRESSION, real_type, e);
  error("illegal type for cos function");
  return 0;
}

Expression *new_arctan_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(ARCTAN_EXPRESSION, real_type, e);
  error("illegal type for arctan function");
  return 0;
}

Expression *new_exp_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(EXP_EXPRESSION, real_type, e);
  error("illegal type for exp function");
  return 0;
}

Expression *new_ln_function(Expression *e) {
  if (is_integer(e->type))
    e = new_integer_to_real_expression(e);
  if (is_real(e->type))
    return new_unary_expression(LN_EXPRESSION, real_type, e);
  error("illegal type for ln function");
  return 0;
}

Expression *new_trunc_function(Expression *e) {
  if (is_integer(e->type))
    return new_integer_to_real_expression(e); /* no need for trunc! */
  if (is_real(e->type))
    return new_unary_expression(TRUNC_EXPRESSION, real_type, e);
  error("illegal type for trunc function");
  return 0;
}

Expression *new_round_function(Expression *e) {
  if (is_integer(e->type))
    return new_integer_to_real_expression(e); /* no need for round! */
  if (is_real(e->type))
    return new_unary_expression(ROUND_EXPRESSION, real_type, e);
  error("illegal type for round function");
  return 0;
}

Expression *new_odd_function(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(ODD_EXPRESSION, boolean_type, e);
  error("illegal type for odd function");
  return 0;
}

Expression *new_ord_function(Expression *e) {
  if (e->type->class == ORDINAL_TYPE)
    return new_unary_expression(ORD_EXPRESSION, integer_type, e);
  error("illegal type for ord function");
  return 0;
}

Expression *new_succ_function(Expression *e) {
  if (e->type->class == ORDINAL_TYPE)
    return new_unary_expression(SUCC_EXPRESSION, e->type, e);
  error("illegal type for succ function");
  return 0;
}

Expression *new_pred_function(Expression *e) {
  if (e->type->class == ORDINAL_TYPE)
    return new_unary_expression(PRED_EXPRESSION, e->type, e);
  error("illegal type for succ function");
  return 0;
}

Expression *new_chr_function(Expression *e) {
  if (is_integer(e->type))
    return new_unary_expression(CHR_EXPRESSION, char_type, e);
  error("illegal type for chr function");
  return 0;
}

Expression *new_eof_function(Expression *e) {
  if (e->type->class == FILE_TYPE)
    return new_unary_expression(EOF_EXPRESSION, boolean_type, e);
  error("eof parameter not a file");
  return 0;
}

Expression *new_eoln_function(Expression *e) {
  if (e->type->class == FILE_TYPE)
    return new_unary_expression(EOLN_EXPRESSION, boolean_type, e);
  error("eoln parameter not a file");
  return 0;
}

Expression *new_argc_function() {
  return new_expression(ARGC_EXPRESSION, integer_type);
}

Expression *new_unary_expression(Expression_Class class, Type *type,
				 Expression *e) {
  Expression *f = new_expression(class, type);
  f->val = e;
  return f;
}

Expression *new_binary_expression(Expression_Class class, Type *type,
				  Expression *e, Expression *f) {
  Expression *g = new_expression(class, type);
  g->lval = e;
  g->rval = f;
  return g;
}

long align_up(long x, long size) {
  return x + (-8&(size+7));
}

long align_down(long x, long size) {
  long align;

  if (size > 4)
    align = 8;
  else if (size > 2)
    align = 4;
  else if (size > 1)
    align = 2;
  else
    align = 1;
  x -= size;
  x &= -align; /* x &= ~(align-1); */
  return x;
}

bool compatible(Type *t1, Type *t2) {
  if (t1 == t2)
    return true;
  if (t1->class == ORDINAL_TYPE && t2->class == ORDINAL_TYPE)
    return t1->ordinal.base == t2->ordinal.base;
  if (is_set(t1) && is_set(t2)) {
    if (t1->set.packed == t2->set.packed)
      return compatible(t1->set.base, t2->set.base);
    return false;
  }
  if (t1->class == POINTER_TYPE && t2->class == POINTER_TYPE)
    return t1->pointer == 0 || t2->pointer == 0 || compatible(t1->pointer, t2->pointer);
  if (is_string(t1) && is_string(t2))
    return t1->array.index_type->ordinal.high == t2->array.index_type->ordinal.high;
  return false;
}

bool assignment_compatible(Type *t1, Type *t2) {
  if (t1 == t2) /* this doesn't check if the types are allowed in files */
    return true;
  if (is_real(t1) && is_integer(t2))
    return true;
  if (t1->class == ORDINAL_TYPE && t2->class == ORDINAL_TYPE)
    return t1->ordinal.base == t2->ordinal.base;
  if (is_set(t1) && is_set(t2))
    return assignment_compatible(t1->set.base, t2->set.base);
  if (is_string(t1) && is_string(t2))
    return compatible(t1, t2);
  return false;
}
