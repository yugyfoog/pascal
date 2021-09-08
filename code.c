#include <stdio.h>
#include <stdlib.h>
#include "machine.h"
#include "pascal.h"
#include "code.h"
#include "gen.h"

Symbol *current_algorithm;

void code_startup(Symbol *);
void code_program_parameters(Symbol_List *);
void code_program_parameter(Symbol *);
void code_algorithms(Symbol_List *);
void code_algorithm(Symbol *);
void code_procedure(Symbol *);
void code_function(Symbol *);
Code *code_program_prologue(Symbol *);
Code *code_program_epilogue();
Code *code_procedure_prologue(Symbol *);
Code *code_procedure_epilogue(Symbol *);
Code *code_function_prologue(Symbol *);
Code *code_function_epilogue(Symbol *);
Code *code_load_return_value(Symbol *);
Code *code_return_value(Type *);
Code *code_statement(Statement *);
Code *code_labeled_statement(Statement *);
Code *code_compound_statement(Statement_Sequence *);
Code *code_assignment_statement(Expression *, Expression *);
Code *code_if_statement(Expression *, Statement *, Statement *);
Code *code_case_statement(Expression *, Case_Element_List *);
Code *code_case_elements(Case_Element_List *, int, int);
void find_range(Case_Element_List *, long *, long *);
void find_range_list(Constant_List *, long *, long *);
Code *build_jump_table(Case_Element_List *, long, long, int, int);
Code *code_goto_statement(Symbol *);
Code *code_local_goto(Symbol *);
Code *code_interprocedural_goto(Symbol *);
Code *code_while_statement(Expression *, Statement *);
Code *code_repeat_statement(Statement_Sequence *, Expression *);
Code *code_for_statement(Statement *);
Code *code_for_to_statement(Statement *);
Code *code_for_downto_statement(Statement *);
Code *code_with_statement(Expression *, Symbol *, Statement *);
Code *code_reset(Expression *, Expression *);
Code *code_rewrite(Expression *, Expression *);
Code *code_get(Expression *);
Code *code_put(Expression *);
Code *code_read(Symbol *, Expression *);
Code *code_read_text(Symbol *, Expression *);
Code *code_read_ordinal(Symbol *, Expression *);
Code *code_read_char(Symbol *, Expression *);
Code *code_read_integer(Symbol *, Expression *);
Code *code_read_real(Symbol *, Expression *);
Code *code_readln(Symbol *);
Code *code_write(Symbol *, Expression *);
Code *code_write_text(Symbol *, Expression *, Expression *, Expression *);
Code *code_write_ordinal(Symbol *, Expression *, Expression *);
Code *code_write_boolean(Symbol *, Expression *, Expression *);
Code *code_write_char(Symbol *, Expression *, Expression *);
Code *code_write_integer(Symbol *, Expression *, Expression *);
Code *code_write_real(Symbol *, Expression *, Expression *, Expression *);
Code *code_write_real_float(Symbol *, Expression *, Expression *);
Code *code_write_real_fixed(Symbol *, Expression *, Expression *, Expression *);
Code *code_write_string(Symbol *, Expression *, Expression *);
Code *code_writeln(Symbol *);
Code *code_page(Expression *);
Code *code_new(Expression *);
Code *code_dispose(Expression *);
Code *code_pack(Expression *, Expression *, Expression *);
Code *code_unpack(Expression *, Expression *, Expression *);
Code *code_argv(Expression *, Expression *);
Code *code_flush(Expression *);
Code *code_close(Expression *);
Code *code_load_value(Expression *);
Code *code_load_address(Expression *);
Code *code_load_variable(Symbol *);
Code *code_load_variable_address(Symbol *);
Code *code_load_index_address(Expression *, Expression *);
Code *code_fetch(Type *);
Code *code_load_constant(Constant *);
Code *code_store(Type *);
Code *code_push(Type *);
Code *code_indirect(Expression *);
Code *code_file_access_address(Expression *);
Code *code_file_access(Expression *);
Code *code_not(Expression *);
Code *code_ordinal_eq(Expression *, Expression *);
Code *code_real_eq(Expression *, Expression *);
Code *code_string_eq(Expression *, Expression *);
Code *code_set_eq(Expression *, Expression *);
Code *code_pointer_eq(Expression *, Expression *);
Code *code_ordinal_ne(Expression *, Expression *);
Code *code_real_ne(Expression *, Expression *);
Code *code_string_ne(Expression *, Expression *);
Code *code_set_ne(Expression *, Expression *);
Code *code_pointer_ne(Expression *, Expression *);
Code *code_ordinal_lt(Expression *, Expression *);
Code *code_real_lt(Expression *, Expression *);
Code *code_string_lt(Expression *, Expression *);
Code *code_ordinal_le(Expression *, Expression *);
Code *code_real_le(Expression *, Expression *);
Code *code_string_le(Expression *, Expression *);
Code *code_set_le(Expression *, Expression *);
Code *code_ordinal_gt(Expression *, Expression *);
Code *code_real_gt(Expression *, Expression *);
Code *code_string_gt(Expression *, Expression *);
Code *code_ordinal_ge(Expression *, Expression *);
Code *code_real_ge(Expression *, Expression *);
Code *code_string_ge(Expression *, Expression *);
Code *code_set_ge(Expression *, Expression *);
Code *code_integer_plus(Expression *);
Code *code_integer_minus(Expression *);
Code *code_integer_add(Expression *, Expression *);
Code *code_integer_subtract(Expression *, Expression *);
Code *code_integer_multiply(Expression *, Expression *);
Code *code_integer_divide(Expression *, Expression *);
Code *code_integer_modulus(Expression *, Expression *);
Code *code_or(Expression *, Expression *);
Code *code_and(Expression *, Expression *);
Code *code_integer_abs(Expression *);
Code *code_integer_sqr(Expression *);
Code *code_odd(Expression *);
Code *code_ord(Expression *);
Code *code_chr(Expression *);
Code *code_succ(Expression *);
Code *code_pred(Expression *);
Code *code_eof(Expression *);
Code *code_eoln(Expression *);
Code *code_argc(void);
Code *code_real_plus(Expression *);
Code *code_real_minus(Expression *);
Code *code_real_add(Expression *, Expression *);
Code *code_real_subtract(Expression *, Expression *);
Code *code_real_multiply(Expression *, Expression *);
Code *code_real_divide(Expression *, Expression *);
Code *code_real_abs(Expression *);
Code *code_real_sqr(Expression *);
Code *code_sqrt(Expression *);
Code *code_sin(Expression *);
Code *code_cos(Expression *);
Code *code_arctan(Expression *);
Code *code_exp(Expression *);
Code *code_ln(Expression *);
Code *code_trunc(Expression *);
Code *code_round(Expression *);
Code *code_integer_to_real(Expression *);
Code *code_index(Expression *, Expression *);
Code *code_field(Expression *, Symbol *);
Code *code_field_address(Expression *, Symbol *);
Code *code_empty_set(void);
Code *code_set_constructor(Expression_List *);
Code *code_set(Expression *);
Code *code_set_range(Expression *, Expression *);
Code *code_set_union(Expression *, Expression *);
Code *code_set_intersection(Expression *, Expression *);
Code *code_set_difference(Expression *, Expression *);
Code *code_in(Expression *, Expression *);
Code *code_function_call(Symbol *, Expression_List *);
Code *code_procedure_call(Symbol *, Expression_List *);
Code *code_parameters(Symbol_List *, Expression_List *);
Code *code_value_parameter(Expression *);
Code *code_variable_parameter(Expression *);
Code *code_procedural_parameter(Symbol *);
Code *code_functional_parameter(Symbol *);
Code *sequence2(Code *, Code *);
Code *sequence3(Code *, Code *, Code *);
Code *sequence4(Code *, Code *, Code *, Code *);
Code *sequence5(Code *, Code *, Code *, Code *, Code *);
Code *sequence6(Code *, Code *, Code *, Code *, Code *, Code *);
Code *sequence7(Code *, Code *, Code *, Code *, Code *, Code *, Code *);
Code *sequence9(Code *, Code *, Code *, Code *, Code *,
		Code *, Code *, Code *, Code *);
Code *new_code(Operation);
Code *new_statement_label(char *);
Code *new_internal_label(int);
Code *new_enter_op(long, int);
Code *new_leave_op(void);
Code *new_return_op(long);
Code *new_function_call_op(char *);
Code *new_function_call_indirect_op(void);
Code *new_procedure_call_op(char *);
Code *new_procedure_call_indirect_op(void);
Code *new_file_access_op(void);
Code *new_load_ordinal_constant_op(long);
Code *new_load_real_constant_op(double);
Code *new_load_string_constant_op(char *);
Code *new_load_nil(void);
Code *new_load_variable_address_op(long, long, long);
Code *new_fetch_signed_op(int);
Code *new_fetch_unsigned_op(int);
Code *new_fetch_real_op(void);
Code *new_fetch_set_op(void);
Code *new_fetch_op(void);
Code *new_load_algorithm_op(char *);
Code *new_return_signed_op(int);
Code *new_return_unsigned_op(int);
Code *new_return_real_op(void);
Code *new_return_pointer_op(void);
Code *new_store_op(long);
Code *new_store_real_op(void);
Code *new_store_set_op(void);
Code *new_move_op(long);
Code *new_nop_op(void);
Code *new_integer_to_real_op(void);
Code *new_ordinal_eq_op(void);
Code *new_real_eq_op(void);
Code *new_string_eq_op(long);
Code *new_pointer_eq_op(void);
Code *new_set_eq_op(void);
Code *new_ordinal_ne_op(void);
Code *new_real_ne_op(void);
Code *new_string_ne_op(long);
Code *new_pointer_ne_op(void);
Code *new_set_ne_op(void);
Code *new_ordinal_lt_op(void);
Code *new_real_lt_op(void);
Code *new_string_lt_op(long);
Code *new_ordinal_le_op(void);
Code *new_real_le_op(void);
Code *new_string_le_op(long);
Code *new_set_le_op(void);
Code *new_ordinal_gt_op(void);
Code *new_real_gt_op(void);
Code *new_string_gt_op(long);
Code *new_ordinal_ge_op(void);
Code *new_real_ge_op(void);
Code *new_string_ge_op(long);
Code *new_set_ge_op(void);
Code *new_integer_negate_op(void);
Code *new_integer_add_op(void);
Code *new_integer_subtract_op(void);
Code *new_integer_multiply_op(void);
Code *new_integer_divide_op(void);
Code *new_integer_modulus_op(void);
Code *new_real_negate_op(void);
Code *new_real_add_op(void);
Code *new_real_subtract_op(void);
Code *new_real_multiply_op(void);
Code *new_real_divide_op(void);
Code *new_not_op(void);
Code *new_or_op(void);
Code *new_and_op(void);
Code *new_integer_abs_op(void);
Code *new_real_abs_op(void);
Code *new_integer_sqr_op(void);
Code *new_real_sqr_op(void);
Code *new_sqrt_op(void);
Code *new_sin_op(void);
Code *new_cos_op(void);
Code *new_arctan_op(void);
Code *new_exp_op(void);
Code *new_ln_op(void);
Code *new_trunc_op(void);
Code *new_round_op(void);
Code *new_odd_op(void);
Code *new_inc_op(void);
Code *new_dec_op(void);
Code *new_eof_op(void);
Code *new_eoln_op(void);
Code *new_argc_op(void);
Code *new_empty_set_op(void);
Code *new_set_union_op(void);
Code *new_set_intersection_op(void);
Code *new_set_difference_op(void);
Code *new_to_set_op(void);
Code *new_to_set_range_op(void);
Code *new_in_op(void);
Code *new_jump_op(int);
Code *new_jump_true_op(int);
Code *new_jump_false_op(int);
Code *new_jump_label_op(char *);
Code *new_jump_index(int, long, int);
Code *new_jump_table(int *, long, int);
Code *new_case_error_op(void);
Code *new_interprocedural_jump_op(long, long, char *);
Code *new_get_op(void);
Code *new_put_op(void);
Code *new_reset_op(void);
Code *new_named_reset_op(void);
Code *new_reset_text_op(void);
Code *new_named_reset_text_op(void);
Code *new_rewrite_op(void);
Code *new_named_rewrite_op(void);
Code *new_rewrite_text_op(void);
Code *new_named_rewrite_text_op(void);
Code *new_read_char_op(void);
Code *new_read_integer_op(void);
Code *new_read_real_op(void);
Code *new_readln_op(void);
Code *new_write_direct_op(void);
Code *new_write_indirect_op(void);
Code *new_write_set_op(void);
Code *new_write_boolean_op(void);
Code *new_write_char_op(void);
Code *new_write_integer_op(void);
Code *new_write_real_float_op(void);
Code *new_write_real_fixed_op(void);
Code *new_write_string_op(void);
Code *new_writeln_op(void);
Code *new_page_op(void);
Code *new_new_op(void);
Code *new_dispose_op(void);
Code *new_argv_op(void);
Code *new_flush_op(void);
Code *new_close_op(void);

void code_program(Symbol *prog) {
  Code *code;
  
  code_startup(prog);

  current_algorithm = prog;

  code = sequence3(code_program_prologue(prog),
		   code_statement(prog->algorithm.statement),
		   code_program_epilogue(prog));

  gen_text_label(prog->name);
  gen_code(code);

  code_algorithms(prog->algorithm.algorithms);
  gen_dump_bits();
  gen_dump_strings();
}

/* generate runtime startup code */

void code_startup(Symbol *prog) {
  gen_startup1();
  code_program_parameters(prog->algorithm.parameters);
  gen_startup2(prog->name);
}

void code_program_parameters(Symbol_List *params) {
  if (params) {
    code_program_parameters(params->next);
    code_program_parameter(params->symbol);
  }
}

void code_program_parameter(Symbol *param) {
  gen_push_program_parameter(param->name);
}

Code *code_program_prologue(Symbol *prog) {
  return new_enter_op(prog->algorithm.local_size, prog->block_level);
}

Code *code_program_epilogue(Symbol *prog) {
  return sequence2(new_leave_op(),
		   new_return_op(prog->algorithm.parameter_size));
}

Code *code_procedure_prologue(Symbol *proc) {
  return new_enter_op(proc->algorithm.local_size, proc->block_level);
}

Code *code_procedure_epilogue(Symbol *proc) {
  return sequence2(new_leave_op(),
		   new_return_op(proc->algorithm.parameter_size));
}

Code *code_function_prologue(Symbol *func) {
  return new_enter_op(func->algorithm.local_size, func->block_level);
}

Code *code_function_epilogue(Symbol *func) {
  return sequence3(code_load_return_value(func->algorithm.return_value),
		   new_leave_op(),
		   new_return_op(func->algorithm.parameter_size));
}

Code *code_load_return_value(Symbol *sym) {
  return sequence2(code_load_variable_address(sym),
		   code_return_value(sym->variable.type));
}

Code *code_return_value(Type *t) {
  switch (t->class) {
  case ORDINAL_TYPE:
    if (t->ordinal.low < 0)
      return new_return_signed_op(t->size);
    return new_return_unsigned_op(t->size);
  case REAL_TYPE:
    return new_return_real_op();
  case POINTER_TYPE:
    return new_return_pointer_op();
  default:
    internal_error();
  }
  return 0;
}

void code_algorithms(Symbol_List *subs) {
  while (subs) {
    code_algorithm(subs->symbol);
    subs = subs->next;
  }
}

void code_algorithm(Symbol *sym) {
  switch (sym->class) {
  case PROCEDURE_SYMBOL:
    code_procedure(sym);
    break;
  case FUNCTION_SYMBOL:
    code_function(sym);
    break;
  default:
    internal_error();
  }  
}

void code_procedure(Symbol *proc) {
  Code *code;

  current_algorithm = proc;

  code = sequence3(code_procedure_prologue(proc),
		   code_statement(proc->algorithm.statement),
		   code_procedure_epilogue(proc));
  gen_text_label(proc->name);
  gen_code(code);

  code_algorithms(proc->algorithm.algorithms);    
}

void code_function(Symbol *func) {
  Code *code;
  
  current_algorithm = func;

  code = sequence3(code_function_prologue(func),
		  code_statement(func->algorithm.statement),
		  code_function_epilogue(func));
  gen_text_label(func->name);
  gen_code(code);

  code_algorithms(func->algorithm.algorithms); 
}
    
Code *code_statement(Statement *stmt) {
  if (stmt->label)
    return code_labeled_statement(stmt);
  switch (stmt->class) {
  case EMPTY_STATEMENT:
    return 0; /* nothing to do */
  case COMPOUND_STATEMENT:
    return code_compound_statement(stmt->compound_statement);
  case ASSIGNMENT_STATEMENT:
    return code_assignment_statement(stmt->assignment.lval,
				     stmt->assignment.rval);
  case PROCEDURE_CALL:
    return code_procedure_call(stmt->call.sym, stmt->call.params);
  case GOTO_STATEMENT:
    return code_goto_statement(stmt->gotostmt);
  case IF_STATEMENT:
    return code_if_statement(stmt->ifstmt.test,
			     stmt->ifstmt.tstmt,
			     stmt->ifstmt.fstmt);
  case CASE_STATEMENT:
    return code_case_statement(stmt->casestmt.index, stmt->casestmt.elements);
  case WHILE_STATEMENT:
    return code_while_statement(stmt->whilestmt.test, stmt->whilestmt.body);
  case REPEAT_STATEMENT:
    return code_repeat_statement(stmt->repeatstmt.body, stmt->repeatstmt.test);
  case FOR_STATEMENT:
    return code_for_statement(stmt);
  case WITH_STATEMENT:
    return code_with_statement(stmt->with.record,
			       stmt->with.variable,
			       stmt->with.body);
  case RESET_STATEMENT:
    return code_reset(stmt->reset_rewrite.file, stmt->reset_rewrite.name);
  case REWRITE_STATEMENT:
    return code_rewrite(stmt->reset_rewrite.file, stmt->reset_rewrite.name);
  case GET_STATEMENT:
    return code_get(stmt->parameter);
  case PUT_STATEMENT:
    return code_put(stmt->parameter);
  case READ_STATEMENT:
    return code_read(stmt->read.file, stmt->read.expression);
  case READ_TEXT_STATEMENT:
    return code_read_text(stmt->read.file, stmt->read.expression);
  case READLN_STATEMENT:
    return code_readln(stmt->readln);
  case WRITE_STATEMENT:
    return code_write(stmt->write.file, stmt->write.expression);
  case WRITE_TEXT_STATEMENT:
    return code_write_text(stmt->write.file,
			   stmt->write.expression,
			   stmt->write.field_width,
			   stmt->write.fractional_digits);
  case WRITELN_STATEMENT:
    return code_writeln(stmt->writeln);
  case PAGE_STATEMENT:
    return code_page(stmt->parameter);
  case NEW_STATEMENT:
    return code_new(stmt->parameter);
  case DISPOSE_STATEMENT:
    return code_dispose(stmt->parameter);
  case PACK_STATEMENT:
    return code_pack(stmt->pack.a, stmt->pack.i, stmt->pack.z);
  case UNPACK_STATEMENT:
    return code_unpack(stmt->pack.z, stmt->pack.a, stmt->pack.i);
  case ARGV_STATEMENT:
    return code_argv(stmt->argv.index, stmt->argv.arg);
  case FLUSH_STATEMENT:
    return code_flush(stmt->parameter);
  case CLOSE_STATEMENT:
    return code_close(stmt->parameter);
  }
  internal_error();
  return 0;
}

Code *code_labeled_statement(Statement *stmt) {
  Code *code = new_statement_label(stmt->label->name);
  stmt->label = 0;
  return sequence2(code, code_statement(stmt));
}

Code *code_assignment_statement(Expression *lval, Expression *rval) {
  return sequence3(code_load_value(rval),
		   code_load_address(lval),
		   code_store(lval->type));
}

Code *code_compound_statement(Statement_Sequence *stmts) {
  Code *code = 0;
  
  while (stmts) {
    code = sequence2(code, code_statement(stmts->statement));
    stmts = stmts->next;
  };
  return code;
}

Code *code_if_statement(Expression *test, Statement *tstmt, Statement *fstmt) {
  Code *code;
  int l1, l2;
  
  l1 = new_label();
  code = sequence3(code_load_value(test),
		   new_jump_false_op(l1),
		   code_statement(tstmt));
  if (fstmt) {
    l2 = new_label();
    code = sequence5(code,
		     new_jump_op(l2),
		     new_internal_label(l1),
		     code_statement(fstmt),
		     new_internal_label(l2));
  }
  else {
    code = sequence2(code,
		     new_internal_label(l1));
  }
  return code;
}

Code *code_case_statement(Expression *index, Case_Element_List *elements) {
  Code *code;
  long minimum, maximum;
  int l1 = new_label(); /* label of jump table */
  int l2 = new_label(); /* label of end of case statement */
  int l3 = new_label(); /* otherwise label */
  minimum = maximum = elements->constants->cnst->ordinal;
  find_range(elements, &minimum, &maximum);

  code = sequence5(code_load_value(index),
		   new_load_ordinal_constant_op(minimum),
		   new_integer_subtract_op(),
		   new_jump_index(l1, maximum-minimum+1, l3),
		   build_jump_table(elements, minimum, maximum, l1, l3));
  /*
    I need to make sure build_jump_table is called before code_case_elements
  */
  return sequence3(code,
		   code_case_elements(elements, l2, l3),
		   new_internal_label(l2));
}

void find_range(Case_Element_List *elements, long *minimum, long *maximum) {
  while (elements) {
    find_range_list(elements->constants, minimum, maximum);
    elements = elements->next;
  }
}

void find_range_list(Constant_List *cnsts, long *minimum, long *maximum) {
  while (cnsts) {
    if (cnsts->cnst->ordinal < *minimum)
      *minimum = cnsts->cnst->ordinal;
    if (cnsts->cnst->ordinal > *maximum)
      *maximum = cnsts->cnst->ordinal;
    cnsts = cnsts->next;
  }
}

Code *build_jump_table(Case_Element_List *elements, long minimum, long maximum, int label, int other_label) {
  Constant_List *cnsts;
  long size = maximum - minimum + 1;
  int *table = calloc(size, sizeof(int));
  
  while (elements) {
    if (elements->constants == 0) /* otherwise */
      elements->label = other_label;
    else {
      elements->label = new_label();
      for (cnsts = elements->constants; cnsts; cnsts = cnsts->next)
	table[cnsts->cnst->ordinal - minimum] = elements->label;
    }
    elements = elements->next;
  }
  return sequence2(new_internal_label(label),
		   new_jump_table(table, size, other_label));
}

Code *code_case_elements(Case_Element_List *elements, int case_end, int otherwise_label) {
  Code *code = 0;
  bool otherwise_part = false;
  
  while (elements) {
    if (elements->constants == 0)
      otherwise_part = elements->label;
    code = sequence4(code,
		     new_internal_label(elements->label),
		     code_statement(elements->statement),
		     new_jump_op(case_end));
    elements = elements->next;
  }
  if (!otherwise_part)
    code = sequence3(code,
		     new_internal_label(otherwise_label),
		     new_case_error_op());
  return code;
}

Code *code_goto_statement(Symbol *label) {
  if (label->label.algorithm == current_algorithm)
    return code_local_goto(label);
  return code_interprocedural_goto(label);
}

Code *code_local_goto(Symbol *label) {
  return new_jump_label_op(label->name);
}

Code *code_interprocedural_goto(Symbol *label) {
  return new_interprocedural_jump_op(label->label.algorithm->block_level,
				     label->label.algorithm->algorithm.local_size,
				     label->name);
}

Code *code_while_statement(Expression *test, Statement *stmt) {
  Code *code;
  int l1, l2;

  l1 = new_label();
  l2 = new_label();
  code = sequence6(new_internal_label(l1),
		   code_load_value(test),
		   new_jump_false_op(l2),
		   code_statement(stmt),
		   new_jump_op(l1),
		   new_internal_label(l2));
  return code;
}

Code *code_repeat_statement(Statement_Sequence *body, Expression *test) {
  Code *code;
  int lab;

  lab = new_label();
  code = sequence4(new_internal_label(lab),
		   code_compound_statement(body),
		   code_load_value(test),
		   new_jump_false_op(lab));
  return code;
}

Code *code_for_statement(Statement *stmt) {
  switch (stmt->forstmt.mode) {
  case TO:
    return code_for_to_statement(stmt);
  case DOWNTO:
    return code_for_downto_statement(stmt);
  }
  return 0;
}

Code *code_for_to_statement(Statement *stmt) {
  Code *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8, *p9;
  int l1, l2;
  
  l1 = new_label();
  l2 = new_label();
  
  /* temp1 = <initial value> */
  p1 = sequence3(code_load_value(stmt->forstmt.initial_value),
		 code_load_variable_address(stmt->forstmt.temp1),
		 code_store(integer_type));

  /* temp2 = <final value> */
  p2 = sequence3(code_load_value(stmt->forstmt.final_value),
		 code_load_variable_address(stmt->forstmt.temp2),
		 code_store(integer_type));

  /* if (temp1 > temp2)
         goto L1           */
  p3 = sequence4(code_load_variable(stmt->forstmt.temp1),
		 code_load_variable(stmt->forstmt.temp2),
		 new_ordinal_gt_op(),
		 new_jump_true_op(l1));

  /* v = temp1 */
  p4 = sequence3(code_load_variable(stmt->forstmt.temp1),
		 code_load_variable_address(stmt->forstmt.control_variable),
		 code_store(stmt->forstmt.control_variable->type));

  /* L2: */

  /* <body> */
  p5 = sequence2(new_internal_label(l2),
		 code_statement(stmt->forstmt.body));

  /* if (v == temp2)
         goto L1       */
  p6 = sequence4(code_load_variable(stmt->forstmt.control_variable),
		 code_load_variable(stmt->forstmt.temp2),
		 new_ordinal_eq_op(),
		 new_jump_true_op(l1));

  /* inc v */
  p7 = sequence4(code_load_variable(stmt->forstmt.control_variable),
		 new_inc_op(),
		 code_load_variable_address(stmt->forstmt.control_variable),
		 code_store(stmt->forstmt.control_variable->type));

  /* goto L2 */
  p8 = new_jump_op(l2);

  /* L1 */
  p9 = new_internal_label(l1);

  return sequence9(p1, p2, p3, p4, p5, p6, p7, p8, p9);
}
  
Code *code_for_downto_statement(Statement *stmt) {
  Code *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8, *p9;
  int l1, l2;

  l1 = new_label();
  l2 = new_label();

  /* temp1 = <initial value> */
  p1 = sequence3(code_load_value(stmt->forstmt.initial_value),
		 code_load_variable_address(stmt->forstmt.temp1),
		 code_store(integer_type));

  /* temp2 = <final value> */
  p2 = sequence3(code_load_value(stmt->forstmt.final_value),
		 code_load_variable_address(stmt->forstmt.temp2),
		 code_store(integer_type));

  /*if (temp1 < temp2)
        goto L1     */
  p3 = sequence4(code_load_variable(stmt->forstmt.temp1),
		 code_load_variable(stmt->forstmt.temp2),
		 new_ordinal_lt_op(),
		 new_jump_true_op(l1));

  /* v = temp1 */
  p4 = sequence3(code_load_variable(stmt->forstmt.temp1),
		 code_load_variable_address(stmt->forstmt.control_variable),
		 code_store(stmt->forstmt.control_variable->type));

  /* L2: */
  p5 = sequence2(new_internal_label(l2),
		 code_statement(stmt->forstmt.body));
  
  /* if (v == temp2)
     goto L1   */
  p6 = sequence4(code_load_variable(stmt->forstmt.control_variable),
		 code_load_variable(stmt->forstmt.temp2),
		 new_ordinal_eq_op(),
		 new_jump_true_op(l1));

  /* dec v */
  p7 = sequence4(code_load_variable(stmt->forstmt.control_variable),
		 new_dec_op(),
		 code_load_variable_address(stmt->forstmt.control_variable),
		 code_store(stmt->forstmt.control_variable->type));
  /* goto L2 */

  p8 = new_jump_op(l2);

  /* L1 */

  p9 = new_internal_label(l1);

  return sequence9(p1, p2, p3, p4, p5, p6, p7, p8, p9);
}

Code *code_with_statement(Expression *record, Symbol *variable, Statement *body) {
  return sequence4(code_load_address(record),
		   code_load_variable_address(variable),
		   code_store(nil_type),  /* any pointer type will do */
		   code_statement(body));
}

Code *code_reset(Expression *file, Expression *name) {
  Code *code;
  
  if (name)
    if (file->type == text_type)
      /*
	name length
	pointer to name
	pointer to file variable
      */
      code = sequence4(new_load_ordinal_constant_op(name->type->array.index_type->ordinal.high),
		       code_load_value(name),
		       code_load_address(file),
		       new_named_reset_text_op());
    else
      /* 
	 name length
	 pointer to name
	 size of buffer
	 pointer to file variable
      */
      code = sequence5(new_load_ordinal_constant_op(name->type->array.index_type->ordinal.high),
		       code_load_value(name),
		       new_load_ordinal_constant_op(file->type->file.base->size),
		       code_load_address(file),
		       new_named_reset_op());
  else
    if (file->type == text_type)
      /*
	pointer to file variable
      */
      code = sequence2(code_load_address(file),
		       new_reset_text_op());
    else
      /*
	size of buffer
	pointer to file variable
      */
      code = sequence3(new_load_ordinal_constant_op(file->type->file.base->size),
		       code_load_address(file),
		       new_reset_op());
  return code;
}

Code *code_rewrite(Expression *file, Expression *name) {
  Code *code;
  
  if (name)
    if (file->type == text_type)
      /*
	name length
	pointer to name
	pointer to file variable
      */
      code = sequence4(new_load_ordinal_constant_op(name->type->array.index_type->ordinal.high),
		       code_load_value(name),
		       code_load_address(file),
		       new_named_rewrite_text_op());
    else
      /* 
	 name length
	 pointer to name
	 size of buffer
	 pointer to file variable
      */
      code = sequence5(new_load_ordinal_constant_op(name->type->array.index_type->ordinal.high),
		       code_load_value(name),
		       new_load_ordinal_constant_op(file->type->file.base->size),
		       code_load_address(file),
		       new_named_rewrite_op());
  else
    if (file->type == text_type)
      /*
	pointer to file variable
      */
      code = sequence2(code_load_address(file),
		       new_rewrite_text_op());
    else
      /*
	size of buffer
	pointer to file variable
      */
      code = sequence3(new_load_ordinal_constant_op(file->type->file.base->size),
		       code_load_address(file),
		       new_rewrite_op());
  return code;
}

Code *code_get(Expression *file) {
  return sequence2(code_load_value(file),
		   new_get_op());
}

Code *code_put(Expression *file) {
  return sequence2(code_load_value(file),
		   new_put_op());
}

Code *code_read(Symbol *file, Expression *e) {
  return sequence3(new_load_ordinal_constant_op(e->type->size),
		   code_load_address(e),
		   code_load_variable(file));
}

Code *code_read_text(Symbol *file, Expression *e) {
  switch (e->type->class) {
  case ORDINAL_TYPE:
    return code_read_ordinal(file, e);
  case REAL_TYPE:
    return code_read_real(file, e);
  default:
    internal_error();
  }
  return 0;
}

Code *code_read_ordinal(Symbol *file, Expression *e) {
  if (e->type->ordinal.base == char_type)
    return code_read_char(file, e);
  if (e->type->ordinal.base == integer_type)
    return code_read_integer(file, e);
  internal_error();
  return 0;
}

Code *code_read_char(Symbol *file, Expression *e) {
  return sequence3(code_load_address(e),
		   code_load_variable(file),
		   new_read_char_op());
}

Code *code_read_integer(Symbol *file, Expression *e) {
  return sequence3(code_load_address(e),
		   code_load_variable(file),
		   new_read_integer_op());
}

Code *code_read_real(Symbol *file, Expression *e) {
  return sequence3(code_load_address(e),
		   code_load_variable(file),
		   new_read_real_op());
}

Code *code_readln(Symbol *file) {
  return sequence2(code_load_variable(file),
		   new_readln_op());
}

/* write to binary (non-text) file */
Code *code_write(Symbol *file, Expression *e) {
  switch (e->type->class) {
  case ORDINAL_TYPE:
  case REAL_TYPE:
    /* 8-byte value on stack */
    return sequence4(new_load_ordinal_constant_op(e->type->size),
		     code_load_value(e),
		     code_load_variable(file),
		     new_write_direct_op());
  case SET_TYPE:
    /* 32-byte value on stack */
    return sequence3(code_load_value(e),
		     code_load_variable(file),
		     new_write_set_op());
  case NIL_TYPE: /* are we allowed to write these types? */
  case FILE_TYPE:  /* if we are then they leave 8-byte values on the stack */
  case POINTER_TYPE:
    internal_error();
    return 0;
  default:
    /* 8-byte pointer to value on stack */
    return sequence4(new_load_ordinal_constant_op(e->type->size),
		     code_load_value(e),
		     code_load_variable(file),
		     new_write_indirect_op());
  }
}

Code *code_write_text(Symbol *file, Expression *e, Expression *width, Expression *prec) {
  switch (e->type->class) {
  case ORDINAL_TYPE:
    return code_write_ordinal(file, e, width);
  case REAL_TYPE:
    return code_write_real(file, e, width, prec);
  case ARRAY_TYPE:
    return code_write_string(file, e, width);
  default:
    internal_error();
  }
  return 0;
}

Code *code_write_ordinal(Symbol *file, Expression *e, Expression *width) {
  if (e->type->ordinal.base == boolean_type)
    return code_write_boolean(file, e, width);
  if (e->type->ordinal.base == char_type)
    return code_write_char(file, e, width);
  if (e->type->ordinal.base == integer_type)
    return code_write_integer(file, e, width);
  internal_error();
  return 0;
}

Code *code_write_boolean(Symbol *file, Expression *e, Expression *width) {
  Code *code;

  if (width)
    code = code_load_value(width);
  else
    code = new_load_ordinal_constant_op(BOOLEAN_FIELD_WIDTH);
  return sequence4(code,
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_boolean_op());
}

Code *code_write_char(Symbol *file, Expression *e, Expression *width) {
  Code *code;

  if (width)
    code = code_load_value(width);
  else
    code = new_load_ordinal_constant_op(CHAR_FIELD_WIDTH);
  return sequence4(code,
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_char_op());
}

Code *code_write_integer(Symbol *file, Expression *e, Expression *width) {
  Code *code;
  
  if (width)
    code = code_load_value(width);
  else
    code = new_load_ordinal_constant_op(INTEGER_FIELD_WIDTH);
  return sequence4(code,
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_integer_op());
}

Code *code_write_real(Symbol *file, Expression *e, Expression *width, Expression *prec) {
  if (prec == 0)
    return code_write_real_float(file, e, width);
  return code_write_real_fixed(file, e, width, prec);
}

Code *code_write_real_float(Symbol *file, Expression *e, Expression *width) {
  Code *code;
  
  if (width)
    code = code_load_value(width);
  else
    code = new_load_ordinal_constant_op(REAL_FIELD_WIDTH);
  return sequence4(code,
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_real_float_op());
}

Code *code_write_real_fixed(Symbol *file, Expression *e, Expression *width, Expression *prec) {
  return sequence5(code_load_value(prec),
		   code_load_value(width),
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_real_fixed_op());
}

Code *code_write_string(Symbol *file, Expression *e, Expression *width) {
  Code *code;

  if (width)
    code = code_load_value(width);
  else
    code = new_load_ordinal_constant_op(STRING_FIELD_WIDTH);
  return sequence5(code,
		   new_load_ordinal_constant_op(e->type->array.index_type->ordinal.high),
		   code_load_value(e),
		   code_load_variable(file),
		   new_write_string_op());
}

Code *code_writeln(Symbol *file) {
  return sequence2(code_load_variable(file),
		   new_writeln_op());
}

Code *code_page(Expression *e) {
  return sequence2(code_load_value(e),
		   new_page_op());
}

Code *code_new(Expression *e) {
  return sequence3(new_load_ordinal_constant_op(e->type->pointer->size),
		   code_load_address(e),
		   new_new_op());
}

Code *code_dispose(Expression *e) {
  return sequence2(code_load_value(e), new_dispose_op());
}

Code *code_pack(Expression *a, Expression *i, Expression *z) {
  return sequence3(code_load_address(z),
		   code_load_index_address(a, i),
		   code_store(z->type));
}

Code *code_unpack(Expression *z, Expression *a, Expression *i) {
  return sequence3(code_load_index_address(a, i),
		   code_load_address(z),
		   code_store(z->type));
}

Code *code_argv(Expression *index, Expression *arg) {
  return sequence4(new_load_ordinal_constant_op(arg->type->array.index_type->ordinal.high),
		   code_load_address(arg),
		   code_load_value(index),
		   new_argv_op());
}

Code *code_flush(Expression *e) {
  return sequence2(code_load_value(e),
		   new_flush_op());
}

Code *code_close(Expression *e) {
  return sequence2(code_load_value(e),
		   new_close_op());
}

Code *code_load_value(Expression *e) {
  switch (e->class) {
  case VARIABLE_EXPRESSION:
    return code_load_variable(e->variable);
  case CONSTANT_EXPRESSION:
    return code_load_constant(e->constant);
  case INDIRECT_EXPRESSION:
    return code_indirect(e->val);
  case FILE_ACCESS:
    return code_file_access(e->val);
  case ORDINAL_EQ_EXPRESSION:
    return code_ordinal_eq(e->lval, e->rval);
  case REAL_EQ_EXPRESSION:
    return code_real_eq(e->lval, e->rval);
  case STRING_EQ_EXPRESSION:
    return code_string_eq(e->lval, e->rval);
  case SET_EQ_EXPRESSION:
    return code_set_eq(e->lval, e->rval); 
  case POINTER_EQ_EXPRESSION:
    return code_pointer_eq(e->lval, e->rval);
  case ORDINAL_NE_EXPRESSION:
    return code_ordinal_ne(e->lval, e->rval);
  case REAL_NE_EXPRESSION:
    return code_real_ne(e->lval, e->rval);
  case STRING_NE_EXPRESSION:
    return code_string_ne(e->lval, e->rval);
  case SET_NE_EXPRESSION:
    return code_set_ne(e->lval, e->rval); 
  case POINTER_NE_EXPRESSION:
    return code_pointer_ne(e->lval, e->rval);
  case FIELD_EXPRESSION:
    return code_field(e->field.base, e->field.field);
  case ORDINAL_LT_EXPRESSION:
    return code_ordinal_lt(e->lval, e->rval);
  case REAL_LT_EXPRESSION:
    return code_real_lt(e->lval, e->rval);
  case STRING_LT_EXPRESSION:
    return code_string_lt(e->lval, e->rval);
  case ORDINAL_LE_EXPRESSION:
    return code_ordinal_le(e->lval, e->rval);
  case REAL_LE_EXPRESSION:
    return code_real_le(e->lval, e->rval);
  case STRING_LE_EXPRESSION:
    return code_string_le(e->lval, e->rval);
  case SET_LE_EXPRESSION:
    return code_set_le(e->lval, e->rval);
  case ORDINAL_GT_EXPRESSION:
    return code_ordinal_gt(e->lval, e->rval);
  case REAL_GT_EXPRESSION:
    return code_real_gt(e->lval, e->rval);
  case STRING_GT_EXPRESSION:
    return code_string_gt(e->lval, e->rval);
  case ORDINAL_GE_EXPRESSION:
    return code_ordinal_ge(e->lval, e->rval);
  case REAL_GE_EXPRESSION:
    return code_real_ge(e->lval, e->rval);
  case STRING_GE_EXPRESSION:
    return code_string_ge(e->lval, e->rval);
  case SET_GE_EXPRESSION:
    return code_set_ge(e->lval, e->rval);
  case INTEGER_PLUS_EXPRESSION:
    return code_integer_plus(e->val);
  case INTEGER_MINUS_EXPRESSION:
    return code_integer_minus(e->val);
  case INTEGER_ADD_EXPRESSION:
    return code_integer_add(e->lval, e->rval);
  case INTEGER_SUBTRACT_EXPRESSION:
    return code_integer_subtract(e->lval, e->rval);
  case INTEGER_MULTIPLY_EXPRESSION:
    return code_integer_multiply(e->lval, e->rval);
  case INTEGER_DIVIDE_EXPRESSION:
    return code_integer_divide(e->lval, e->rval);
  case INTEGER_MODULUS_EXPRESSION:
    return code_integer_modulus(e->lval, e->rval);
  case NOT_EXPRESSION:
    return code_not(e->val);
  case OR_EXPRESSION:
    return code_or(e->lval, e->rval);
  case AND_EXPRESSION:
    return code_and(e->lval, e->rval);
  case INTEGER_ABS_EXPRESSION:
    return code_integer_abs(e->val);
  case INTEGER_SQR_EXPRESSION:
    return code_integer_sqr(e->val);
  case ODD_EXPRESSION:
    return code_odd(e->val);
  case ORD_EXPRESSION:
    return code_ord(e->val);
  case CHR_EXPRESSION:
    return code_chr(e->val);
  case SUCC_EXPRESSION:
    return code_succ(e->val);
  case PRED_EXPRESSION:
    return code_pred(e->val);
  case REAL_PLUS_EXPRESSION:
    return code_real_plus(e->val);
  case REAL_MINUS_EXPRESSION:
    return code_real_minus(e->val);
  case REAL_ADD_EXPRESSION:
    return code_real_add(e->lval, e->rval);
  case REAL_SUBTRACT_EXPRESSION:
    return code_real_subtract(e->lval, e->rval);
  case REAL_MULTIPLY_EXPRESSION:
    return code_real_multiply(e->lval, e->rval);
  case REAL_DIVIDE_EXPRESSION:
    return code_real_divide(e->lval, e->rval);
  case REAL_ABS_EXPRESSION:
    return code_real_abs(e->val);
  case REAL_SQR_EXPRESSION:
    return code_real_sqr(e->val);
  case SQRT_EXPRESSION:
    return code_sqrt(e->val);
  case SIN_EXPRESSION:
    return code_sin(e->val);
  case COS_EXPRESSION:
    return code_cos(e->val);
  case ARCTAN_EXPRESSION:
    return code_arctan(e->val);
  case EXP_EXPRESSION:
    return code_exp(e->val);
  case LN_EXPRESSION:
    return code_ln(e->val);
  case TRUNC_EXPRESSION:
    return code_trunc(e->val);
  case ROUND_EXPRESSION:
    return code_round(e->val);
  case INTEGER_TO_REAL_EXPRESSION:
    return code_integer_to_real(e->val);
  case INDEX_EXPRESSION:
    return code_index(e->lval, e->rval);
  case EMPTY_SET_EXPRESSION:
    return code_empty_set();
  case SET_CONSTRUCTOR:
    return code_set_constructor(e->set);
  case SET_EXPRESSION:
    return code_set(e->val);
  case SET_RANGE_EXPRESSION:
    return code_set_range(e->lval, e->rval);
  case SET_UNION_EXPRESSION:
    return code_set_union(e->lval, e->rval);
  case SET_INTERSECTION_EXPRESSION:
    return code_set_intersection(e->lval, e->rval);
  case SET_DIFFERENCE_EXPRESSION:
    return code_set_difference(e->lval, e->rval);
  case IN_EXPRESSION:
    return code_in(e->lval, e->rval);
  case EOF_EXPRESSION:
    return code_eof(e->val);
  case EOLN_EXPRESSION:
    return code_eoln(e->val);
  case ARGC_EXPRESSION:
    return code_argc();
  case FUNCTION_CALL:
    return code_function_call(e->call.sym, e->call.params);
  case PROCEDURAL_PARAMETER_EXPRESSION:
  case FUNCTIONAL_PARAMETER_EXPRESSION:
    break;
  }
  internal_error();
  return 0;
}

Code *code_integer_to_real(Expression *e) {
  return sequence2(code_load_value(e),
		   new_integer_to_real_op());
}

Code *code_ordinal_eq(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		  code_load_value(f),
		  new_ordinal_eq_op());
}

Code *code_real_eq(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_eq_op());
}

Code *code_string_eq(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_eq_op(e->type->size));
}

Code *code_set_eq(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_eq_op());
}

Code *code_pointer_eq(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_pointer_eq_op());
}

Code *code_ordinal_ne(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_ordinal_ne_op());
}

Code *code_real_ne(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_ne_op());
}

Code *code_string_ne(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_ne_op(e->type->size));
}

Code *code_set_ne(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_ne_op());
}

Code *code_pointer_ne(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_pointer_ne_op());
}

Code *code_ordinal_lt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_ordinal_lt_op());
}

Code *code_real_lt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_lt_op());
}

Code *code_string_lt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_lt_op(e->type->size));
}

Code *code_ordinal_le(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_ordinal_le_op());
}

Code *code_real_le(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_le_op());
}

Code *code_string_le(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_le_op(e->type->size));
}

Code *code_set_le(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_le_op());
}

Code *code_ordinal_gt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_ordinal_gt_op());
}

Code *code_real_gt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_gt_op());
}

Code *code_string_gt(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_gt_op(e->type->size));
}

Code *code_ordinal_ge(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_ordinal_ge_op());
}

Code *code_real_ge(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_ge_op());
}

Code *code_string_ge(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_string_ge_op(e->type->size));
}

Code *code_set_ge(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_ge_op());
}

Code *code_integer_plus(Expression *e) {
  return code_load_value(e);
}

Code *code_integer_minus(Expression *e) {
  return sequence2(code_load_value(e),
		   new_integer_negate_op());
}

Code *code_integer_add(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_integer_add_op());
}

Code *code_integer_subtract(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_integer_subtract_op());
}

Code *code_integer_multiply(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_integer_multiply_op());
}

Code *code_integer_divide(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_integer_divide_op());
}

Code *code_integer_modulus(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_integer_modulus_op());
}

Code *code_not(Expression *e) {
  return sequence2(code_load_value(e),
		   new_not_op());
}

Code *code_or(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_or_op());
}

Code *code_and(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_and_op());
}

Code *code_index(Expression *e, Expression *f) {
  return sequence2(code_load_index_address(e, f),
		   code_fetch(e->type->array.component_type));
}

Code *code_field_address(Expression *e, Symbol *field) {
  return sequence3(code_load_address(e),
		   new_load_ordinal_constant_op(field->variable.offset),
		   new_integer_add_op());
}

Code *code_field(Expression *e, Symbol *field) {
  return sequence2(code_field_address(e, field),
		   code_fetch(field->variable.type));
}

Code *code_empty_set() {
  return new_empty_set_op();
}

Code *code_set_constructor(Expression_List *exprs) {
  Code *code;
  
  code = new_empty_set_op();
  while (exprs) {
    code = sequence3(code,
		     code_load_value(exprs->expr),
		     new_set_union_op());
    exprs = exprs->next;
  }
  return code;
}

Code *code_set(Expression *e) {
  return sequence2(code_load_value(e),
		   new_to_set_op());
}

Code *code_set_range(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_to_set_range_op());
}

Code *code_set_union(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_union_op());
}

Code *code_set_intersection(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_intersection_op());
}

Code *code_set_difference(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_set_difference_op());
}

Code *code_in(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_in_op());
}

Code *code_integer_abs(Expression *e) {
  return sequence2(code_load_value(e),
		   new_integer_abs_op());
}

Code *code_integer_sqr(Expression *e) {
  return sequence2(code_load_value(e),
		   new_integer_sqr_op());
}

Code *code_odd(Expression *e) {
  return sequence2(code_load_value(e),
		   new_odd_op());
}

Code *code_ord(Expression *e) {
  return code_load_value(e);
}

Code *code_chr(Expression *e) {
  return code_load_value(e);
}

Code *code_succ(Expression *e) {
  return sequence2(code_load_value(e),
		   new_inc_op());
}

Code *code_pred(Expression *e) {
  return sequence2(code_load_value(e),
		   new_dec_op());
}

Code *code_eof(Expression *e) {
  return sequence2(code_load_value(e),
		   new_eof_op());
}

Code *code_eoln(Expression *e) {
  return sequence2(code_load_value(e),
		   new_eoln_op());
}

Code *code_argc() {
  return new_argc_op();
}

Code *code_real_plus(Expression *e) {
  return code_load_value(e);
}

Code *code_real_minus(Expression *e) {
  return sequence2(code_load_value(e),
		   new_real_negate_op());
}

Code *code_real_add(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_add_op());
}

Code *code_real_subtract(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_subtract_op());
}

Code *code_real_multiply(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_multiply_op());
}

Code *code_real_divide(Expression *e, Expression *f) {
  return sequence3(code_load_value(e),
		   code_load_value(f),
		   new_real_divide_op());
}

Code *code_real_abs(Expression *e) {
  return sequence2(code_load_value(e),
		   new_real_abs_op());
}

Code *code_real_sqr(Expression *e) {
  return sequence2(code_load_value(e),
		   new_real_sqr_op());
}

Code *code_sqrt(Expression *e) {
  return sequence2(code_load_value(e),
		   new_sqrt_op());
}

Code *code_sin(Expression *e) {
  return sequence2(code_load_value(e),
		   new_sin_op());
}

Code *code_cos(Expression *e) {
  return sequence2(code_load_value(e),
		   new_cos_op());
}

Code *code_arctan(Expression *e) {
  return sequence2(code_load_value(e),
		   new_arctan_op());
}

Code *code_exp(Expression *e) {
  return sequence2(code_load_value(e),
		   new_exp_op());
}

Code *code_ln(Expression *e) {
  return sequence2(code_load_value(e),
		   new_ln_op());
}

Code *code_trunc(Expression *e) {
  return sequence2(code_load_value(e),
		   new_trunc_op());
}

Code *code_round(Expression *e) {
  return sequence2(code_load_value(e),
		   new_round_op());
}

Code *code_load_address(Expression *e) {
  switch (e->class) {
  case VARIABLE_EXPRESSION:
    return code_load_variable_address(e->variable);
  case INDEX_EXPRESSION:
    return code_load_index_address(e->lval, e->rval);
  case FIELD_EXPRESSION:
    return code_field_address(e->field.base, e->field.field);
  case INDIRECT_EXPRESSION:
    return code_load_value(e->val);
  case FILE_ACCESS:
    return code_file_access_address(e);
  default:
    printf("class = %d\n", e->class);
    internal_error();
  }
  return 0;
}

Code *code_load_variable(Symbol *sym) {
  return sequence2(code_load_variable_address(sym),
		   code_fetch(sym->variable.type));
}

Code *code_load_variable_address(Symbol *sym) {
  switch (sym->class) {
  case VARIABLE_SYMBOL:
  case VALUE_PARAMETER:
  case PROCEDURE_PARAMETER:
  case FUNCTION_PARAMETER:
    return new_load_variable_address_op(current_algorithm->block_level,
					current_algorithm->block_level - sym->block_level,
					sym->variable.offset);
  case VARIABLE_PARAMETER:
    return sequence2(new_load_variable_address_op(current_algorithm->block_level,
						  current_algorithm->block_level - sym->block_level,
						  sym->variable.offset),
		     new_fetch_op());
  default:
    printf("class = %d\n", sym->class);
    internal_error();
  }
  return 0;
}

Code *code_load_index_address(Expression *e, Expression *f) {
  return sequence7(code_load_value(f),
		   new_load_ordinal_constant_op(e->type->array.index_type->ordinal.low),
		   new_integer_subtract_op(),
		   new_load_ordinal_constant_op(e->type->array.component_type->size),
		   new_integer_multiply_op(),
		   code_load_address(e),
		   new_integer_add_op());
}

Code *code_fetch(Type *t) {
  switch (t->class) {
  case ORDINAL_TYPE:
    if (t->ordinal.low < 0)
      return new_fetch_signed_op(t->size);
    return new_fetch_unsigned_op(t->size);
  case REAL_TYPE:
    return new_fetch_real_op();
  case SET_TYPE:
    return new_fetch_set_op();
  case NIL_TYPE:
  case FILE_TYPE:
  case POINTER_TYPE:
    return new_fetch_op();
   
  default:
    /* leave pointer on stack, should set be here too?  */
    ;
  }
  return 0;
}

Code *code_load_constant(Constant *cnst) {
  switch (cnst->type->class) {
  case ORDINAL_TYPE:
    return new_load_ordinal_constant_op(cnst->ordinal);
  case REAL_TYPE:
    return new_load_real_constant_op(cnst->real);
  case ARRAY_TYPE: /* must be a string */
    return new_load_string_constant_op(cnst->string);
  case POINTER_TYPE: /* nil */
    return new_load_nil();
  default:
    internal_error();
  }
  return 0;
}

Code *code_indirect(Expression *e) {
  return sequence2(code_load_value(e),
		   code_fetch(e->type));
}

Code *code_file_access_address(Expression *e) {
  return sequence2(code_load_value(e),
		   new_file_access_op());
}


Code *code_file_access(Expression *e) {
  return sequence2(code_file_access_address(e),
		   code_fetch(e->type));
}

Code *code_store(Type *t) {
  switch (t->class) {
  case ORDINAL_TYPE:
  case NIL_TYPE:
  case FILE_TYPE:
  case POINTER_TYPE:
    return new_store_op(t->size);
  case REAL_TYPE:
    return new_store_real_op();
  case SET_TYPE:
    return new_store_set_op();
  default:
    /* code fetch left a pointer on the stack */
    return new_move_op(t->size);
  }
}

Code *code_function_call(Symbol *sym, Expression_List *params) {
  Code *code = code_parameters(sym->algorithm.parameters, params);
  if (sym->class == FUNCTION_SYMBOL)
    return sequence2(code,
		     new_function_call_op(sym->name));
  if (sym->class == FUNCTION_PARAMETER)
    return sequence4(code,
		     code_load_variable_address(sym),
		     new_fetch_op(),
		     new_function_call_indirect_op());
  internal_error();
  return 0;
}

Code *code_procedure_call(Symbol *sym, Expression_List *params) {
  Code *code = code_parameters(sym->algorithm.parameters, params);
  if (sym->class == PROCEDURE_SYMBOL)
    return sequence2(code,
		     new_procedure_call_op(sym->name));
  if (sym->class == PROCEDURE_PARAMETER)
    return sequence4(code,
		     code_load_variable_address(sym),
		     new_fetch_op(),
		     new_procedure_call_indirect_op());
  internal_error();
  return 0;
}

Code *code_parameters(Symbol_List *defs, Expression_List *exprs) {
  Code *code;
  
  if (defs == 0 && exprs == 0)
    return 0;
  if (defs == 0 || exprs == 0) {
    internal_error();
    return 0;
  }
  switch (defs->symbol->class) {
  case VALUE_PARAMETER:
    code = code_value_parameter(exprs->expr);
    break;
  case VARIABLE_PARAMETER:
    code = code_variable_parameter(exprs->expr);
    break;
  case PROCEDURE_PARAMETER:
    code = code_procedural_parameter(exprs->expr->variable);
    break;
  case FUNCTION_PARAMETER:
    code = code_functional_parameter(exprs->expr->variable);
    break;
  default:
    internal_error();
    code = 0;
  }
  return sequence2(code_parameters(defs->next, exprs->next), code);
}

Code *code_value_parameter(Expression *e) {
  return code_load_value(e);
}

Code *code_variable_parameter(Expression *e) {
  return code_load_address(e);
}

Code *code_procedural_parameter(Symbol *proc) {
  if (proc->class == PROCEDURE_SYMBOL)
    return new_load_algorithm_op(proc->name);
  if (proc->class == FUNCTION_PARAMETER)
    return sequence2(code_load_variable_address(proc),
		     new_fetch_op());
  internal_error();
  return 0;
}

Code *code_functional_parameter(Symbol *func) {
  if (func->class == FUNCTION_SYMBOL)
    return new_load_algorithm_op(func->name);
  if (func->class == FUNCTION_PARAMETER)
    return sequence2(code_load_variable_address(func),
		     new_fetch_op());
  internal_error();
  return 0;
}

Code *new_code(Operation op) {
  Code *code = new(Code);
  code->op = op;
  code->next = code->prev = code;
  return code;
}

Code *new_statement_label(char *label) {
  Code *code = new_code(LABEL_OP);
  code->name = label;
  return code;
}

Code *new_internal_label(int label) {
  Code *code = new_code(INTERNAL_LABEL);
  code->ilabel = label;
  return code;
}

Code *new_nop_op() {
  return new_code(NOP_OP);
}

Code *new_enter_op(long size, int level) {
  Code *code = new_code(ENTER_OP);
  code->enter.size = size;
  code->enter.level = level;
  return code;
}

Code *new_leave_op() {
  Code *code = new_code(LEAVE_OP);
  return code;
}

Code *new_return_op(long size) {
  Code *code = new_code(RETURN_OP);
  code->value = size;
  return code;
}

Code *new_procedure_call_op(char *name) {
  Code *code = new_code(PROCEDURE_CALL_OP);
  code->name = name;
  return code;
}

Code *new_procedure_call_indirect_op() {
  return new_code(PROCEDURE_CALL_INDIRECT_OP);
}

Code *new_function_call_op(char *name) {
  Code *code = new_code(FUNCTION_CALL_OP);
  code->name = name;
  return code;
}

Code *new_function_call_indirect_op() {
  return new_code(FUNCTION_CALL_INDIRECT_OP);
}

Code *new_load_ordinal_constant_op(long cnst) {
  Code *code = new_code(LOAD_ORDINAL_CONSTANT_OP);
  code->value = cnst;
  return code;
}

Code *new_load_real_constant_op(double x) {
  Code *code = new_code(LOAD_REAL_CONSTANT_OP);
  code->rval = x;
  return code;
}

Code *new_load_string_constant_op(char *str) {
  Code *code = new_code(LOAD_STRING_CONSTANT_OP);
  code->strval = str;
  return code;
}

Code *new_load_nil() {
  return new_code(LOAD_NIL_OP);
}

Code *new_load_variable_address_op(long level, long delta, long offset) {
  Code *code = new_code(LOAD_VARIABLE_ADDRESS_OP);
  code->var.level = level;
  code->var.delta = delta;
  code->var.offset = offset;
  return code;
}

Code *new_fetch_signed_op(int size) {
  Code *code = new_code(FETCH_SIGNED_OP);
  code->value = size;
  return code;
}

Code *new_fetch_unsigned_op(int size) {
  Code *code = new_code(FETCH_UNSIGNED_OP);
  code->value = size;
  return code;
}

Code *new_fetch_real_op() {
  return new_code(FETCH_REAL_OP);
}

Code *new_fetch_set_op() {
  return new_code(FETCH_SET_OP);
}

Code *new_fetch_op() {
  return new_code(FETCH_OP);
}

Code *new_file_access_op() {
  return new_code(FILE_ACCESS_OP);
}

Code *new_load_algorithm_op(char *name) {
  Code *code = new_code(LOAD_ALGORITHM_OP);
  code->name = name;
  return code;
}

Code *new_return_signed_op(int size) {
  Code *code = new_code(RETURN_SIGNED_OP);
  code->value = size;
  return code;
}

Code *new_return_unsigned_op(int size) {
  Code *code = new_code(RETURN_UNSIGNED_OP);
  code->value = size;
  return code;
}

Code *new_return_real_op() {
  return new_code(RETURN_REAL_OP);
}

Code *new_return_pointer_op() {
  return new_code(RETURN_POINTER_OP);
}

Code *new_store_op(long size) {
  Code *code = new_code(STORE_OP);
  code->value = size;
  return code;
}

Code *new_store_real_op() {
  return new_code(STORE_REAL_OP);
}

Code *new_store_set_op() {
  return new_code(STORE_SET_OP);
}

Code *new_move_op(long size) {
  Code *code = new_code(MOVE_OP);
  code->value = size;
  return code;
}

Code *new_integer_to_real_op() {
  return new_code(INTEGER_TO_REAL_OP);
}

Code *new_ordinal_eq_op() {
  return new_code(ORDINAL_EQ_OP);
}

Code *new_real_eq_op() {
  return new_code(REAL_EQ_OP);
}

Code *new_string_eq_op(long size) {
  Code *code = new_code(STRING_EQ_OP);
  code->value = size;
  return code;
}

Code *new_pointer_eq_op() {
  return new_code(POINTER_EQ_OP);
}

Code *new_set_eq_op() {
  return new_code(SET_EQ_OP);
}

Code *new_ordinal_ne_op() {
  return new_code(ORDINAL_NE_OP);
}

Code *new_real_ne_op() {
  return new_code(REAL_NE_OP);
}

Code *new_string_ne_op(long size) {
  Code *code = new_code(STRING_NE_OP);
  code->value = size;
  return code;
}

Code *new_pointer_ne_op() {
  return new_code(POINTER_NE_OP);
}

Code *new_set_ne_op() {
  return new_code(SET_NE_OP);
}

Code *new_ordinal_lt_op() {
  return new_code(ORDINAL_LT_OP);
}

Code *new_real_lt_op() {
  return new_code(REAL_LT_OP);
}

Code *new_string_lt_op(long size) {
  Code *code = new_code(STRING_LT_OP);
  code->value = size;
  return code;
}

Code *new_ordinal_le_op() {
  return new_code(ORDINAL_LE_OP);
}

Code *new_real_le_op() {
  return new_code(REAL_LE_OP);
}

Code *new_string_le_op(long size) {
  Code *code = new_code(STRING_LE_OP);
  code->value = size;
  return code;
} 

Code *new_set_le_op() {
  return new_code(SET_LE_OP);
}

Code *new_ordinal_gt_op() {
  return new_code(ORDINAL_GT_OP);
}

Code *new_real_gt_op() {
  return new_code(REAL_GT_OP);
}

Code *new_string_gt_op(long size) {
  Code *code = new_code(STRING_GT_OP);
  code->value = size;
  return code;
}

Code *new_ordinal_ge_op() {
  return new_code(ORDINAL_GE_OP);
}

Code *new_real_ge_op() {
  return new_code(REAL_GE_OP);
}

Code *new_string_ge_op(long size) {
  Code *code = new_code(STRING_GE_OP);
  code->value = size;
  return code;
}

Code *new_set_ge_op() {
  return new_code(SET_GE_OP);
}

Code *new_integer_negate_op() {
  return new_code(INTEGER_NEGATE_OP);
}

Code *new_integer_add_op() {
  return new_code(INTEGER_ADD_OP);
}

Code *new_integer_subtract_op() {
  return new_code(INTEGER_SUBTRACT_OP);
}

Code *new_integer_multiply_op() {
  return new_code(INTEGER_MULTIPLY_OP);
}

Code *new_integer_divide_op() {
  return new_code(INTEGER_DIVIDE_OP);
}

Code *new_integer_modulus_op() {
  return new_code(INTEGER_MODULUS_OP);
}

Code *new_real_negate_op() {
  return new_code(REAL_NEGATE_OP);
}

Code *new_real_add_op() {
  return new_code(REAL_ADD_OP);
}

Code *new_real_subtract_op() {
  return new_code(REAL_SUBTRACT_OP);
}

Code *new_real_multiply_op() {
  return new_code(REAL_MULTIPLY_OP);
}

Code *new_real_divide_op() {
  return new_code(REAL_DIVIDE_OP);
}

Code *new_not_op() {
  return new_code(NOT_OP);
}

Code *new_or_op() {
  return new_code(OR_OP);
}

Code *new_and_op() {
  return new_code(AND_OP);
}

Code *new_integer_abs_op() {
  return new_code(INTEGER_ABS_OP);
}

Code *new_real_abs_op() {
  return new_code(REAL_ABS_OP);
}

Code *new_integer_sqr_op() {
  return new_code(INTEGER_SQR_OP);
}

Code *new_real_sqr_op() {
  return new_code(REAL_SQR_OP);
}

Code *new_sqrt_op() {
  return new_code(SQRT_OP);
}

Code *new_sin_op() {
  return new_code(SIN_OP);
}

Code *new_cos_op() {
  return new_code(COS_OP);
}

Code *new_arctan_op() {
  return new_code(ARCTAN_OP);
}

Code *new_exp_op() {
  return new_code(EXP_OP);
}

Code *new_ln_op() {
  return new_code(LN_OP);
}

Code *new_trunc_op() {
  return new_code(TRUNC_OP);
}

Code *new_round_op() {
  return new_code(ROUND_OP);
}

Code *new_odd_op() {
  return new_code(ODD_OP);
}

Code *new_inc_op() {
  return new_code(INC_OP);
}

Code *new_dec_op() {
  return new_code(DEC_OP);
}

Code *new_eof_op() {
  return new_code(EOF_OP);
}

Code *new_eoln_op() {
  return new_code(EOLN_OP);
}

Code *new_argc_op() {
  return new_code(ARGC_OP);
}

Code *new_empty_set_op() {
  return new_code(EMPTY_SET_OP);
}

Code *new_to_set_op() {
  return new_code(TO_SET_OP);
}

Code *new_to_set_range_op() {
  return new_code(TO_SET_RANGE_OP);
}

Code *new_set_union_op() {
  return new_code(SET_UNION_OP);
}

Code *new_set_intersection_op() {
  return new_code(SET_INTERSECTION_OP);
}

Code *new_set_difference_op() {
  return new_code(SET_DIFFERENCE_OP);
}

Code *new_in_op() {
  return new_code(IN_OP);
}

Code *new_jump_op(int label) {
  Code *code = new_code(JUMP_OP);
  code->jump = label;
  return code;
}

Code *new_jump_true_op(int label) {
  Code *code = new_code(JUMP_TRUE_OP);
  code->jump = label;
  return code;
}

Code *new_jump_false_op(int label) {
  Code *code = new_code(JUMP_FALSE_OP);
  code->jump = label;
  return code;
}

Code *new_jump_label_op(char *lab) {
  Code *code = new_code(JUMP_LABEL_OP);
  code->name = lab;
  return code;
}

Code *new_jump_index(int label, long size, int other) {
  Code *code = new_code(JUMP_INDEX_OP);
  code->jump_index.label = label;
  code->jump_index.size = size;
  code->jump_index.otherwise = other;
  return code;
}

Code *new_jump_table(int *table, long size, int otherwise_label) {
  Code *code = new_code(JUMP_TABLE);
  code->jump_table.table = table;
  code->jump_table.size = size;
  code->jump_table.otherwise = otherwise_label;
  return code;
}

Code *new_case_error_op() {
  return new_code(CASE_ERROR_OP);
}

Code *new_interprocedural_jump_op(long offset, long locals, char *name) {
  Code *code = new_code(INTERPROCEDURAL_JUMP_OP);
  code->inter.offset = offset;
  code->inter.locals = locals;
  code->inter.name = name;
  return code;
}

Code *new_get_op() {
  return new_code(GET_OP);
}

Code *new_put_op() {
  return new_code(PUT_OP);
}

Code *new_reset_op() {
  return new_code(RESET_OP);
}

Code *new_named_reset_op() {
  return new_code(NAMED_RESET_OP);
}

Code *new_reset_text_op() {
  return new_code(RESET_TEXT_OP);
}

Code *new_named_reset_text_op() {
  return new_code(NAMED_RESET_TEXT_OP);
}

Code *new_rewrite_op() {
  return new_code(REWRITE_OP);
}

Code *new_named_rewrite_op() {
  return new_code(NAMED_REWRITE_OP);
}

Code *new_rewrite_text_op() {
  return new_code(REWRITE_TEXT_OP);
}

Code *new_named_rewrite_text_op() {
  return new_code(NAMED_REWRITE_TEXT_OP);
}

Code *new_read_char_op() {
  return new_code(READ_CHAR_OP);
}

Code *new_read_integer_op() {
  return new_code(READ_INTEGER_OP);
}

Code *new_read_real_op() {
  return new_code(READ_REAL_OP);
}

Code *new_readln_op() {
  return new_code(READLN_OP);
}

Code *new_write_direct_op() {
  return new_code(WRITE_DIRECT_OP);
}

Code *new_write_indirect_op() {
  return new_code(WRITE_INDIRECT_OP);
}

Code *new_write_set_op() {
  return new_code(WRITE_SET_OP);
}

Code *new_write_boolean_op() {
  return new_code(WRITE_BOOLEAN_OP);
}

Code *new_write_char_op() {
  return new_code(WRITE_CHAR_OP);
}

Code *new_write_integer_op() {
  return new_code(WRITE_INTEGER_OP);
}

Code *new_write_real_float_op() {
  return new_code(WRITE_REAL_FLOAT_OP);
}

Code *new_write_real_fixed_op() {
  return new_code(WRITE_REAL_FIXED_OP);
}

Code *new_write_string_op() {
  return new_code(WRITE_STRING_OP);
}

Code *new_writeln_op() {
  return new_code(WRITELN_OP);
}

Code *new_page_op() {
  return new_code(PAGE_OP);
}

Code *new_new_op() {
  return new_code(NEW_OP);
}

Code *new_dispose_op() {
  return new_code(DISPOSE_OP);
}

Code *new_argv_op() {
  return new_code(ARGV_OP);
}

Code *new_flush_op() {
  return new_code(FLUSH_OP);
}

Code *new_close_op() {
  return new_code(CLOSE_OP);
}

/* link two code lists */

Code *sequence2(Code *x, Code *y) {
  Code *xp, *yp;

  if (x == 0)
    return y;
  if (y == 0)
    return x;
    
  xp = x->prev;
  yp = y->prev;
  x->prev->next = y;
  x->prev = yp;
  y->prev->next = x;
  y->prev = xp;
  return x;
}

Code *sequence3(Code *x, Code *y, Code *z) {
  return sequence2(x, sequence2(y, z));
}

Code *sequence4(Code *a, Code *b, Code *c, Code *d) {
  return sequence2(sequence2(a, b),
		   sequence2(c, d));
}

Code *sequence5(Code *a, Code *b, Code *c, Code *d, Code *e) {
  return sequence3(sequence2(a, b),
		   sequence2(c, d),
		   e);
}

Code *sequence6(Code *a, Code *b, Code *c, Code *d, Code *e, Code *f) {
  return sequence3(sequence2(a, b),
		   sequence2(c, d),
		   sequence2(e, f));
}

Code *sequence7(Code *a, Code *b, Code *c, Code *d, Code *e, Code *f, Code *g) {
  return sequence2(sequence2(sequence2(a, b),
			     sequence2(c, d)),
		   sequence2(sequence2(e, f),
			     g));
}

Code *sequence9(Code *a, Code *b, Code *c, Code *d, Code *e, Code *f, Code *g, Code *h, Code *i) {
  return sequence3(sequence3(a, b, c),
		   sequence3(d, e, f),
		   sequence3(g, h, i));
}
