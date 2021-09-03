#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include "machine.h"
#include "pascal.h"
#include "code.h"
#include "gen.h"

int trunc_flag = 0;
int trunc_label1;
int trunc_label2;
int round_flag = 0;
int round_label;

typedef struct String_Node {
  char *str;
  int lab;
  struct String_Node *next;
} String_Node;

String_Node *strings = 0;

void gen_enter(long, int);
void gen_leave(void);
void gen_return(long);
void gen_statement_label(char *);
void gen_internal_label(int);
void gen_procedure_call(char *);
void gen_procedure_call_indirect(void);
void gen_function_call(char *);
void gen_function_call_indirect(void);
void gen_return_signed(int);
void gen_return_unsigned(int);
void gen_return_real(void);
void gen_return_pointer(void);
void gen_load_ordinal_constant(long);
void gen_load_real_constant(double);
void gen_load_string_constant(char *);
void gen_load_variable_address(int, int, long);
void gen_load_nil(void);
void gen_load_algorithm(char *);
void gen_fetch_signed(int);
void gen_fetch_unsigned(int);
void gen_fetch_real(void);
void gen_fetch_set(void);
void gen_fetch(void);
void gen_store(int);
void gen_store_real(void);
void gen_store_set(void);
void gen_move(long);
void gen_integer_to_real(void);
void gen_ordinal_eq(void);
void gen_real_eq(void);
void gen_string_eq(long);
void gen_pointer_eq(void);
void gen_set_eq(void);
void gen_ordinal_ne(void);
void gen_real_ne(void);
void gen_string_ne(long);
void gen_pointer_ne(void);
void gen_set_ne(void);
void gen_ordinal_lt(void);
void gen_real_lt(void);
void gen_string_lt(long);
void gen_ordinal_le(void);
void gen_real_le(void);
void gen_string_le(long);
void gen_set_le(void);
void gen_ordinal_gt(void);
void gen_real_gt(void);
void gen_string_gt(long);
void gen_ordinal_ge(void);
void gen_real_ge(void);
void gen_string_ge(long);
void gen_set_ge(void);
void gen_integer_negate(void);
void gen_integer_add(void);
void gen_integer_subtract(void);
void gen_integer_multiply(void);
void gen_integer_divide(void);
void gen_integer_modulus(void);
void gen_real_negate(void);
void gen_real_add(void);
void gen_real_subtract(void);
void gen_real_multiply(void);
void gen_real_divide(void);
void gen_not(void);
void gen_or(void);
void gen_and(void);
void gen_integer_abs(void);
void gen_real_abs(void);
void gen_integer_sqr(void);
void gen_real_sqr(void);
void gen_sqrt(void);
void gen_sin(void);
void gen_cos(void);
void gen_arctan(void);
void gen_exp(void);
void gen_ln(void);
void gen_trunc(void);
void gen_round(void);
void set_trunc_labels(void);
void set_round_label(void);
void gen_odd(void);
void gen_inc(void);
void gen_dec(void);
void gen_empty_set(void);
void gen_to_set(void);
void gen_to_set_range(void);
void gen_set_union(void);
void gen_set_intersection(void);
void gen_set_difference(void);
void gen_in(void);
void gen_eof(void);
void gen_eoln(void);
void gen_jump(int);
void gen_jump_true(int);
void gen_jump_false(int);
void gen_jump_label(char *);
void gen_jump_index(int, long);
void gen_jump_table(int *, long);
void gen_interprocedural_jump(long, long, char *);
void gen_get(void);
void gen_put(void);
void gen_reset(void);
void gen_named_reset(void);
void gen_reset_text(void);
void gen_named_reset_text(void);
void gen_rewrite(void);
void gen_named_rewrite(void);
void gen_rewrite_text(void);
void gen_named_rewrite_text(void);
void gen_file_access(void);
void gen_read_char(void);
void gen_read_integer(void);
void gen_read_real(void);
void gen_readln(void);
void gen_write_direct(void);
void gen_write_indirect(void);
void gen_write_set(void);
void gen_write_boolean(void);
void gen_write_char(void);
void gen_write_integer(void);
void gen_write_real_float(void);
void gen_write_real_fixed(void);
void gen_write_string(void);
void gen_writeln(void);
void gen_page(void);
void gen_new(void);
void gen_dispose(void);
void gen_argc(void);
void gen_argv(void);
void gen_flush(void);
void gen_close(void);

void add_string(char *, int);
void output_string(char *);

void gen_code(Code *code) {
  Code *start = code;
  if (code) {
    do {
      switch (code->op) {
      case NOP_OP:
	break;
      case LABEL_OP:
	gen_statement_label(code->name);
	break;
      case INTERNAL_LABEL:
	gen_internal_label(code->ilabel);
	break;
      case ENTER_OP:
	gen_enter(code->enter.size, code->enter.level);
	break;
      case LEAVE_OP:
	gen_leave();
	break;
      case RETURN_OP:
	gen_return(code->value);
	break;
      case PROCEDURE_CALL_OP:
	gen_procedure_call(code->name);
	break;
      case PROCEDURE_CALL_INDIRECT_OP:
	gen_procedure_call_indirect();
	break;
      case FUNCTION_CALL_OP:
	gen_function_call(code->name);
	break;
      case FUNCTION_CALL_INDIRECT_OP:
	gen_function_call_indirect();
	break;
      case LOAD_ORDINAL_CONSTANT_OP:
	gen_load_ordinal_constant(code->value);
	break;
      case LOAD_REAL_CONSTANT_OP:
	gen_load_real_constant(code->rval);
	break;
      case LOAD_STRING_CONSTANT_OP:
	gen_load_string_constant(code->strval);
	break;
      case LOAD_VARIABLE_ADDRESS_OP:
	gen_load_variable_address(code->var.level, code->var.delta, code->var.offset);
	break;
      case LOAD_NIL_OP:
	gen_load_nil();
	break;
      case LOAD_ALGORITHM_OP:
	gen_load_algorithm(code->name);
	break;
      case FETCH_SIGNED_OP:
	gen_fetch_signed(code->value);
	break;
      case FETCH_UNSIGNED_OP:
	gen_fetch_unsigned(code->value);
	break;
      case FETCH_REAL_OP:
	gen_fetch_real();
	break;
      case FETCH_SET_OP:
	gen_fetch_set();
	break;
      case FETCH_OP:
	gen_fetch();
	break;
      case RETURN_SIGNED_OP:
	gen_return_signed(code->value);
	break;
      case RETURN_UNSIGNED_OP:
	gen_return_unsigned(code->value);
	break;
      case RETURN_REAL_OP:
	gen_return_real();
	break;
      case RETURN_POINTER_OP:
	gen_return_pointer();
	break;
      case STORE_OP:
	gen_store(code->value);
	break;
      case STORE_REAL_OP:
	gen_store_real();
	break;
      case STORE_SET_OP:
	gen_store_set();
	break;
      case MOVE_OP:
	gen_move(code->value);
	break;
      case INTEGER_TO_REAL_OP:
	gen_integer_to_real();
	break;
      case ORDINAL_EQ_OP:
	gen_ordinal_eq();
	break;
      case REAL_EQ_OP:
	gen_real_eq();
	break;
      case STRING_EQ_OP:
	gen_string_eq(code->value);
	break;
      case POINTER_EQ_OP:
	gen_pointer_eq();
	break;
      case SET_EQ_OP:
	gen_set_eq();
	break;
      case ORDINAL_NE_OP:
	gen_ordinal_ne();
	break;
      case REAL_NE_OP:
	gen_real_ne();
	break;
      case STRING_NE_OP:
	gen_string_ne(code->value);
	break;
      case POINTER_NE_OP:
	gen_pointer_ne();
	break;
      case SET_NE_OP:
	gen_set_ne();
	break;
      case ORDINAL_LT_OP:
	gen_ordinal_lt();
	break;
      case REAL_LT_OP:
	gen_real_lt();
	break;
      case STRING_LT_OP:
	gen_string_lt(code->value);
	break;
      case ORDINAL_LE_OP:
	gen_ordinal_le();
	break;
      case REAL_LE_OP:
	gen_real_le();
	break;
      case STRING_LE_OP:
	gen_string_le(code->value);
	break;
      case SET_LE_OP:
	gen_set_le();
	break;
      case ORDINAL_GT_OP:
	gen_ordinal_gt();
	break;
      case REAL_GT_OP:
	gen_real_gt();
	break;
      case STRING_GT_OP:
	gen_string_gt(code->value);
	break;
      case ORDINAL_GE_OP:
	gen_ordinal_ge();
	break;
      case REAL_GE_OP:
	gen_real_ge();
	break;
      case STRING_GE_OP:
	gen_string_ge(code->value);
	break;
      case SET_GE_OP:
	gen_set_ge();
	break;
      case INTEGER_NEGATE_OP:
	gen_integer_negate();
	break;
      case INTEGER_ADD_OP:
	gen_integer_add();
	break;
      case INTEGER_SUBTRACT_OP:
	gen_integer_subtract();
	break;
      case INTEGER_MULTIPLY_OP:
	gen_integer_multiply();
	break;
      case INTEGER_DIVIDE_OP:
	gen_integer_divide();
	break;
      case INTEGER_MODULUS_OP:
	gen_integer_modulus();
	break;
      case REAL_NEGATE_OP:
	gen_real_negate();
	break;
      case REAL_ADD_OP:
	gen_real_add();
	break;
      case REAL_SUBTRACT_OP:
	gen_real_subtract();
	break;
      case REAL_MULTIPLY_OP:
	gen_real_multiply();
	break;
      case REAL_DIVIDE_OP:
	gen_real_divide();
	break;
      case NOT_OP:
	gen_not();
	break;
      case OR_OP:
	gen_or();
	break;
      case AND_OP:
	gen_and();
	break;
      case INTEGER_ABS_OP:
	gen_integer_abs();
	break;
      case REAL_ABS_OP:
	gen_real_abs();
	break;
      case INTEGER_SQR_OP:
	gen_integer_sqr();
	break;
      case REAL_SQR_OP:
	gen_real_sqr();
	break;
      case SQRT_OP:
	gen_sqrt();
	break;
      case SIN_OP:
	gen_sin();
	break;
      case COS_OP:
	gen_cos();
	break;
      case ARCTAN_OP:
	gen_arctan();
	break;
      case EXP_OP:
	gen_exp();
	break;
      case LN_OP:
	gen_ln();
	break;
      case TRUNC_OP:
	gen_trunc();
	break;
      case ROUND_OP:
	gen_round();
	break;
      case ODD_OP:
	gen_odd();
	break;
      case INC_OP:
	gen_inc();
	break;
      case DEC_OP:
	gen_dec();
	break;
      case EMPTY_SET_OP:
	gen_empty_set();
	break;
      case TO_SET_OP:
	gen_to_set();
	break;
      case TO_SET_RANGE_OP:
	gen_to_set_range();
	break;
      case SET_UNION_OP:
	gen_set_union();
	break;
      case SET_INTERSECTION_OP:
	gen_set_intersection();
	break;
      case SET_DIFFERENCE_OP:
	gen_set_difference();
	break;
      case IN_OP:
	gen_in();
	break;
      case EOF_OP:
	gen_eof();
	break;
      case EOLN_OP:
	gen_eoln();
	break;
      case JUMP_OP:
	gen_jump(code->jump);
	break;
      case JUMP_TRUE_OP:
	gen_jump_true(code->jump);
	break;
      case JUMP_FALSE_OP:
	gen_jump_false(code->jump);
	break;
      case JUMP_LABEL_OP:
	gen_jump_label(code->name);
	break;
      case JUMP_INDEX_OP:
	gen_jump_index(code->jump_index.label, code->jump_index.size);
	break;
      case JUMP_TABLE:
	gen_jump_table(code->jump_table.table, code->jump_table.size);
	break;
      case INTERPROCEDURAL_JUMP_OP:
	gen_interprocedural_jump(code->inter.offset, code->inter.locals, code->inter.name);
	break;
      case GET_OP:
	gen_get();
	break;
      case PUT_OP:
	gen_put();
	break;
      case RESET_OP:
	gen_reset();
	break;
      case NAMED_RESET_OP:
	gen_named_reset();
	break;
      case RESET_TEXT_OP:
	gen_reset_text();
	break;
      case NAMED_RESET_TEXT_OP:
	gen_named_reset_text();
	break;
      case REWRITE_OP:
	gen_rewrite();
	break;
      case NAMED_REWRITE_OP:
	gen_named_rewrite();
	break;
      case REWRITE_TEXT_OP:
	gen_rewrite_text();
	break;
      case NAMED_REWRITE_TEXT_OP:
	gen_named_rewrite_text();
	break;
      case FILE_ACCESS_OP:
	gen_file_access();
	break;
      case READ_CHAR_OP:
	gen_read_char();
	break;
      case READ_INTEGER_OP:
	gen_read_integer();
	break;
      case READ_REAL_OP:
	gen_read_real();
	break;
      case READLN_OP:
	gen_readln();
	break;
      case WRITE_DIRECT_OP:
	gen_write_direct();
	break;
      case WRITE_INDIRECT_OP:
	gen_write_indirect();
	break;
      case WRITE_SET_OP:
	gen_write_set();
	break;
      case WRITE_BOOLEAN_OP:
	gen_write_boolean();
	break;
      case WRITE_CHAR_OP:
	gen_write_char();
	break;
      case WRITE_INTEGER_OP:
	gen_write_integer();
	break;
      case WRITE_REAL_FLOAT_OP:
	gen_write_real_float();
	break;
      case WRITE_REAL_FIXED_OP:
	gen_write_real_fixed();
	break;
      case WRITE_STRING_OP:
	gen_write_string();
	break;
      case WRITELN_OP:
	gen_writeln();
	break;
      case PAGE_OP:
	gen_page();
	break;
      case NEW_OP:
	gen_new();
	break;
      case DISPOSE_OP:
	gen_dispose();
	break;
      case ARGC_OP:
	gen_argc();
	break;
      case ARGV_OP:
	gen_argv();
	break;
      case FLUSH_OP:
	gen_flush();
	break;
      case CLOSE_OP:
	gen_close();
	break;
      } 
      code = code->next;
    } while (code != start);
  }
}

void gen_comment(char *fmt, ...) {
  va_list args;

  fprintf(output, "# ");
  va_start(args, fmt);
  vfprintf(output, fmt, args);
  va_end(args);
  fprintf(output, "\n");
}

void gen_statement_label(char *label) {
  fprintf(output, ".LG%s:\n", label);
}

void gen_internal_label(int label) {
  fprintf(output, ".L%d:\n", label);
}

void gen_text_label(char *label) {
  fprintf(output, "%s:\n", label);
}

void gen_startup1() {
  fprintf(output, "\t.globl\t_start\n");
  fprintf(output, "_start:\n");
  fprintf(output, "\tmov\t(%%rsp),%%rax\n");
  fprintf(output, "\tmov\t%%rax,p_argc\n");
  fprintf(output, "\tlea\t8(%%rsp),%%rax\n");
  fprintf(output, "\tmov\t%%rax,p_argv\n");
}

void gen_startup2(char *name) {
  fprintf(output, "\tcall\t%s\n", name);
  fprintf(output, "\txorq\t%%rdi,%%rdi\n");
  fprintf(output, "\tcall\tp_exit\n");
}

void gen_push_program_parameter(char *name) {
  fprintf(output, "\tleaq\t%s(%%rip),%%rax\n", name);
  fprintf(output, "\tpushq\t%%rax\n");
}

void gen_enter(long size, int level) {
  if (size >= 65536)
    fatal_error("frame size too large");
  if (level >= 255)
    fatal_error("subroutines nested too deep");
  fprintf(output, "\tenter\t$%ld,$%d\n", size, level+1);
}
    
void gen_leave() {
  fprintf(output, "\tleave\n");
}

void gen_return(long size) {
  if (size == 0)
    fprintf(output, "\tret\n");
  else
    fprintf(output, "\tret\t$%ld\n", size);
}

void gen_procedure_call(char *name) {
  fprintf(output, "\tcall\t%s\n", name);
}

void gen_procedure_call_indirect() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tcall\t*(%%rax)\n");
}

void gen_function_call(char *name) {
  fprintf(output, "\tcall\t%s\n", name);
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_function_call_indirect() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tcall\t*(%%rax)\n");
}

void gen_load_ordinal_constant(long x) {
  fprintf(output, "\tmov\t$%ld,%%rax\n", x);
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_load_real_constant(double r) {
  fprintf(output, "\tmov\t$%ld,%%rax\n", *(long *)&r);
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_load_string_constant(char *x) {
  int lab = new_label();
  
  add_string(x, lab);
  fprintf(output, "\tlea\t.L%d(%%rip),%%rax\n", lab);
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_load_variable_address(int level, int delta, long offset) {
  if (delta == 0) {  /* local */
    if (offset < 0) { /* variable */
      fprintf(output, "\tlea\t%ld(%%rbp),%%rax\n", offset - 8*level - 8);
    }
    else {  /* parameter */
      fprintf(output, "\tlea\t%ld(%%rbp),%%rax\n", offset + 16);
    }
  }
  else { /* non-local */
    fprintf(output, "\tmov\t%d(%%rbp),%%rax\n", 8*(delta - level - 1));
    if (offset < 0) { /* variable */
      fprintf(output, "\tlea\t%ld(%%rax),%%rax\n", offset - 8*(level - delta) - 8);
    }
    else {
      fprintf(output, "\tlea\t%ld(%%rax),%%rax\n", offset + 16);
    }
  }
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_load_nil() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_load_algorithm(char *name) {
  fprintf(output, "\tlea\t%s(%%rip),%%rax\n", name);
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_fetch() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tmov\t(%%rax),%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

/* size 1, 2, 4, 8 */

void gen_fetch_signed(int size) {
  fprintf(output, "\tpop\t%%rax\n");
  switch (size) {
  case 1:
    fprintf(output, "\tmovsbq\t(%%rax),%%rax\n");
    break;
  case 2:
    fprintf(output, "\tmovswq\t(%%rax),%%rax\n");
    break;
  case 4:
    fprintf(output, "\tmovslq\t(%%rax),%%rax\n");
    break;
  case 8:
    fprintf(output, "\tmov\t(%%rax),%%rax\n");
    break;
  default:
    internal_error();
  }
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_fetch_unsigned(int size) {
  fprintf(output, "\tpop\t%%rdx\n");
  switch (size) {
  case 1:
    fprintf(output, "\txor\t%%rax,%%rax\n");
    fprintf(output, "\tmov\t(%%rdx),%%al\n");
    break;
  case 2:
    fprintf(output, "\txor\t%%rax,%%rax\n");
    fprintf(output, "\tmov\t(%%rdx),%%ax\n");
    break;
  case 4:
    fprintf(output, "\txor\t%%rax,%%rax\n");
    fprintf(output, "\tmov\t(%%rdx),%%eax\n");
    break;
  case 8:
    fprintf(output, "\tmov\t(%%rdx),%%rax\n");
    break;
  default:
    internal_error();
  }
  fprintf(output, "\tpush\t%%rax\n");
  
}

void gen_fetch_real() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpush\t(%%rdx)\n");
}

void gen_fetch_set() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpush\t24(%%rdx)\n");
  fprintf(output, "\tpush\t16(%%rdx)\n");
  fprintf(output, "\tpush\t8(%%rdx)\n");
  fprintf(output, "\tpush\t(%%rdx)\n");
}

void gen_return_signed(int size) {
  fprintf(output, "\tpop\t%%rax\n");
  switch (size) {
  case 1:
    fprintf(output, "\tmovsbq\t(%%rax),%%rax\n");
    break;
  case 2:
    fprintf(output, "\tmovswq\t(%%rax),%%rax\n");
    break;
  case 4:
    fprintf(output, "\tmovslq\t(%%rax),%%rax\n");
    break;
  case 8:
    fprintf(output, "\tmov\t(%%rax),%%rax\n");
    break;
  default:
    internal_error();
  }
}

void gen_return_unsigned(int size) {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  switch (size) {
  case 1:
    fprintf(output, "\tmov\t(%%rdx),%%al\n");
    break;
  case 2:
    fprintf(output, "\tmov\t(%%rdx),%%ax\n");
    break;
  case 4:
    fprintf(output, "\tmov\t(%%rdx),%%eax\n");
    break;
  case 8:
    fprintf(output, "\tmov\t(%%rdx),%%rax\n");
    break;
  default:
    internal_error();
  }
}

void gen_return_real() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tmov\t(%%rax),%%rax\n");
}

void gen_return_pointer() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tmov\t(%%rax),%%rax\n");
}

/* only works for ordinal values? */

void gen_store(int size) {
  fprintf(output, "\tpop\t%%rdx\n");  /* %rdx -- destation address */
  fprintf(output, "\tpop\t%%rax\n");  /* %rax -- source register */
  switch (size) {
  case 1:
    fprintf(output, "\tmov\t%%al,(%%rdx)\n");
    break;
  case 2:
    fprintf(output, "\tmov\t%%ax,(%%rdx)\n");
    break;
  case 4:
    fprintf(output, "\tmov\t%%eax,(%%rdx)\n");
    break;
  case 8:
    fprintf(output, "\tmov\t%%rax,(%%rdx)\n");
    break;
  default:
    internal_error();
  }
}

void gen_store_real() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tmov\t%%rax,(%%rdx)\n");
}

void gen_store_set() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tpopq\t(%%rax)\n");
  fprintf(output, "\tpopq\t8(%%rax)\n");
  fprintf(output, "\tpopq\t16(%%rax)\n");
  fprintf(output, "\tpopq\t24(%%rax)\n");
}

void gen_move(long size) {
  fprintf(output, "\tpop\t%%rdi\n");
  fprintf(output, "\tpop\t%%rsi\n");
  fprintf(output, "\tmov\t$%ld,%%rcx\n", size);
  fprintf(output, "\trep movsb\n");
}

void gen_integer_to_real() {
  fprintf(output, "\tcvtsi2sdq\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,(%%rsp)\n");
}

void gen_get() {
  fprintf(output, "\tcall\tget_x\n");
}

void gen_put() {
  fprintf(output, "\tcall\tput_x\n");
}

void gen_reset() {
  fprintf(output, "\tcall\treset_x\n");
}

void gen_named_reset() {
  fprintf(output, "\tcall\tnamed_reset_x\n");
}

void gen_reset_text() {
  fprintf(output, "\tcall\treset_text_x\n");
}

void gen_named_reset_text() {
  fprintf(output, "\tcall\tnamed_reset_text_x\n");
}

void gen_rewrite() {
  fprintf(output, "\tcall\trewrite_x\n");
}

void gen_named_rewrite() {
  fprintf(output, "\tcall\tnamed_rewrite_x\n");
}

void gen_rewrite_text() {
  fprintf(output, "\tcall\trewrite_text_x\n");
}

void gen_named_rewrite_text() {
  fprintf(output, "\tcall\tnamed_rewrite_text_x\n");
}

/* return turn a pointer to file buffer */

void gen_file_access() {
  fprintf(output, "\tcall\tfile_access_x\n");
}

void gen_read_char() {
  fprintf(output, "\tcall\tread_char_x\n");
}

void gen_read_integer() {
  fprintf(output, "\tcall\tread_integer_x\n");
}

void gen_read_real() {
  fprintf(output, "\tcall\tread_real_x\n");
}

void gen_readln() {
  fprintf(output, "\tcall\treadln_x\n");
}

void gen_write_direct() {
  fprintf(output, "\tcall\twrite_direct_x\n");
}

void gen_write_indirect() {
  fprintf(output, "\tcall\twrite_indirect_x\n");
}

void gen_write_set() {
  fprintf(output, "\tcall\twrite_set_x\n");
}

void gen_write_boolean() {
  fprintf(output, "\tcall\twrite_boolean_x\n");
}

void gen_write_char() {
  fprintf(output, "\tcall\twrite_char_x\n");
}

void gen_write_integer() {
  fprintf(output, "\tcall\twrite_integer_x\n");
}

void gen_write_real_float() {
  fprintf(output, "\tcall\twrite_real_float_x\n");
}

void gen_write_real_fixed() {
  fprintf(output, "\tcall\twrite_real_fixed_x\n");
}

void gen_write_string() {
  fprintf(output, "\tcall\twrite_string_x\n");
}

void gen_writeln() {
  fprintf(output, "\tcall\twriteln_x\n");
}

void gen_page() {
  fprintf(output, "\tcall\tpage_x\n");
}

void gen_new() {
  fprintf(output, "\tcall\tp_new_x\n");
}

void gen_dispose() {
  fprintf(output, "\tcall\tp_dispose_x\n");
}

void gen_ordinal_eq() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsete\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_eq() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsete\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_string_eq(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsete\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}

void gen_pointer_eq() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsete\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_set_eq() {
  int label = new_label();
  
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tadd\t$64,%%rsp\n");
  fprintf(output, "\tmov\t-32(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-64(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-24(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-56(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-16(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-48(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-8(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-40(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tinc\t%%rax\n");
  fprintf(output, ".L%d:\tpush  %%rax\n", label);
}
  

void gen_ordinal_ne() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetne\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_ne() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsetne\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}


void gen_string_ne(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsetne\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}

void gen_pointer_ne() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetne\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_set_ne() {
  int label = new_label();
  
  fprintf(output, "\tmov\t$1,%%rax\n");
  fprintf(output, "\tadd\t$64,%%rsp\n");
  fprintf(output, "\tmov\t-32(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-64(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-24(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-56(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-16(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-48(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\tmov\t-8(%%rsp),%%rdx\n");
  fprintf(output, "\tcmp\t-40(%%rsp),%%rdx\n");
  fprintf(output, "\tjne\t.L%d\n", label);
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, ".L%d:\tpush  %%rax\n", label);
}
  
void gen_ordinal_lt() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetl\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_lt() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tseta\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_string_lt(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsetl\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}

void gen_ordinal_le() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetle\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_le() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsetae\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_string_le(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsetle\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}

void gen_set_le() {
  int label = new_label();

  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tadd\t$64,%%rsp\n");
  fprintf(output, "\tmov\t-32(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-64(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-24(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-56(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-16(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-48(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-8(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-40(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tinc\t%%rax\n");
  fprintf(output, ".L%d:\tpush\t%%rax\n", label);
}

void gen_ordinal_gt() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetg\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_gt() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsetb\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_string_gt(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsetg\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}  

void gen_ordinal_ge() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tcmp\t%%rdx,(%%rsp)\n");
  fprintf(output, "\tsetge\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_ge() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tcomisd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsetbe\t%%al\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}  

void gen_string_ge(long size) {
  fprintf(output, "\tpushq\t$%ld\n", size);
  fprintf(output, "\tcall\tp_string_compare_x\n");
  fprintf(output, "\txor\t%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tsetge\t%%dl\n");
  fprintf(output, "\tpush\t%%rdx\n");
}  

void gen_set_ge() {
  int label = new_label();

  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tadd\t$64,%%rsp\n");
  fprintf(output, "\tmov\t-64(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-32(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-56(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-24(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-48(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-16(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tmov\t-40(%%rsp),%%rdx\n");
  fprintf(output, "\tandn\t-8(%%rsp),%%rdx,%%rdx\n");
  fprintf(output, "\ttest\t%%rdx,%%rdx\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
  fprintf(output, "\tinc\t%%rax\n");
  fprintf(output, ".L%d:\tpush\t%%rax\n", label);
}

void gen_integer_negate() {
  fprintf(output, "\tnegq\t(%%rsp)\n");
}

void gen_integer_add() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tadd\t%%rax,(%%rsp)\n");
}

void gen_integer_subtract() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tsub\t%%rdx,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_integer_multiply() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\timul\t%%rdx,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_integer_divide() {
  fprintf(output, "\tpop\t%%rcx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tcqto\n");
  fprintf(output, "\tidiv\t%%rcx\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_integer_modulus() {
  fprintf(output, "\tpop\t%%rcx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tcqto\n");
  fprintf(output, "\tidiv\t%%rcx\n");
  fprintf(output, "\tpush\t%%rdx\n");
}

void gen_real_negate() {
  fprintf(output, "\tmov\t$%ld,%%rax\n", 0x8000000000000000);
  fprintf(output, "\txor\t%%rax,(%%rsp)\n");
}

void gen_real_add() {
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\taddsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,(%%rsp)\n");
}

void gen_real_subtract() {
  fprintf(output, "\tmovsd\t8(%%rsp),%%xmm0\n");
  fprintf(output, "\tsubsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,8(%%rsp)\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
}

void gen_real_multiply() {
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
  fprintf(output, "\tmulsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,(%%rsp)\n");
}

void gen_real_divide() {
  fprintf(output, "\tmovsd\t8(%%rsp),%%xmm0\n");
  fprintf(output, "\tdivsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,8(%%rsp)\n");
  fprintf(output, "\tadd\t$8,%%rsp\n");
}

void gen_not() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\txor\t$1,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_or() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tor\t%%rdx,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_and() {
  fprintf(output, "\tpop\t%%rdx\n");
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tand\t%%rdx,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_integer_abs() {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tmov\t%%rax,%%rdx\n");
  fprintf(output, "\tneg\t%%rax\n");
  fprintf(output, "\tcmovs\t%%rdx,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_real_abs() {
  fprintf(output, "\tmov\t$%ld,%%rax\n", 0x7fffffffffffffff);
  fprintf(output, "\tand\t%%rax,(%%rsp)\n");
}

void gen_integer_sqr() {
  fprintf(output, "\tmov\t(%%rsp),%%rax\n");
  fprintf(output, "\timul\t%%rax,%%rax\n");
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_real_sqr() {
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmulsd\t%%xmm0,%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,(%%rsp)\n");
}

void gen_sqrt() {
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tsqrtsd\t%%xmm0,%%xmm0\n");
  fprintf(output, "\tmovsd\t%%xmm0,(%%rsp)\n");
}

void gen_sin() {
  fprintf(output, "\tcall\tsin_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_cos() {
  fprintf(output, "\tcall\tcos_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_arctan() {
  fprintf(output, "\tcall\tarctan_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_exp() {
  fprintf(output, "\tcall\texp_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_ln() {
  fprintf(output, "\tcall\tln_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_trunc() {
  int label;

  set_trunc_labels();
  label = new_label();
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t.L%d(%%rip),%%xmm1\n", trunc_label1);
  fprintf(output, "\tandpd\t%%xmm0,%%xmm1\n");
  fprintf(output, "\tcomisd\t.L%d(%%rip),%%xmm1\n", trunc_label2);
  fprintf(output, "\tjae\t.L%d\n", label);
  fprintf(output, "\tcvttsd2siq\t%%xmm0,%%rax\n");
  fprintf(output, "\tcvtsi2sdq\t%%rax,%%xmm0\n");
  fprintf(output, ".L%d:\tmovsd\t%%xmm0,(%%rsp)\n", label);
}

void gen_round() {
  int label;

  set_trunc_labels();
  set_round_label();
  label = new_label();
  fprintf(output, "\tmovsd\t(%%rsp),%%xmm0\n");
  fprintf(output, "\tmovsd\t.L%d(%%rip),%%xmm1\n", trunc_label1);
  fprintf(output, "\tvandpd\t%%xmm0,%%xmm1,%%xmm2\n");
  fprintf(output, "\tcomisd\t.L%d(%%rip),%%xmm2\n", trunc_label2);
  fprintf(output, "\tjae\t.L%d\n", label);
  fprintf(output, "\tvandnpd\t%%xmm1,%%xmm0,%%xmm3\n");
  fprintf(output, "\taddsd\t.L%d(%%rip),%%xmm2\n", round_label);
  fprintf(output, "\tcvttsd2siq\t%%xmm2,%%rax\n");
  fprintf(output, "\tcvtsi2sdq\t%%rax,%%xmm0\n");
  fprintf(output, "\torpd\t%%xmm3,%%xmm0\n");
  fprintf(output, ".L%d:\tmovsd\t%%xmm0,(%%rsp)\n", label);
}

void set_trunc_labels() {
  if (trunc_flag == 0) {
    trunc_flag++;
    trunc_label1 = new_label();
    trunc_label2 = new_label();
  }
}

void set_round_label() {
  if (round_flag == 0) {
    round_flag++;
    round_label = new_label();
  }
}

void gen_odd() {
  fprintf(output, "\tandq\t$1,(%%rsp)\n");
}

void gen_inc() {
  fprintf(output, "\taddq\t$1,(%%rsp)\n");
}

void gen_dec() {
  fprintf(output, "\tsubq\t$1,(%%rsp)\n");
}

void gen_empty_set() {
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_to_set() {
  fprintf(output, "\tpop\t%%rdx\n");
  gen_empty_set();
  fprintf(output, "\tmov\t%%rdx,%%rcx\n");
  fprintf(output, "\tshr\t$6,%%rdx\n");
  fprintf(output, "\tbts\t%%rcx,(%%rsp,%%rdx)\n");
}

void gen_to_set_range() {
  int l0, l1, l2, l3, l4;

  l0 = new_label();
  l1 = new_label();
  l2 = new_label();
  l3 = new_label();
  l4 = new_label();
  fprintf(output, "\tpop\t%%rdx\n");                 /* %rdx =  '9'  0x39 */
  fprintf(output, "\tpop\t%%rdi\n");                 /* %rdi = '0'  0x30 */
  fprintf(output, "\tcmp\t%%rdx,%%rdi\n");           /* 0x30 ?? 0x39 */
  fprintf(output, "\tjle\t.L%d\n", l0);
  gen_empty_set();
  fprintf(output, "\tjmp\t.L%d\n", l4);
  fprintf(output, ".L%d:\txor\t%%r8,%%r8\n", l0);    /* %r8 = 0 */
  fprintf(output, "\tmov\t%%rdi,%%rsi\n");           /* %rsi = 0x30 */
  fprintf(output, "\tshr\t$6,%%rsi\n");              /* %rsi = 0x30/64 = 0 */
  fprintf(output, "\tmov\t%%rdx,%%rcx\n");           /* %rcx = 0x39 */
  fprintf(output, "\tshr\t$6,%%rcx\n");              /* %rcx = 0x39/64 = 0 */
  fprintf(output, "\tmov\t$-4,%%r10\n");             /* %r10 = -4 */
  fprintf(output, ".L%d:\tmov\t%%r8,%%r9\n", l1);    /* %r9= 0 */
  fprintf(output, "\ttest\t%%rsi,%%rsi\n");          /* %rsi & %rsi, 0&0 */
  fprintf(output, "\tjnz\t.L%d\n", l2);
  fprintf(output, "\tbts\t%%rdi,%%r9\n");            /* %r9 = 0x0001 0000 0000 0000 */
  fprintf(output, "\tneg\t%%r9\n");                  /* %r9 = 0xffff 0000 0000 0000 */
  fprintf(output, "\tmov\t$-1,%%r8\n");              /* %r8 = 0xffff ffff ffff ffff */
  fprintf(output, ".L%d:\ttest\t%%rcx,%%rcx\n", l2); /* %rcx & %rcx, 0 & 0 */
  fprintf(output, "\tjnz\t.L%d\n", l3);
  fprintf(output, "\txor\t%%rax,%%rax\n");           /* %rax = 0 */
  fprintf(output, "\tbts\t%%rdx,%%rax\n");           /* %rax = 0x0200 0000 0000 0000 */
  fprintf(output, "\tshl\t$1,%%rax\n");              /* %rax = 0x0400 0000 0000 0000 */
  fprintf(output, "\tsub\t$1,%%rax\n");              /* %rax = 0x03ff ffff ffff ffff */
  fprintf(output, "\tand\t%%rax,%%r9\n");            /* %r9 = 0x03ff 0000 0000 0000 */
  fprintf(output, "\txor\t%%r8,%%r8\n");             /* %r8 = 0 */
  fprintf(output, ".L%d:\tmov\t%%r9,(%%rsp,%%r10,8)\n", l3);
  fprintf(output, "\tdec\t%%rsi\n");
  fprintf(output, "\tdec\t%%rcx\n");
  fprintf(output, "\tinc\t%%r10\n");
  fprintf(output, "\tjnz\t.L%d\n", l1);
  fprintf(output, "\tsub\t$32,%%rsp\n");
  fprintf(output, ".L%d:\n", l4);
}

void gen_set_union() {
  fprintf(output, "\tvmovupd\t(%%rsp),%%ymm0\n");
  fprintf(output, "\tadd\t$32,%%rsp\n");
  fprintf(output, "\tvorpd\t(%%rsp),%%ymm0,%%ymm0\n");
  fprintf(output, "\tvmovupd\t%%ymm0,(%%rsp)\n");
}  

void gen_set_intersection() {
  fprintf(output, "\tvmovupd\t(%%rsp),%%ymm0\n");
  fprintf(output, "\tadd\t$32,%%rsp\n");
  fprintf(output, "\tvandpd\t(%%rsp),%%ymm0,%%ymm0\n");
  fprintf(output, "\tvmovupd\t%%ymm0,(%%rsp)\n");
}

void gen_set_difference() {
  fprintf(output, "\tvmovupd\t(%%rsp),%%ymm0\n");
  fprintf(output, "\tadd\t$32,%%rsp\n");
  fprintf(output, "\tvandnpd\t(%%rsp),%%ymm0,%%ymm0\n");
  fprintf(output, "\tvmovupd\t%%ymm0,(%%rsp)\n");
}

void gen_in() {
  int l1, l2;
  l1 = new_label();
  l2 = new_label();
  fprintf(output, "\tmov\t32(%%rsp),%%rcx\n");
  fprintf(output, "\tmov\t%%rcx,%%rax\n");
  fprintf(output, "\tshr\t$8,%%rax\n");
  fprintf(output, "\tjz\t.L%d\n", l1);
  fprintf(output, "\txor\t%%rax,%%rax\n");
  fprintf(output, "\tjmp\t.L%d\n", l2);
  fprintf(output, ".L%d:\tmov\t%%rcx,%%rax\n", l1);
  fprintf(output, "\tshr\t$6,%%rax\n");
  fprintf(output, "\tmov\t(%%rsp,%%rax,8),%%rax\n");
  fprintf(output, "\tshr\t%%cl,%%rax\n");
  fprintf(output, "\tand\t$1,%%rax\n");
  fprintf(output, ".L%d:\tadd\t$32,%%rsp\n", l2);
  fprintf(output, "\tmov\t%%rax,(%%rsp)\n");
}

void gen_eof() {
  fprintf(output, "\tcall\teof_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_eoln() {
  fprintf(output, "\tcall\teoln_x\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_jump(int label) {
  fprintf(output, "\tjmp\t.L%d\n", label);
}

void gen_jump_true(int label) {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tjnz\t.L%d\n", label);
}

void gen_jump_false(int label) {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\ttest\t%%rax,%%rax\n");
  fprintf(output, "\tjz\t.L%d\n", label);
}

void gen_jump_label(char *label) {
  fprintf(output, "\tjmp\t.LG%s\n", label);
}

void gen_jump_index(int label, long size) {
  fprintf(output, "\tpop\t%%rax\n");
  fprintf(output, "\tcmp\t$%ld,%%rax\n", size);
  fprintf(output, "\tjae\tcase_error\n");
  fprintf(output, "\tmov\t.L%d(,%%rax,8),%%rax\n", label);
  fprintf(output, "\tjmp\t*%%rax\n");
};

void gen_jump_table(int *table, long size) {
  long i;
  
  for (i = 0; i < size; i++) {
    if (table[i] == 0)
      fprintf(output, "\t.quad\tcase_error\n");
    else
      fprintf(output, "\t.quad\t.L%d\n", table[i]);
  }
}

void gen_interprocedural_jump(long level, long locals, char *label) {
  fprintf(output, "\tmov\t%ld(%%rbp),%%rbp\n", -8*(level+1));
  fprintf(output, "\tlea\t%ld(%%rbp),%%rsp\n", -8*(level+2) - locals);
  fprintf(output, "\tjmp\t.LG%s\n", label);
}

void gen_argc() {
  fprintf(output, "\tmov\tp_argc(%%rip),%%rax\n");
  fprintf(output, "\tpush\t%%rax\n");
}

void gen_argv() {
  fprintf(output, "\tcall\targv_x\n");
}

void gen_flush() {
  fprintf(output, "\tcall\tflush_x\n");
}

void gen_close() {
  fprintf(output, "\tcall\tclose_x\n");
}

void add_string(char *string, int label) {
  String_Node *ptr = new(String_Node);
  ptr->str = string;
  ptr->lab = label;
  ptr->next = strings;
  strings = ptr;
}

void gen_dump_bits() {
  if (trunc_flag == 1) {
    trunc_flag++;
    fprintf(output, "\t.align\t8\n");
    fprintf(output, ".L%d:\t.quad\t%ld\n", trunc_label1, 0x7fffffffffffffff);
    fprintf(output, ".L%d:\t.quad\t%ld\n", trunc_label2, 0x4330000000000000);
  }
  if (round_flag == 1) {
    round_flag++;
    fprintf(output, "\t.align\t8\n");
    fprintf(output, ".L%d:\t.quad\t%ld\n", round_label, 0x7fe0000000000000);
  }
}

void gen_dump_strings() {
  while (strings) {
    fprintf(output, "\t.align\t8\n");
    fprintf(output, ".L%d:\t.ascii\t", strings->lab);
    output_string(strings->str);
    fprintf(output, "\n");
    strings = strings->next;
  }
}

void output_string(char *s) {
  fputc('"', output);
  while (*s) {
    if (*s == '"') {
      fputc('\\', output);
      fputc('"', output);
    }
    else if (isprint(*s))
      fputc(*s, output);
    else
      fprintf(output, "\\%03o", *s);
    s++;
  }
  fputc('"', output);
}

int new_label() {
  static int next_label = 1;
  return next_label++;
}
