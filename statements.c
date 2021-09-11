#include <stdio.h>
#include <stdlib.h>
#include "machine.h"
#include "pascal.h"

Statement_Sequence *statement_sequence(void);
Statement *statement(void);
Symbol *statement_label(void);
Statement *new_compound_statement(Statement_Sequence *, Symbol *);
Statement *new_empty_statement(Symbol *);
Statement *assignment_statement(Symbol *);
Statement *return_assignment_statement(Symbol *, Symbol *);
Statement *procedure_statement(Symbol *, Symbol *);
Statement *standard_procedure(Symbol *, Symbol *);
Statement *rewrite_procedure(Symbol *);
Statement *reset_procedure(Symbol *);
Statement *put_procedure(Symbol *);
Statement *get_procedure(Symbol *);
Statement *read_procedure(Symbol *);
Statement *readln_procedure(Symbol *);
Statement_Sequence *read_parameter_list(Symbol *, Statement_Sequence *);
Statement_Sequence *read_parameters(Symbol *, Statement_Sequence *);
Statement_Sequence *read_parameters_more(Symbol *, Expression *, Statement_Sequence *);
Statement_Sequence *read_parameters_text(Symbol *, Statement_Sequence *);
Statement_Sequence *read_parameters_text_more(Symbol *, Expression *, Statement_Sequence *);
Statement *write_procedure(Symbol *);
Statement *writeln_procedure(Symbol *);
Statement_Sequence *write_parameter_list(Symbol *, Statement_Sequence *);
Statement_Sequence *write_parameters(Symbol *, Statement_Sequence *);
Statement_Sequence *write_parameters_more(Symbol *, Expression *, Statement_Sequence *);
Statement_Sequence *write_parameters_text(Symbol *, Statement_Sequence *);
Statement_Sequence *write_parameters_text_more(Symbol *, Expression *, Statement_Sequence *);
Statement *page_procedure(Symbol *);
Statement *new_page_procedure(Expression *, Symbol *);
Statement *new_procedure(Symbol *);
Statement *new_new_procedure(Expression *, Symbol *);
Statement *dispose_procedure(Symbol *);
Statement *new_dispose_procedure(Expression *, Symbol *);
Statement *pack_procedure(Symbol *);
Statement *new_pack_procedure(Expression *, Expression *, Expression *, Symbol *);
Statement *unpack_procedure(Symbol *);
Statement *new_unpack_procedure(Expression *, Expression *, Expression *, Symbol *);
Statement *argv_procedure(Symbol *);
Statement *flush_procedure(Symbol *);
Statement *new_flush_procedure(Expression *, Symbol *);
Statement *close_procedure(Symbol *);
Statement *new_close_procedure(Expression *, Symbol *);
Statement *exit_procedure(Symbol *);
Statement *new_exit_procedure(Expression *, Symbol *);
Statement *goto_statement(Symbol *);
Symbol *goto_label(void);
Statement *new_goto_statement(char *, Symbol *);
Statement *if_statement(Symbol *);
Statement *new_if_statement(Expression *, Statement *, Statement *, Symbol *);
Statement *case_statement(Symbol *);
Case_Element_List *case_element_list(void);
Statement *new_case_statement(Expression *, Case_Element_List *, Symbol *);
Statement *repeat_statement(Symbol *);
Statement *while_statement(Symbol *);
Statement *new_while_statement(Expression *, Statement *, Symbol *);
Statement *new_repeat_statement(Statement_Sequence *, Expression *, Symbol *);
Statement *for_statement(Symbol *);
Statement *new_for_statement(Symbol *, Symbol *, Symbol *,
			     Expression *, Expression *,
			     Statement *, For_Mode,
			     Symbol *);
Statement *with_statement(Symbol *);
Statement *new_with_statement(Expression *, Symbol *, Statement *, Symbol *);
Symbol *push_with_variable(Type *);
void pop_with_variable(void);
Statement *record_variable_list(Symbol *);

Statement_Sequence *statement_sequence() {
  Statement_Sequence *stmts = new(Statement_Sequence);

  stmts->statement = statement();
  if (match(SEMICOLON_TOKEN))
    stmts->next = statement_sequence();
  else
    stmts->next = 0;
  return stmts;
}

Statement *statement() {
  Symbol *label;
  Symbol *sym;
  
  if (check(INTEGER_TOKEN))
    label = statement_label();
  else
    label = 0;
  
  switch (token_type) {
  case SEMICOLON_TOKEN:
  case END_TOKEN:
  case ELSE_TOKEN:
  case UNTIL_TOKEN:
    return new_empty_statement(label);
  case IDENTIFIER_TOKEN:
    sym = lookup_symbol(token);
    if (sym == 0)
      error("%s undefined", token);
    if (sym) {
      switch (sym->class) {
      case FUNCTION_SYMBOL:
	return return_assignment_statement(sym, label);
      case VARIABLE_SYMBOL:
      case VALUE_PARAMETER:
      case VARIABLE_PARAMETER:
      case FIELD_SYMBOL:
	return assignment_statement(label);
      case PROCEDURE_SYMBOL:
      case PROCEDURE_PARAMETER:
	return procedure_statement(sym, label);
      case STANDARD_PROCEDURE:
	return standard_procedure(sym, label);
      default:
	break;
      }
    }
    error("illegal statement near %s", token);
    return 0;
  case GOTO_TOKEN:
    return goto_statement(label);
  case BEGIN_TOKEN:
    return compound_statement(label);
  case IF_TOKEN:
    return if_statement(label);
  case CASE_TOKEN:
    return case_statement(label);
  case REPEAT_TOKEN:
    return repeat_statement(label);
  case WHILE_TOKEN:
    return while_statement(label);
  case FOR_TOKEN:
    return for_statement(label);
  case WITH_TOKEN:
    return with_statement(label);
  default:
    error("illegal statement near %s", token);
    return 0;
  }
}

Symbol *statement_label(void) {
  Symbol *sym;
  char *lab = label();

  sym = lookup_symbol(lab);
  need(COLON_TOKEN);
  return sym;
}

Statement *new_empty_statement(Symbol *label) {
  Statement *stmt = new_statement(EMPTY_STATEMENT, label);
  return stmt;
}

Statement *assignment_statement(Symbol *label) {
  Expression *lval, *rval;
  
  lval = variable_access();
  need(ASSIGN_TOKEN);
  rval = expression();
  return new_assignment_statement(lval, rval, label, true);
}

Statement *return_assignment_statement(Symbol *func, Symbol *label) {
  Expression *lval, *rval;
  
  next_token();
  lval = new_variable_expression(func->algorithm.return_value);
  need(ASSIGN_TOKEN);
  rval = expression();
  return new_assignment_statement(lval, rval, label, true);
}   

Statement *procedure_statement(Symbol *sym, Symbol *label) {
  Expression_List *exprs = 0;
  
  next_token();
  if (match(LPAREN_TOKEN)) {
    if (sym->algorithm.parameters == 0)
      error("procedure parameters don't match procedure definition");
    else
      exprs = actual_parameter_list(sym->algorithm.parameters);
    need(RPAREN_TOKEN);
  }
  else if (sym->algorithm.parameters != 0)
    error("procedure parameters don't match procedure definition");
  return new_procedure_call(sym, exprs, label);
}

Statement *standard_procedure(Symbol *proc, Symbol *label) {
  next_token(); /* skip the identifier we already looked up in 'proc' */
  switch (proc->stdproc) {
  case REWRITE_PROCEDURE:
    return rewrite_procedure(label);
  case RESET_PROCEDURE:
    return reset_procedure(label);
  case PUT_PROCEDURE:
    return put_procedure(label);
  case GET_PROCEDURE:
    return get_procedure(label);
  case READ_PROCEDURE:
    return read_procedure(label);
  case READLN_PROCEDURE:
    return readln_procedure(label);
  case WRITE_PROCEDURE:
    return write_procedure(label);
  case WRITELN_PROCEDURE:
    return writeln_procedure(label);
  case PAGE_PROCEDURE:
    return page_procedure(label);
  case NEW_PROCEDURE:
    return new_procedure(label);
  case DISPOSE_PROCEDURE:
    return dispose_procedure(label);
  case PACK_PROCEDURE:
    return pack_procedure(label);
  case UNPACK_PROCEDURE:
    return unpack_procedure(label);
  case ARGV_PROCEDURE:
    return argv_procedure(label);
  case FLUSH_PROCEDURE:
    return flush_procedure(label);
  case CLOSE_PROCEDURE:
    return close_procedure(label);
  case EXIT_PROCEDURE:
    return exit_procedure(label);
  }
  return 0;
}

Statement *rewrite_procedure(Symbol *label) {
  Expression *file, *name;

  need(LPAREN_TOKEN);
  file = expression();
  if (match(COMMA_TOKEN))
    name = expression();
  else
    name = 0;
  need(RPAREN_TOKEN);
  return new_rewrite_statement(file, name, label);
}

Statement *reset_procedure(Symbol *label) {
  Expression *file, *name;

  need(LPAREN_TOKEN);
  file = expression();
  if (match(COMMA_TOKEN))
    name = expression();
  else
    name = 0;
  need(RPAREN_TOKEN);
  return new_reset_statement(file, name, label);
}

Statement *put_procedure(Symbol *label) {
  Expression *file;

  need(LPAREN_TOKEN);
  file = expression();
  need(RPAREN_TOKEN);
  return new_put_statement(file, label);
}

Statement *get_procedure(Symbol *label) {
  Expression *file;

  need(LPAREN_TOKEN);
  file = expression();
  need(RPAREN_TOKEN);
  return new_get_statement(file, label);
}

Statement *read_procedure(Symbol *label) {
  Statement_Sequence *stmts;
  Symbol *file;

  file = create_tempory_file_variable();

  need(LPAREN_TOKEN);
  stmts = read_parameter_list(file, 0);
  need(RPAREN_TOKEN);
  return new_compound_statement(stmts, label);
}

Statement *readln_procedure(Symbol *label) {
  Statement_Sequence *stmts, *nstmts;
  Expression *lval, *rval;
  Symbol *file;

  file = create_tempory_file_variable();

  stmts = new(Statement_Sequence);
  stmts->statement = new_readln_statement(file);
  stmts->next = 0;

  if (match(LPAREN_TOKEN)) {
    stmts = read_parameter_list(file, stmts);
    need(RPAREN_TOKEN);
  }
  else {
    lval = new_variable_expression(file);
    rval = new_variable_expression(lookup_symbol("input"));
    nstmts = new(Statement_Sequence);
    nstmts->statement = new_assignment_statement(lval, rval, 0, false);
    nstmts->next = stmts;
    stmts = nstmts;
  }
  return new_compound_statement(stmts, label);
}

Statement_Sequence *read_parameter_list(Symbol *file, Statement_Sequence *coda) {
  Statement_Sequence *stmts;
  Expression *e, *lval, *rval;

  stmts = new(Statement_Sequence);

  e = expression();
  if (is_file(e->type)) {
    lval = new_variable_expression(file);
    rval = e;
    stmts->statement = new_assignment_statement(lval, rval, 0, false);
    if (match(COMMA_TOKEN))
      if (e->type == text_type)
	stmts->next = read_parameters_text(file, coda);
      else
	stmts->next = read_parameters(file, coda);
    else
      stmts->next = coda;
  }
  else {
    lval = new_variable_expression(file);
    rval = new_variable_expression(lookup_symbol("input"));
    stmts->statement = new_assignment_statement(lval, rval, 0, false);
    stmts->next = read_parameters_text_more(file, e, coda);
  }
  return stmts;
}				    

Statement_Sequence *read_parameters(Symbol *file, Statement_Sequence *coda) {
  return read_parameters_more(file, variable_access(), coda);
}

Statement_Sequence *read_parameters_more(Symbol *file, Expression *e,
					 Statement_Sequence *coda) {
  Statement_Sequence *stmts;

  stmts = new(Statement_Sequence);
  stmts->statement = new_read_statement(file, e);
  if (match(COMMA_TOKEN))
    stmts->next = read_parameters(file, coda);
  else
    stmts->next = coda;
  return stmts;
}

Statement_Sequence *read_parameters_text(Symbol *file, Statement_Sequence *coda) {
  return read_parameters_text_more(file, variable_access(), coda);
}

Statement_Sequence *read_parameters_text_more(Symbol *file, Expression *e,
					      Statement_Sequence *coda) {
  Statement_Sequence *stmts;

  stmts = new(Statement_Sequence);
  stmts->statement = new_read_text_statement(file, e);
  if (match(COMMA_TOKEN))
    stmts->next = read_parameters_text(file, coda);
  else
    stmts->next = coda;
  return stmts;
}

Statement *write_procedure(Symbol *label) {
  Statement_Sequence *stmts;
  Symbol *file;

  file = create_tempory_file_variable();
  
  need(LPAREN_TOKEN);
  stmts = write_parameter_list(file, 0);
  need(RPAREN_TOKEN);
  return new_compound_statement(stmts, label);
}

Statement *writeln_procedure(Symbol *label) {
  Statement_Sequence *stmts, *nstmts;
  Expression *lval, *rval;
  Symbol *file;

  /* create tempory variable to store file expression */
  
  file = create_tempory_file_variable();

  /* create trailing statement sequence */
  
  stmts = new(Statement_Sequence);
  stmts->statement = new_writeln_statement(file);
  stmts->next = 0;
  
  if (match(LPAREN_TOKEN)) {
    stmts = write_parameter_list(file, stmts);
    need(RPAREN_TOKEN);
  }
  else {
    lval = new_variable_expression(file);
    rval = new_variable_expression(lookup_symbol("output"));
    nstmts = new(Statement_Sequence);
    nstmts->statement = new_assignment_statement(lval, rval, 0, false);
    nstmts->next = stmts;
    stmts = nstmts;
  }
  return new_compound_statement(stmts, label);
}

Statement_Sequence *write_parameter_list(Symbol *file, Statement_Sequence *coda) {
  Statement_Sequence *stmts;
  Expression *e, *lval, *rval;

  stmts = new(Statement_Sequence);
  
  e = expression();
  if (is_file(e->type)) {
    lval = new_variable_expression(file);
    rval = e;
    stmts->statement = new_assignment_statement(lval, rval, 0, false);
    if (match(COMMA_TOKEN))
      if (e->type == text_type)
	stmts->next = write_parameters_text(file, coda);
      else
	stmts->next = write_parameters(file, coda);
    else
      stmts->next = coda;
  }
  else {
    lval = new_variable_expression(file);
    rval = new_variable_expression(lookup_symbol("output"));
    stmts->statement = new_assignment_statement(lval, rval, 0, false);
    stmts->next = write_parameters_text_more(file, e, coda);
  }
  return stmts;
}

Statement_Sequence *write_parameters(Symbol *file, Statement_Sequence *coda) {
  return write_parameters_more(file, expression(), coda);
}

Statement_Sequence *write_parameters_more(Symbol *file, Expression *e,
					  Statement_Sequence *coda) {
  Statement_Sequence *stmts;

  stmts = new(Statement_Sequence);
  stmts->statement = new_write_statement(file, e);
  if (match(COMMA_TOKEN))
    stmts->next = write_parameters(file, coda);
  else
    stmts->next = coda;
  return stmts;
}

Statement_Sequence *write_parameters_text(Symbol *file, Statement_Sequence *coda) {
  return write_parameters_text_more(file, expression(), coda);
}

Statement_Sequence *write_parameters_text_more(Symbol *file, Expression *e,
					  Statement_Sequence *coda) {
  Statement_Sequence *stmts;
  Expression *f1, *f2;

  f1 = 0;
  f2 = 0;
  if (match(COLON_TOKEN)) {
    f1 = expression();
    if (match(COLON_TOKEN))
      f2 = expression();
  }
  stmts = new(Statement_Sequence);
  stmts->statement = new_write_text_statement(file, e, f1, f2);
  if (match(COMMA_TOKEN))
    stmts->next = write_parameters_text(file, coda);
  else
    stmts->next = coda;
  return stmts;
}

Statement *page_procedure(Symbol *label) {
  Expression *e;
  
  if (match(LPAREN_TOKEN)) {
    e = expression();
    need(RPAREN_TOKEN);
  }
  else
    e = new_variable_expression(lookup_symbol("output"));
  return new_page_procedure(e, label);
}

Statement *new_page_procedure(Expression *e, Symbol *label) {
  Statement *stmt = new_statement(PAGE_STATEMENT, label);
  if (e->type != text_type)
    error("parameter to page procedure not a text file");
  stmt->parameter = e;
  return stmt;
}

Statement *new_procedure(Symbol *label) {
  Expression *e;
  
  need(LPAREN_TOKEN);
  e = variable_access();
  need(RPAREN_TOKEN);
  return new_new_procedure(e, label);
}

Statement *new_new_procedure(Expression *e, Symbol *label) {
  Statement *stmt;

  if (e->type->class != POINTER_TYPE)
    error("parameter to new procedure not a pointer");
  stmt = new_statement(NEW_STATEMENT, label);
  stmt->parameter = e;
  return stmt;
}
    
Statement *dispose_procedure(Symbol *label) {
  Expression *e;

  need(LPAREN_TOKEN);
  e = expression();
  need(RPAREN_TOKEN);
  return new_dispose_procedure(e, label);
}

Statement *new_dispose_procedure(Expression *e, Symbol *label) {
  Statement *stmt;

  if (e->type->class != POINTER_TYPE)
    error("parameter to dispose procedure not a pointer");
  stmt = new_statement(DISPOSE_STATEMENT, label);
  stmt->parameter = e;
  return stmt;
}

Statement *pack_procedure(Symbol *label) {
  Expression *a, *i, *z;

  need(LPAREN_TOKEN);
  a = variable_access();
  need(COMMA_TOKEN);
  i = expression();
  need(COMMA_TOKEN);
  z = variable_access();
  need(RPAREN_TOKEN);
  return new_pack_procedure(a, i, z, label);
}

Statement *new_pack_procedure(Expression *a, Expression *i,
			      Expression *z, Symbol *label) {
  Statement *stmt = new_statement(PACK_STATEMENT, label);
  stmt->pack.a = a;
  stmt->pack.i = i;
  stmt->pack.z = z;
  return stmt;
}

Statement *unpack_procedure(Symbol *label) {
  Expression *z, *a, *i;
  
  need(LPAREN_TOKEN);
  z = variable_access();
  need(COMMA_TOKEN);
  a = variable_access();
  need(COMMA_TOKEN);
  i = expression();
  need(RPAREN_TOKEN);
  return new_unpack_procedure(z, a, i, label);
}

Statement *new_unpack_procedure(Expression *z, Expression *a,
				Expression *i, Symbol *label) {
  Statement *stmt = new_statement(UNPACK_STATEMENT, label);
  stmt->pack.z = z;
  stmt->pack.a = a;
  stmt->pack.i = i;
  return stmt;
}

Statement *argv_procedure(Symbol *label) {
  Expression *index, *arg;
  
  need(LPAREN_TOKEN);
  index = expression();
  need(COMMA_TOKEN);
  arg = variable_access();
  need(RPAREN_TOKEN);
  return new_argv_statement(index, arg, label);
}

Statement *new_argv_statement(Expression *index, Expression *arg, Symbol *label) {
  Statement *stmt = new_statement(ARGV_STATEMENT, label);
  stmt->argv.index = index;
  stmt->argv.arg = arg;
  return stmt;
}

Statement *flush_procedure(Symbol *label) {
  Expression *e;

  if (match(LPAREN_TOKEN)) {
    e = expression();
    need(RPAREN_TOKEN);
  }
  else
    e = new_variable_expression(lookup_symbol("input"));
  return new_flush_procedure(e, label);
}

Statement *new_flush_procedure(Expression *e, Symbol *label) {
  Statement *stmt;
  
  if (e->type->class != FILE_TYPE)
    error("parameter to flush procedure is not a file");
  stmt = new_statement(FLUSH_STATEMENT, label);
  stmt->parameter = e;
  return stmt;
}

Statement *close_procedure(Symbol *label) {
  Expression *e;

  need(LPAREN_TOKEN);
  e = expression();
  need(RPAREN_TOKEN);
  return new_close_procedure(e, label);
}

Statement *new_close_procedure(Expression *e, Symbol *label) {
  Statement *stmt;

  if (e->type->class != FILE_TYPE)
    error("parameter to close procedure is not a file");
  stmt = new_statement(CLOSE_STATEMENT, label);
  stmt->parameter = e;
  return stmt;
}

Statement *exit_procedure(Symbol *label) {
  Expression *e;

  need(LPAREN_TOKEN);
  e = expression();
  need(RPAREN_TOKEN);
  return new_exit_procedure(e, label);
}

Statement *new_exit_procedure(Expression *e, Symbol *label) {
  Statement *stmt;

  /* allow exit(true)/exit(false) in addition to exit(0) */
  
  if (e->type->class != ORDINAL_TYPE)
    error("parameter to exit procedure in not ordinal");
  stmt = new_statement(EXIT_STATEMENT, label);
  stmt->parameter = e;
  return stmt;
}

Statement *goto_statement(Symbol *stmt_label) {
  char *lab;
  next_token();
  lab = label();
  return new_goto_statement(lab, stmt_label);
}

Statement *new_goto_statement(char *goto_label, Symbol *statement_label) {
  Statement *stmt = new_statement(GOTO_STATEMENT, statement_label);

  stmt->gotostmt = lookup_symbol(goto_label);
  if (stmt->gotostmt == 0)
    error("%s undefined\n");
  return stmt;
}

Statement *compound_statement(Symbol *label) {
  Statement_Sequence *stmts;
  
  need(BEGIN_TOKEN);
  stmts = statement_sequence();
  need(END_TOKEN);
  return new_compound_statement(stmts, label);
}

Statement *new_compound_statement(Statement_Sequence *stmts, Symbol *label) {
  Statement *stmt = new_statement(COMPOUND_STATEMENT, label);
  stmt->compound_statement = stmts;
  return stmt;
}

Statement *if_statement(Symbol *label) {
  Expression *e;
  Statement *tstmt;
  Statement *fstmt;
  
  next_token();
  e = expression();
  need(THEN_TOKEN);
  tstmt = statement();
  if (match(ELSE_TOKEN))
    fstmt = statement();
  else
    fstmt = 0;
  return new_if_statement(e, tstmt, fstmt, label);
}

Statement *new_if_statement(Expression *e, Statement *tstmt, Statement *fstmt, Symbol *label) {
  Statement *stmt = new_statement(IF_STATEMENT, label);
  stmt->ifstmt.test = e;
  stmt->ifstmt.tstmt = tstmt;
  stmt->ifstmt.fstmt = fstmt;
  return stmt;
}

Statement *case_statement(Symbol *label) {
  Case_Element_List *elements;
  Expression *index;
  
  next_token();
  index = expression();
  need(OF_TOKEN);
  elements = case_element_list();
  need(END_TOKEN);
  return new_case_statement(index, elements, label);
}

Case_Element_List *case_element_list() {
  Case_Element_List *cases = new(Case_Element_List);
  if (match(ELSE_TOKEN))
    cases->constants = 0;
  else
    cases->constants = constant_list();
  need(COLON_TOKEN);
  cases->statement = statement();
  if (match(SEMICOLON_TOKEN)) {
    if (!check(END_TOKEN))
      cases->next = case_element_list();
    else
      cases->next = 0;
  }
  else
    cases->next = 0;
  return cases;
}

Statement *new_case_statement(Expression *index, Case_Element_List *elements, Symbol *label) {
  Statement *stmt = new_statement(CASE_STATEMENT, label);
  if (index->type->class != ORDINAL_TYPE)
    error("case switch type not ordinal");
  stmt->casestmt.index = index;
  stmt->casestmt.elements = elements;
  return stmt;
}

Statement *repeat_statement(Symbol *label) {
  Statement_Sequence *stmts;
  Expression *e;
  
  next_token();
  stmts = statement_sequence();
  need(UNTIL_TOKEN);
  e = expression();
  return new_repeat_statement(stmts, e, label);
}

Statement *new_repeat_statement(Statement_Sequence *body, Expression *e, Symbol *label) {
  Statement *stmt = new_statement(REPEAT_STATEMENT, label);
  stmt->repeatstmt.body = body;
  stmt->repeatstmt.test = e;
  return stmt;
}

Statement *while_statement(Symbol *label) {
  Expression *e;
  Statement *stmt;
  
  next_token();
  e = expression();
  need(DO_TOKEN);
  stmt = statement();
  return new_while_statement(e, stmt, label);
}

Statement *new_while_statement(Expression *e, Statement *body, Symbol *label) {
  Statement *stmt = new_statement(WHILE_STATEMENT, label);
  stmt->whilestmt.test = e;
  stmt->whilestmt.body = body;
  return stmt;
}

Statement *for_statement(Symbol *label) {
  Symbol *temp1, *temp2;
  Symbol *control_variable;
  Expression *initial_value;
  Expression *final_value;
  Statement *stmt;
  For_Mode mode;
  char *id;
  
  push_for_variable_stack();
  temp1 = create_for_variable(1);
  temp2 = create_for_variable(2);

  next_token();
  id = identifier();
  control_variable = lookup_symbol_local(id);
  need(ASSIGN_TOKEN);
  initial_value = expression();
  if (match(TO_TOKEN))
    mode = TO;
  else if (match(DOWNTO_TOKEN))
    mode = DOWNTO;
  else {
    error("'to' or 'downto' expected near %s", token);
    mode = TO;
  }
  final_value = expression();
  need(DO_TOKEN);
  stmt = statement();

  pop_for_variable_stack();
  return new_for_statement(temp1, temp2, control_variable,
			   initial_value, final_value,
			   stmt, mode, label);
}

Statement *new_for_statement(Symbol *temp1, Symbol *temp2, Symbol *ctrl,
			     Expression *ival, Expression *fval,
			     Statement *body, For_Mode mode,
			     Symbol *label) {
  Statement *stmt = new_statement(FOR_STATEMENT, label);
  stmt->forstmt.temp1 = temp1;
  stmt->forstmt.temp2 = temp2;
  stmt->forstmt.control_variable = ctrl;
  stmt->forstmt.initial_value = ival;
  stmt->forstmt.final_value = fval;
  stmt->forstmt.body = body;
  stmt->forstmt.mode = mode;
  return stmt;
}

Statement *with_statement(Symbol *label) {
  next_token();
  return record_variable_list(label);
}

Statement *record_variable_list(Symbol *label) {
  Statement *stmt;
  Expression *e;
  Symbol *variable;
  
  e = variable_access();
  if (match(COMMA_TOKEN)) {
    variable = push_with_variable(e->type);
    stmt = new_with_statement(e, variable, record_variable_list(0), label);
    pop_with_variable();
    return stmt;
  }
  if (match(DO_TOKEN)) {
    variable = push_with_variable(e->type);
    stmt = new_with_statement(e, variable, statement(), label);
    pop_with_variable();
    return stmt;
  }
  error("syntax error in with statement near %s", token);
  return 0;
}

Statement *new_with_statement(Expression *e, Symbol *variable, Statement *body, Symbol *label) {
  Statement *stmt = new_statement(WITH_STATEMENT, label);
  stmt->with.record = e;
  stmt->with.variable = variable;
  stmt->with.body = body;
  return stmt;
}

Symbol *push_with_variable(Type *type) {
  Symbol_List *syms;
  Symbol *variable;
  /*
    create tempory variable of type 'type'
    push reference to variable to with-variable-stack
    enter fields of type 'type' in to symbol table
    return tempory variable.
  */
  variable = create_tempory_with_variable(type);
  
  syms = new(Symbol_List);
  syms->symbol = variable;
  syms->next = with_symbol_stack;
  with_symbol_stack = syms;

  push_symbol_table();
  
  insert_record_fields(type);

  return variable;
}

void pop_with_variable() {
  pop_symbol_table();
}



    
    
  
  
