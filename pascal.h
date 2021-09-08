#define internal_error() x_internal_error(__FILE__,__LINE__,__FUNCTION__)

#define new(T) (T *)malloc(sizeof(T))

typedef enum {false, true} bool;

typedef enum {TO, DOWNTO} For_Mode;

typedef struct Identifier_List {
  char *id;
  struct Identifier_List *next;
} Identifier_List;

typedef enum {
  AND_TOKEN, ARRAY_TOKEN, BEGIN_TOKEN, CASE_TOKEN, CONST_TOKEN, DIV_TOKEN,
  DO_TOKEN, DOWNTO_TOKEN, ELSE_TOKEN, END_TOKEN, FILE_TOKEN, FOR_TOKEN,
  FUNCTION_TOKEN, GOTO_TOKEN, IF_TOKEN, IN_TOKEN, LABEL_TOKEN, MOD_TOKEN,
  NIL_TOKEN, NOT_TOKEN, OF_TOKEN, OR_TOKEN, OTHERWISE_TOKEN, PACKED_TOKEN,
  PROCEDURE_TOKEN, PROGRAM_TOKEN, RECORD_TOKEN, REPEAT_TOKEN, SET_TOKEN,
  THEN_TOKEN, TO_TOKEN, TYPE_TOKEN, UNTIL_TOKEN, VAR_TOKEN, WHILE_TOKEN,
  WITH_TOKEN,
  PLUS_TOKEN, MINUS_TOKEN, MULTIPLY_TOKEN, DIVIDE_TOKEN,
  EQ_TOKEN, NE_TOKEN, LT_TOKEN, LE_TOKEN, GT_TOKEN, GE_TOKEN,
  PERIOD_TOKEN, COMMA_TOKEN, COLON_TOKEN, SEMICOLON_TOKEN,
  LPAREN_TOKEN, RPAREN_TOKEN, LBRACK_TOKEN, RBRACK_TOKEN,
  ASSIGN_TOKEN, ARROW_TOKEN, ELLIPSIS_TOKEN,
  IDENTIFIER_TOKEN, INTEGER_TOKEN, REAL_TOKEN, STRING_TOKEN,
  END_OF_FILE_TOKEN
} Token_Type;

typedef struct Ordinal_Type {
  struct Type *base;
  Ordinal low;
  Ordinal high;
} Ordinal_Type;

typedef struct Array_Type {
  bool packed;
  struct Type *component_type;
  struct Type *index_type;
} Array_Type;

typedef struct File_Type {
  bool packed;
  struct Type *base;
} File_Type;

typedef struct Set_Type {
  bool packed;
  struct Type *base;
} Set_Type;

typedef struct Variant_List {
  struct Constant_List *cnsts;
  struct Type *fields;
  struct Variant_List *next;
} Variant_List;

typedef struct Variant_Part {
  struct Symbol *selector;
  Variant_List *variants;
} Variant_Part;

typedef struct Record_Type {
  bool packed;
  struct Symbol_List *fields;
  Variant_Part *variant;
} Record_Type;

typedef enum {
  ORDINAL_TYPE, REAL_TYPE, NIL_TYPE, ARRAY_TYPE,
  RECORD_TYPE, SET_TYPE, FILE_TYPE, POINTER_TYPE
} Type_Class;

typedef struct Type {
  Type_Class class;
  int size;
  union {
    Ordinal_Type ordinal;
    Array_Type array;
    File_Type file;
    Set_Type set;
    Record_Type record;
    struct Type *pointer;
  };
} Type;

typedef struct Constant {
  struct Type *type;
  union {
    Ordinal ordinal;
    Real real;
    char *string;
  };
} Constant;

typedef struct Constant_List {
  Constant *cnst;
  struct Constant_List *next;
} Constant_List;

typedef enum {
  REWRITE_PROCEDURE, RESET_PROCEDURE, PUT_PROCEDURE, GET_PROCEDURE,
  READ_PROCEDURE, WRITE_PROCEDURE, READLN_PROCEDURE, WRITELN_PROCEDURE,
  PAGE_PROCEDURE, NEW_PROCEDURE, DISPOSE_PROCEDURE,
  PACK_PROCEDURE, UNPACK_PROCEDURE,
  ARGV_PROCEDURE, FLUSH_PROCEDURE, CLOSE_PROCEDURE,
} Standard_Procedure;

typedef enum {
  ABS_FUNCTION, SQR_FUNCTION, SIN_FUNCTION, COS_FUNCTION, ARCTAN_FUNCTION,
  EXP_FUNCTION, LN_FUNCTION, SQRT_FUNCTION, TRUNC_FUNCTION, ROUND_FUNCTION,
  ORD_FUNCTION, CHR_FUNCTION, SUCC_FUNCTION, PRED_FUNCTION, ODD_FUNCTION,
  EOF_FUNCTION, EOLN_FUNCTION,
  ARGC_FUNCTION,
} Standard_Function;

typedef struct Variable_Symbol {
  Type *type;
  int offset;
  Type *parent; /* used for field symbols to
			    connect to the record that contains this field */
} Variable_Symbol;

typedef enum {UNKNOWN, DEFINED, EXTERNAL, FORWARD} Directive;

typedef struct Algorithm_Symbol {
  int local_size;
  int parameter_size;
  Directive declared;
  struct Symbol_List *parameters;
  struct Symbol_List *algorithms;
  struct Statement *statement;
  Type *type;
  struct Symbol *return_value;
} Algorithm_Symbol;

typedef struct Label_Symbol {
  struct Symbol *algorithm;    /* this is the program/procedure/function that the label is defined in */
} Label_Symbol;

typedef enum {
  PROGRAM_SYMBOL, PROCEDURE_SYMBOL, FUNCTION_SYMBOL,
  CONSTANT_SYMBOL, TYPE_SYMBOL, VARIABLE_SYMBOL,
  VALUE_PARAMETER, VARIABLE_PARAMETER,
  FIELD_SYMBOL,
  PROCEDURE_PARAMETER, FUNCTION_PARAMETER,
  STANDARD_PROCEDURE, STANDARD_FUNCTION,
  LABEL_SYMBOL,
} Symbol_Class;

typedef struct Symbol {
  Symbol_Class class;
  char *name;
  int block_level;
  union {
    struct Constant *constant;         /* CONSTANT_SYMBOL */
    Type *type;                        /* TYPE_SYMBOL */
    Variable_Symbol variable;          /* VARIABLE_SYMBOL, FIELD_SYMBOL, VARIABLE_PARAMETER, VALUE_PARAMETER */
    Algorithm_Symbol algorithm;        /* PROGRAM_SYMBOL, PROCEDURE_SYMBOL, FUNCTION_SYMBOL */
    Standard_Procedure stdproc;        /* STANDARD_PROCEDURE */
    Standard_Function stdfunc;         /* STANDARD_FUNCTION */
    Label_Symbol label;                /* LABEL_SYMBOL */
  };
} Symbol;

typedef struct Call_Expression { /* used in Expression and Statement */
  Symbol *sym;
  struct Expression_List *params;
} Call_Expression;

typedef struct Field_Expression {
  struct Expression *base;
  Symbol *field;
} Field_Expression;

typedef enum {
  VARIABLE_EXPRESSION, CONSTANT_EXPRESSION,
  ORDINAL_EQ_EXPRESSION, REAL_EQ_EXPRESSION,
  STRING_EQ_EXPRESSION, SET_EQ_EXPRESSION,
  POINTER_EQ_EXPRESSION,
  ORDINAL_NE_EXPRESSION, REAL_NE_EXPRESSION,
  STRING_NE_EXPRESSION, SET_NE_EXPRESSION,
  POINTER_NE_EXPRESSION,
  ORDINAL_LT_EXPRESSION, REAL_LT_EXPRESSION,
  STRING_LT_EXPRESSION,
  ORDINAL_LE_EXPRESSION, REAL_LE_EXPRESSION,
  STRING_LE_EXPRESSION, SET_LE_EXPRESSION,
  ORDINAL_GT_EXPRESSION, REAL_GT_EXPRESSION,
  STRING_GT_EXPRESSION,
  ORDINAL_GE_EXPRESSION, REAL_GE_EXPRESSION,
  STRING_GE_EXPRESSION, SET_GE_EXPRESSION,
  INTEGER_PLUS_EXPRESSION, REAL_PLUS_EXPRESSION,
  INTEGER_MINUS_EXPRESSION, REAL_MINUS_EXPRESSION,
  INTEGER_TO_REAL_EXPRESSION,
  INTEGER_ADD_EXPRESSION, REAL_ADD_EXPRESSION,
  INTEGER_SUBTRACT_EXPRESSION, REAL_SUBTRACT_EXPRESSION,
  INTEGER_MULTIPLY_EXPRESSION, REAL_MULTIPLY_EXPRESSION,
  INTEGER_DIVIDE_EXPRESSION, INTEGER_MODULUS_EXPRESSION,
  REAL_DIVIDE_EXPRESSION,
  OR_EXPRESSION, AND_EXPRESSION, NOT_EXPRESSION,
  INDEX_EXPRESSION, IN_EXPRESSION,
  SET_CONSTRUCTOR, SET_EXPRESSION, SET_RANGE_EXPRESSION,
  SET_UNION_EXPRESSION, SET_INTERSECTION_EXPRESSION,
  SET_DIFFERENCE_EXPRESSION, EMPTY_SET_EXPRESSION,
  FIELD_EXPRESSION, INDIRECT_EXPRESSION, FILE_ACCESS,
  INTEGER_ABS_EXPRESSION, REAL_ABS_EXPRESSION,
  INTEGER_SQR_EXPRESSION, REAL_SQR_EXPRESSION,
  SQRT_EXPRESSION,  EXP_EXPRESSION, LN_EXPRESSION,
  SIN_EXPRESSION, COS_EXPRESSION, ARCTAN_EXPRESSION,
  TRUNC_EXPRESSION, ROUND_EXPRESSION,
  FUNCTION_CALL, ODD_EXPRESSION, ORD_EXPRESSION, CHR_EXPRESSION,
  SUCC_EXPRESSION, PRED_EXPRESSION,
  EOF_EXPRESSION, EOLN_EXPRESSION,
  ARGC_EXPRESSION,
  PROCEDURAL_PARAMETER_EXPRESSION,
  FUNCTIONAL_PARAMETER_EXPRESSION,
} Expression_Class;

typedef struct Expression {
  Expression_Class class;
  Type *type;
  union {
    Constant *constant; /* CONSTANT_EXPRESSION */
    Symbol *variable;  /* VARIABLE_EXPRESSION, FUNCTION_PARAMETER_EXPRESSION */
    struct Expression *val;  /* unary operators */
    struct {
      struct Expression *lval;  /* binary operators */
      struct Expression *rval;
    };
    Call_Expression call;
    struct Expression_List *set;
    Field_Expression field;
  };
} Expression;

typedef struct Expression_List {
  Expression *expr;
  struct Expression_List *next;
} Expression_List;

typedef struct Symbol_List {
  struct Symbol_List *next;
  Symbol *symbol;
  int level;
} Symbol_List;

typedef struct Assignment_Statement {
  Expression *lval;
  Expression *rval;
} Assignment_Statement;

typedef struct If_Statement {
  Expression *test;
  struct Statement *tstmt;
  struct Statement *fstmt;
} If_Statement;

typedef struct Case_Statement {
  Expression *index;
  struct Case_Element_List *elements;
} Case_Statement;

typedef struct Case_Element_List {
  int label;
  Constant_List *constants;
  struct Statement *statement;
  struct Case_Element_List *next;
} Case_Element_List;

typedef struct While_Statement {
  Expression *test;
  struct Statement *body;
} While_Statement;

typedef struct Repeat_Statement {
  struct Statement_Sequence *body;
  Expression *test;
} Repeat_Statement;

typedef struct For_Statement {
  Symbol *temp1;
  Symbol *temp2;
  Symbol *control_variable;
  Expression *initial_value;
  Expression *final_value;
  struct Statement *body;
  For_Mode mode;
} For_Statement;

typedef struct With_Statement {
  Expression *record;
  Symbol *variable;
  struct Statement *body;
} With_Statement;

typedef struct Pack_Statement {
  Expression *a;
  Expression *i;
  Expression *z;
} Pack_Statement;

typedef struct Read_Statement {
  Symbol *file;
  Expression *expression;
} Read_Statement;

typedef struct Write_Statement {
  Symbol *file;
  Expression *expression;
  Expression *field_width;
  Expression *fractional_digits;
} Write_Statement;

typedef struct Reset_Rewrite_Statement {
  Expression *name;
  Expression *file;
} Reset_Rewrite_Statement;

typedef struct Argv_Statement {
  Expression *index;
  Expression *arg;
} Argv_Statement;

typedef enum {
  EMPTY_STATEMENT, COMPOUND_STATEMENT,
  ASSIGNMENT_STATEMENT, GOTO_STATEMENT,
  IF_STATEMENT, CASE_STATEMENT,
  WHILE_STATEMENT, REPEAT_STATEMENT,
  FOR_STATEMENT, WITH_STATEMENT,  PROCEDURE_CALL,
  RESET_STATEMENT, REWRITE_STATEMENT,
  GET_STATEMENT, PUT_STATEMENT,
  READ_STATEMENT, READ_TEXT_STATEMENT, READLN_STATEMENT,
  WRITE_STATEMENT, WRITE_TEXT_STATEMENT, WRITELN_STATEMENT,
  NEW_STATEMENT, DISPOSE_STATEMENT,
  PAGE_STATEMENT,
  PACK_STATEMENT, UNPACK_STATEMENT,
  ARGV_STATEMENT, FLUSH_STATEMENT, CLOSE_STATEMENT,
} Statement_Class;

typedef struct Statement {
  Statement_Class class;
  Symbol *label;
  union {
    struct Statement_Sequence *compound_statement;
    Assignment_Statement assignment;
    Symbol *gotostmt;
    If_Statement ifstmt;
    Case_Statement casestmt;
    While_Statement whilestmt;
    Repeat_Statement repeatstmt;
    For_Statement forstmt;
    With_Statement with;
    Reset_Rewrite_Statement reset_rewrite;
    Read_Statement read;
    Write_Statement write;
    Symbol *writeln;
    Symbol *readln;
    Call_Expression call;
    Argv_Statement argv;
    Pack_Statement pack;  /* pack and unpack procedures */
    Expression *parameter; /* used for single parameter standard procedures */
  };
} Statement;

typedef struct Statement_Sequence {
  Statement *statement;
  struct Statement_Sequence *next;
} Statement_Sequence;

typedef struct Undefined_Pointer_List {
  char *id;
  Type *type;
  struct Undefined_Pointer_List *next;
} Undefined_Pointer_List;

/* pascal.c */

extern char *input_name;
extern FILE *input;
extern FILE *output;

/* symbols.c */

extern Type *boolean_type;
extern Type *char_type;
extern Type *integer_type;
extern Type *real_type;
extern Type *text_type;
extern Type *empty_set_type;
extern Type *nil_type;
extern Constant *nil_constant;

void initialize_symbols(void);

void push_symbol_table(void);
void pop_symbol_table(void);

Symbol *new_symbol(char *, Symbol_Class);
Symbol *new_program_symbol(char *, Symbol_List *);
Symbol *new_procedure_symbol(char *, Symbol_List *);
Symbol *new_function_symbol(char *, Symbol_List *, Type *);
Symbol *new_parameter_symbol(char *, Symbol_Class);
Symbol *new_procedure_parameter_symbol(char *, Symbol_List *);
Symbol *new_function_parameter_symbol(char *, Symbol_List *, Type *);
Symbol *new_constant_symbol(char *, Constant *);
Symbol *new_type_symbol(char *, Type *);
Symbol *new_field_symbol(char *);

void insert_symbols(Symbol_List *);
void insert_symbol(Symbol *);
void insert_variable_symbols(Identifier_List *, Type *);
void insert_record_fields(Type *);

Symbol *lookup_symbol(char *);
Symbol *lookup_symbol_local(char *);

void install_variable_symbols(Identifier_List *, Type *);

/* source.c */

extern int line_number;
extern char *token;
extern Token_Type token_type;

bool check(Token_Type);
void need(Token_Type);
bool match(Token_Type);

void next_token(void);

/* constants.c */

Constant *constant(void);
Constant *unsigned_constant(void);
Constant_List *constant_list(void);
Constant *new_ordinal_constant(Type *, Ordinal);
Constant *new_real_constant(double);
Constant *new_string_constant(char *);
char *fix_string(char *);

/* types.c */

extern Undefined_Pointer_List *undefined_pointer_list;

Type *type_identifier(void);
Type *result_type(void);
Type *type_denoter(void);
void define_undefined_pointer(char *, Type *);
Type *new_type(Type_Class);
Type *new_ordinal_type(Type *, Ordinal, Ordinal);
Type *new_real_type(void);
Type *new_file_type(bool, Type *);
Type *new_pointer_type(Type *);
Type *new_undefined_pointer_type(char *);
Type *new_set_type(bool, Type *);
Type *new_record_type(Symbol_List *, Variant_Part *);

/* parse.c */

Symbol *parse(void);
char *identifier(void);
Identifier_List *identifier_list(void);
char *label(void);

/* statements.c */

Statement *compound_statement(Symbol *);

/* expressions.c */


Expression *expression(void);
Expression *variable_access(void);
Expression_List *actual_parameter_list(Symbol_List *);

/* semantics.c */

extern int block_level;
extern long variable_offset;
extern long parameter_offset;

extern Symbol_List *with_symbol_stack;

bool is_integer(Type *);
bool is_char(Type *);
bool is_boolean(Type *);
bool is_real(Type *);
bool is_set(Type *);
bool is_file(Type *);
bool is_string(Type *);

Symbol *new_symbol(char *, Symbol_Class);
Symbol *new_variable_symbol(char *, Type *);
Symbol *new_label_symbol(char *, Symbol *);
Type *new_subrange_type(Constant *, Constant *);

Symbol *find_with_variable(Type *);

Statement *new_statement(Statement_Class, Symbol *);
Statement *new_assignment_statement(Expression *, Expression *, Symbol *);
Statement *new_procedure_call(Symbol *, Expression_List *, Symbol *);
Statement *new_reset_statement(Expression *, Expression *, Symbol *);
Statement *new_rewrite_statement(Expression *, Expression *, Symbol *);
Statement *new_get_statement(Expression *, Symbol *);
Statement *new_put_statement(Expression *, Symbol *);
Statement *new_read_statement(Symbol *, Expression *);
Statement *new_read_text_statement(Symbol *, Expression *);
Statement *new_readln_statement(Symbol *);
Statement *new_write_statement(Symbol *, Expression *);
Statement *new_write_text_statement(Symbol *, Expression *, Expression *, Expression *);
Statement *new_writeln_statement(Symbol *);
Statement *new_argv_statement(Expression *, Expression *, Symbol *);
Expression *new_expression(Expression_Class, Type *);
Expression *new_constant_expression(Constant *);
Expression *new_variable_expression(Symbol *);
Expression *new_eq_expression(Expression *, Expression *);
Expression *new_ne_expression(Expression *, Expression *);
Expression *new_lt_expression(Expression *, Expression *);
Expression *new_le_expression(Expression *, Expression *);
Expression *new_gt_expression(Expression *, Expression *);
Expression *new_ge_expression(Expression *, Expression *);
Expression *new_in_expression(Expression *, Expression *);
Expression *new_plus_expression(Expression *);
Expression *new_minus_expression(Expression *);
Expression *new_add_expression(Expression *, Expression *);
Expression *new_subtract_expression(Expression *, Expression *);
Expression *new_or_expression(Expression *, Expression *);
Expression *new_multiply_expression(Expression *, Expression *);
Expression *new_divide_expression(Expression *, Expression *);
Expression *new_div_expression(Expression *, Expression *);
Expression *new_mod_expression(Expression *, Expression *);
Expression *new_and_expression(Expression *, Expression *);
Expression *new_not_expression(Expression *);
Expression *new_index_expression(Expression *, Expression *);
Expression *new_field_designator_expression(Expression *, Symbol *);
Symbol *find_field(Type *, char *);
Expression *new_indirect_expression(Expression *);
Expression *new_abs_function(Expression *);
Expression *new_sqr_function(Expression *);
Expression *new_sqrt_function(Expression *);
Expression *new_sin_function(Expression *);
Expression *new_cos_function(Expression *);
Expression *new_arctan_function(Expression *);
Expression *new_exp_function(Expression *);
Expression *new_ln_function(Expression *);
Expression *new_trunc_function(Expression *);
Expression *new_round_function(Expression *);
Expression *new_odd_function(Expression *);
Expression *new_ord_function(Expression *);
Expression *new_succ_function(Expression *);
Expression *new_pred_function(Expression *);
Expression *new_chr_function(Expression *);
Expression *new_eof_function(Expression *);
Expression *new_eoln_function(Expression *);
Expression *new_argc_function(void);
Expression *new_function_call(Symbol *, Expression_List *);
Expression *new_empty_set(void);
Expression *new_set_constructor(Expression_List *);
Expression *new_set_expression(Expression *);
Expression *new_set_range_expression(Expression *, Expression *);
Expression *new_procedural_parameter_expression(Symbol *, Symbol *);
Expression *new_functional_parameter_expression(Symbol *, Symbol *);
Symbol *create_tempory_file_variable(void);
Symbol *create_tempory_with_variable(Type *);

void push_for_variable_stack(void);
void pop_for_variable_stack(void);
Symbol *create_for_variable(int);

long align_up(long, long);
long align_down(long, long);

/* error.c */

extern int error_count;

void syntax_error(int);

void warning(char *, ...);
void error(char *, ...);
void fatal_error(char *, ...);

void x_internal_error(char *, int, char const *);
void x_undefined(char *, int, char const *);
