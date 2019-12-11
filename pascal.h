#include "machine.h"

#define XXX() x_undefined(__FILE__,__FUNCTION__,__LINE__)

#define MAX_ERRORS 5

#define new(T) ((T *)malloc(sizeof(T)))

typedef enum {false, true} bool;

typedef enum {
  AND_TOKEN, ARRAY_TOKEN, BEGIN_TOKEN, CASE_TOKEN, CONST_TOKEN, DIV_TOKEN,
  DO_TOKEN, DOWNTO_TOKEN, ELSE_TOKEN, END_TOKEN, FILE_TOKEN, FOR_TOKEN,
  FUNCTION_TOKEN, GOTO_TOKEN, IF_TOKEN, IN_TOKEN, LABEL_TOKEN, MOD_TOKEN,
  NIL_TOKEN, NOT_TOKEN, OF_TOKEN, OR_TOKEN, PACKED_TOKEN, PROCEDURE_TOKEN,
  PROGRAM_TOKEN, RECORD_TOKEN, REPEAT_TOKEN, SET_TOKEN, THEN_TOKEN,
  TO_TOKEN, TYPE_TOKEN, UNTIL_TOKEN, VAR_TOKEN, WHILE_TOKEN, WITH_TOKEN,
  PLUS_TOKEN, MINUS_TOKEN, MULTIPLY_TOKEN, DIVIDE_TOKEN,
  PERIOD_TOKEN, COMMA_TOKEN, COLON_TOKEN, SEMICOLON_TOKEN,
  EQ_TOKEN, NE_TOKEN, LT_TOKEN, LE_TOKEN, GT_TOKEN, GE_TOKEN,
  LPAREN_TOKEN, RPAREN_TOKEN, LBRACK_TOKEN, RBRACK_TOKEN,
  ASSIGN_TOKEN, ELLIPSIS_TOKEN, ARROW_TOKEN,
  INTEGER_TOKEN, REAL_TOKEN, CHAR_TOKEN, STRING_TOKEN,
  IDENTIFIER_TOKEN,
  END_OF_FILE_TOKEN
} Token_Type;

typedef enum {
  DISPOSE_PROCEDURE, GET_PROCEDURE, NEW_PROCEDURE, PACK_PROCEDURE,
  PAGE_PROCEDURE, PUT_PROCEDURE, READ_PROCEDURE, READLN_PROCEDURE,
  RESET_PROCEDURE, REWRITE_PROCEDURE, UNPACK_PROCEDURE,
  WRITE_PROCEDURE, WRITELN_PROCEDURE
} Standard_Procedure;

typedef enum {
  ABS_FUNCTION, SQR_FUNCTION, SIN_FUNCTION, COS_FUNCTION,
  EXP_FUNCTION, LN_FUNCTION, SQRT_FUNCTION, ARCTAN_FUNCTION,
  TRUNC_FUNCTION, ROUND_FUNCTION, ORD_FUNCTION, CHR_FUNCTION,
  SUCC_FUNCTION, PRED_FUNCTION, ODD_FUNCTION, EOLN_FUNCTION,
  EOF_FUNCTION
} Standard_Function;

typedef struct Ordinal_Type {
  struct Type *base;
  Ordinal low;
  Ordinal high;
} Ordinal_Type;

typedef struct Array_Type {
  struct Type *index;
  struct Type *component;
} Array_Type;

typedef struct Variant {
  struct Constant_List *cnsts;
  struct Type *fields;
} Variant;

typedef struct Variant_List {
  Variant *variant;
  struct Variant_List *next;
} Variant_List;

typedef struct Variant_Part {
  struct Symbol *selector;
  Variant_List *variants;
} Variant_Part;

typedef struct Record_Type {
  struct Symbol_List *fixed;
  Variant_Part *variant;
} Record_Type;

typedef enum {
  ORDINAL_TYPE, REAL_TYPE,
  ARRAY_TYPE, PACKED_ARRAY_TYPE,
  RECORD_TYPE, PACKED_RECORD_TYPE,
  SET_TYPE, PACKED_SET_TYPE,
  FILE_TYPE, PACKED_FILE_TYPE,
  POINTER_TYPE, TEXT_TYPE,
  NIL_TYPE, FORWARD_TYPE
} Type_Class;

typedef struct Type {
  Type_Class class;
  union {
    Ordinal_Type ordinal;
    Array_Type array;
    Record_Type record;
    struct Type *subtype; /* used for set, file and pointer types */
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
  Constant *constant;
  struct Constant_List *next;
} Constant_List;
  
typedef struct Statement {
  enum {
    EMPTY_STATEMENT, COMPOUND_STATEMENT,
    ASSIGNMENT_STATEMENT, PROCEDURE_STATEMENT,
    GOTO_STATEMENT, IF_STATEMENT, CASE_STATEMENT,
    REPEAT_STATEMENT, WHILE_STATEMENT, FOR_STATEMENT,
    WITH_STATEMENT,
    
    DISPOSE_STATEMENT, GET_STATEMENT, NEW_STATEMENT,
    PACK_STATEMENT, PAGE_STATEMENT, PUT_STATEMENT,
    READ_STATEMENT, READLN_STATEMENT, RESET_STATEMENT,
    REWRITE_STATEMENT, UNPACK_STATEMENT, WRITE_STATEMENT,
    WRITELN_STATEMENT
  } class;
} Statement;

typedef struct Block {
  struct Symbol_List *variables;
  struct Symbol_List *procfuncs;
  struct Statement *stmt;
} Block;

typedef struct Program_Symbol {
  struct Identifier_List *params;
  Block *block;
} Program_Symbol;

typedef struct Variable_Symbol {
  int level;
  Type *type;
} Variable_Symbol;

typedef enum {
  PROGRAM_SYMBOL, PROCEDURE_SYMBOL, FUNCTION_SYMBOL,
  LABEL_SYMBOL, CONSTANT_SYMBOL, TYPE_SYMBOL,
  VARIABLE_SYMBOL, VARARG_SYMBOL, VALARG_SYMBOL,
  PROCARG_SYMBOL, FUNCARG_SYMBOL,
  STDPROC_SYMBOL, STDFUNC_SYMBOL,
  FIELD_SYMBOL,
} Symbol_Class;

typedef struct Symbol {
  Symbol_Class class;
  char *name;
  union {
    Program_Symbol prog;
    struct Constant *cnst;
    struct Type *type;
    Variable_Symbol var;
    Standard_Procedure stdproc;
    Standard_Function stdfunc;
  };
} Symbol;

typedef struct Symbol_List {
  Symbol *sym;
  int level;
  struct Symbol_List *next;
} Symbol_List;

typedef struct Identifier_List {
  char *id;
  struct Identifier_List *next;
} Identifier_List;

/* main.c */

extern char *program_name;
extern char *input_name;
extern char *output_name;
extern FILE *input;
extern FILE *output;

/* symbol.c */

extern Type *char_type;
extern Type *integer_type;
extern Type *real_type;
extern Type *text_type;
extern Type *forward_type;

void initialize_symbols(void);
Symbol *new_program_symbol(char *, Identifier_List *);
Symbol *new_val_param_symbol(char *, Type *);
void push_symbol_table(void);
void pop_symbol_table(void);

void insert(Symbol *);
Symbol *lookup(char *);

/* source.c */

extern int line_number;
extern Token_Type token_type;
extern char *token;

bool check(Token_Type);
bool match(Token_Type);
void need(Token_Type);
void next_token(void);

/* semantics.c */


Symbol *new_constant_symbol(char *, Constant *);
Symbol *new_type_symbol(char *, Type *);
Symbol *new_field_symbol(char *, Type *);
Symbol *new_variable_symbol(char *, Type *);

Constant *new_ordinal_constant(Type *, Ordinal);
Constant *new_real_constant(Real);

Type *new_type(Type_Class);
Type *new_subrange_type(Constant *, Constant *);
Type *new_ordinal_type(Type *, Ordinal, Ordinal);
Type *new_enumerated_type(Identifier_List *);
Type *make_string_type(char *);
Type *new_array_type(Type *, Type *);
Type *make_packed(Type *);
Type *new_set_type(Type *);
Type *new_file_type(Type *);
Type *new_pointer_type(Type *);

/* parse.c */

extern int lexical_level;

Symbol *parse(void);
char *identifier(void);

/* stmts.c */

void compound_statement(void);

/* expr.c */

Constant_List *constant_list(void);
Constant *constant(void);
void expression(void);

/* code.c */

void code_program(Symbol *);

/* error.c */

extern int error_count;

void warning(char *, ...);
void error(char *, ...);
void fatal_error(char *, ...);
void x_undefined(char *, char const *, int);
