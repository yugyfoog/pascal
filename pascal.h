#define XXX() x_undefined(__FILE__,__FUNCTION__,__LINE__)

#define MAX_ERRORS 5

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
  INTEGER_TOKEN, REAL_TOKEN, STRING_TOKEN,
  IDENTIFIER_TOKEN,
  END_OF_FILE_TOKEN
} Token_Type;

typedef struct Symbol {
  enum {
    PROGRAM_SYMBOL, PROCEDURE_SYMBOL, FUNCTION_SYMBOL,
    LABEL_SYMBOL, CONSTANT_SYMBOL, TYPE_SYMBOL,
    VARIABE_SYMBOL, VARARG_SYMBOL, VALARG_SYMBOL,
    PROCARG_SYMBOL, FUNCARG_SYMBOL,
    STDPROC_SYMBOL, STDFUNC_SYMBOL,
    VARIANT_SELECTOR_SYMBOL
  } class;
} Symbol;

/* main.c */

extern char *program_name;
extern char *input_name;
extern char *output_name;
extern FILE *input;
extern FILE *output;

/* source.c */

extern int line_number;
extern Token_Type token_type;
extern char *token;

void next_token(void);

/* parse.c */

Symbol *parse(void);

/* code.c */

void code_program(Symbol *);

/* error.c */

extern int error_count;

void warning(char *, ...);
void error(char *, ...);
void fatal_error(char *, ...);
void x_undefined(char *, char const *, int);
