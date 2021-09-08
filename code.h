typedef struct Enter_Code {
  long size;
  int level;
} Enter;

typedef struct Variable_Code {
  int level;
  int delta;
  long offset;
} Variable_Code;

typedef struct Jump_Index {
  int label;
  long size;
  int otherwise;
} Jump_Index;

typedef struct Jump_Table {
  int *table;
  long size;
  int otherwise;
} Jump_Table;

typedef struct Interprocedural_Jump {
  long offset;
  long locals;
  char *name;
} Interprocedural_Jump;

typedef enum {
  NOP_OP, LABEL_OP, INTERNAL_LABEL,
  ENTER_OP, LEAVE_OP, RETURN_OP,
  PROCEDURE_CALL_OP, PROCEDURE_CALL_INDIRECT_OP,
  FUNCTION_CALL_OP, FUNCTION_CALL_INDIRECT_OP,
  LOAD_ORDINAL_CONSTANT_OP,
  LOAD_REAL_CONSTANT_OP,
  LOAD_STRING_CONSTANT_OP,
  LOAD_VARIABLE_ADDRESS_OP,
  LOAD_NIL_OP, LOAD_ALGORITHM_OP,
  FETCH_SIGNED_OP, FETCH_UNSIGNED_OP,
  FETCH_REAL_OP, FETCH_SET_OP, FETCH_OP,
  RETURN_SIGNED_OP, RETURN_UNSIGNED_OP,
  RETURN_REAL_OP, RETURN_POINTER_OP,
  STORE_OP, STORE_REAL_OP, STORE_SET_OP,
  MOVE_OP, INTEGER_TO_REAL_OP,
  ORDINAL_EQ_OP, REAL_EQ_OP, STRING_EQ_OP,
  POINTER_EQ_OP, SET_EQ_OP,
  ORDINAL_NE_OP, REAL_NE_OP, STRING_NE_OP,
  POINTER_NE_OP, SET_NE_OP,
  ORDINAL_LT_OP, REAL_LT_OP, STRING_LT_OP,
  ORDINAL_LE_OP, REAL_LE_OP,
  STRING_LE_OP, SET_LE_OP,
  ORDINAL_GT_OP, REAL_GT_OP, STRING_GT_OP,
  ORDINAL_GE_OP, REAL_GE_OP,
  STRING_GE_OP, SET_GE_OP,
  INTEGER_NEGATE_OP,
  INTEGER_ADD_OP, INTEGER_SUBTRACT_OP,
  INTEGER_MULTIPLY_OP, INTEGER_DIVIDE_OP,
  INTEGER_MODULUS_OP,
  REAL_NEGATE_OP,
  REAL_ADD_OP, REAL_SUBTRACT_OP,
  REAL_MULTIPLY_OP, REAL_DIVIDE_OP,
  INTEGER_ABS_OP, REAL_ABS_OP,
  INTEGER_SQR_OP, REAL_SQR_OP,
  SQRT_OP, SIN_OP, COS_OP,
  ARCTAN_OP, EXP_OP, LN_OP,
  TRUNC_OP, ROUND_OP,
  ODD_OP,
  AND_OP, OR_OP, NOT_OP,
  INC_OP, DEC_OP,
  EOF_OP, EOLN_OP,
  JUMP_OP, JUMP_TRUE_OP, JUMP_FALSE_OP,
  JUMP_LABEL_OP, JUMP_INDEX_OP,
  JUMP_TABLE, CASE_ERROR_OP,
  INTERPROCEDURAL_JUMP_OP,
  EMPTY_SET_OP,  TO_SET_OP,
  SET_UNION_OP, SET_INTERSECTION_OP,
  SET_DIFFERENCE_OP,
  TO_SET_RANGE_OP, IN_OP,
  FILE_ACCESS_OP, GET_OP, PUT_OP,
  RESET_OP, NAMED_RESET_OP,
  RESET_TEXT_OP, NAMED_RESET_TEXT_OP,
  REWRITE_OP, NAMED_REWRITE_OP,
  REWRITE_TEXT_OP, NAMED_REWRITE_TEXT_OP,
  READ_CHAR_OP, READ_INTEGER_OP,
  READ_REAL_OP, READLN_OP,
  WRITE_DIRECT_OP, WRITE_INDIRECT_OP, WRITE_SET_OP,
  WRITE_BOOLEAN_OP,
  WRITE_CHAR_OP,
  WRITE_INTEGER_OP,
  WRITE_REAL_FLOAT_OP, WRITE_REAL_FIXED_OP,
  WRITE_STRING_OP,
  WRITELN_OP, PAGE_OP,
  NEW_OP, DISPOSE_OP,
  ARGC_OP, ARGV_OP,
  FLUSH_OP, CLOSE_OP,
} Operation;

typedef struct Code {
  struct Code *next;
  struct Code *prev;
  Operation op;
  union {
    long value;
    double rval;
    char *strval;
    unsigned long set;
    Enter enter;
    Variable_Code var;
    int jump;  /* label to jump to */
    char *name; /* name of functions or goto labels */
    Jump_Index jump_index;
    Jump_Table jump_table;
    Interprocedural_Jump inter;
    int ilabel; /* internal (compiler generated) labels */
  };
  
} Code;


void code_program(Symbol *);
