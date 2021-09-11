#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include "machine.h"
#include "pascal.h"

#define KEYWORDS (WITH_TOKEN+1)
#define TOKEN_DELTA 16

int line_number = 1; /* start counting lines at 1 */

char *token;
Token_Type token_type;
int token_index;
int token_allocated;

int char_stack[4]; /* 4 should be more than enough */
int char_index;

char *token_name[] = {
  "and", "array", "begin", "case", "const", "div",
  "do", "downto", "else", "end", "file", "for",
  "function", "goto", "if", "in", "label", "mod",
  "nil", "not", "of", "or", "packed", "procedure",
  "program", "record", "repeat", "set", "then",
  "to", "type", "until", "var", "while", "with",
  "+", "-", "*", "/",
  "=", "<>", "<", "<=", ">", ">=",
  ".", ",", ":", ";",
  "(", ")", "[", "]",
  ":=", "^", "..",
  "identifier", "integer", "real", "string",
  "end of file"
};

void white_space(void);
void comment(void);
void word_symbol(void);
void number_symbol(void);
void digits(void);
void string_symbol(void);
void character_symbol(void);
Token_Type follow(char *, ...);

void token_reset(void);
void token_add(int);
void token_expand(void);

int peek_char(void);
int next_char(void);
void back_char(int);

bool check(Token_Type tt) {
  return token_type == tt;
}

void need(Token_Type tt) {
  if (check(tt))
    next_token();
  else
    error("missing %s near %s", token_name[tt], token);
}

bool match(Token_Type tt) {
  if (check(tt)) {
    next_token();
    return true;
  }
  return false;
}
    

void next_token() {
  int c;

  token_reset();
  white_space();
  c = peek_char();
  if (c == EOF)
    token_type = END_OF_FILE_TOKEN;
  else if (isalpha(c))
    word_symbol();
  else if (isdigit(c))
    number_symbol();
  else if (c == '\'')
    string_symbol();
  else
    character_symbol();
}

void white_space() {
  int c;

  for (;;) {
    c = next_char();
    if (c == EOF)
      return;
    if (c == '{')
      comment();
    else if (c == '(') {
      c = next_char();
      if (c == '*')
	comment();
      else {
	back_char(c);
	back_char('(');
	return;
      }
    }
    else if (isgraph(c)) { /* treat all control characters as white-space */
      back_char(c);
      return;
    }
    else if (c == '\n')
      line_number++;
  }
}

void comment() {
  int c;
  int save_line_number = line_number;
  int temp;
  
  c = next_char();
  for (;;) {
    if (c == EOF) {
      temp = line_number;
      line_number = save_line_number;
      error("non-terminated comment");
      line_number = temp;
      return;
    }
    if (c == '}')
      return;
    if (c == '*') {
      do
	c = next_char();
      while (c == '*');
      if (c == ')')
	return;
    }
    else {
      if (c == '\n')
	line_number++;
      c = next_char();
    }
  }
}

void word_symbol() {
  int low, mid, high;
  int test, c;

  for (;;) {
    c = next_char();
    if (!isalnum(c))
      break;
    token_add(tolower(c));
  }
  back_char(c);

  token_type = IDENTIFIER_TOKEN;
  low = 0;
  high = KEYWORDS;
  while (low < high) {
    mid = (high + low)/2;
    test = strcmp(token, token_name[mid]);
    if (test < 0)
      high = mid;
    else if (test > 0)
      low = mid+1;
    else {
      token_type = mid;
      break;
    }
  }
}

void number_symbol() {
  int sign;

  token_type = INTEGER_TOKEN;
  digits();
  if (peek_char() == '.') {
    next_char();
    if (!isdigit(peek_char())) {
      back_char('.');
      return;
    }
    token_type = REAL_TOKEN;
    token_add('.');
    digits();
  }
  if (tolower(peek_char()) == 'e') {
    next_char();
    sign = peek_char();
    if (sign == '+' || sign == '-')
      next_char();
    else
      sign = 0;
    if (!isdigit(peek_char())) { /* Uh Oh */
      if (sign)
	back_char(sign);
      back_char('e');
      return;
    }
    token_type = REAL_TOKEN;
    token_add('e');
    if (sign)
      token_add(sign);
    digits();
  }
}

void digits() {
  int c;

  for (;;) {
    c = next_char();
    if (isdigit(c))
      token_add(c);
    else {
      back_char(c);
      return;
    }
  }
}
  
void string_symbol() {
  int c;

  token_type = STRING_TOKEN;
  c = next_char();
  do {
    do {
      token_add(c);
      c = next_char();
      if (c == '\n' || c == EOF) {
	error("missing quote");
	line_number++;
	token_add('\'');
	return;
      }
    } while (c != '\'');
    token_add(c);
    c = next_char();
  } while (c == '\'');
  back_char(c);
}

void character_symbol() {
  int c;

  c = next_char();
  token_add(c);
  switch (c) {
  case '+':
    token_type = PLUS_TOKEN;
    break;
  case '-':
    token_type = MINUS_TOKEN;
    break;
  case '*':
    token_type = MULTIPLY_TOKEN;
    break;
  case '/':
    token_type = DIVIDE_TOKEN;
    break;
  case '=':
    token_type = EQ_TOKEN;
    break;
  case '<':
    token_type = follow(">=", NE_TOKEN, LE_TOKEN, LT_TOKEN);
    break;
  case '>':
    token_type = follow("=", GE_TOKEN, GT_TOKEN);
    break;
  case '.':
    token_type = follow(".)", ELLIPSIS_TOKEN, RBRACK_TOKEN, PERIOD_TOKEN);
    break;
  case ',':
    token_type = COMMA_TOKEN;
    break;
  case ':':
    token_type = follow("=", ASSIGN_TOKEN, COLON_TOKEN);
    break;
  case ';':
    token_type = SEMICOLON_TOKEN;
    break;
  case '(':
    token_type = follow(".", LBRACK_TOKEN, LPAREN_TOKEN);
    break;
  case ')':
    token_type = RPAREN_TOKEN;
    break;
  case '[':
    token_type = LBRACK_TOKEN;
    break;
  case ']':
    token_type = RBRACK_TOKEN;
    break;
  case '^':
  case '@':
    token_type = ARROW_TOKEN;
    break;
  default:
    error("illegal character: '%c'", c);
    next_token();
    break;
  }
}

Token_Type follow(char *s, ...) {
  Token_Type tt;
  va_list args;
  int c;

  c = next_char();
  va_start(args, s);
  while (*s) {
    tt = va_arg(args, Token_Type);
    if (*s++ == c) {
      va_end(args);
      return tt;
    }
  }
  back_char(c);
  tt = va_arg(args, Token_Type);
  va_end(args);
  return tt;
}

void token_reset() {
  if (token == 0)
    token_expand();
  token[0] = '\0';
  token_index = 0;
}

void token_add(int c) {
  if (token_index >= token_allocated-1)
    token_expand();
  token[token_index++] = c;
  token[token_index] = '\0';
}

void token_expand() {
  token_allocated += TOKEN_DELTA;
  token = realloc(token, token_allocated);
}

int peek_char() {
  if (char_index)
    return char_stack[char_index-1];
  return char_stack[char_index++] = getc(input);
}

int next_char() {
  if (char_index)
    return char_stack[--char_index];
  return getc(input);
}

void back_char(int c) {
  char_stack[char_index++] = c;
}

