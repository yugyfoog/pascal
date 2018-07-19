#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include "pascal.h"

#define TOKEN_DELTA 16
#define KEYWORDS ((int)WITH_TOKEN + 1)

int line_number = 1;

char *token;
Token_Type token_type;
int token_size;
int token_index;

int char_stack[4];
int char_index;

char *token_name[] = {
  "and", "array", "begin", "case", "const", "div",
  "do", "downto", "else", "end", "file", "for",
  "function", "goto", "if", "in", "label", "mod",
  "nil", "not", "of", "or", "packed", "procedure",
  "program", "record", "repeat", "set", "then",
  "to", "type", "until", "var", "while", "with",
  "+", "-", "*", "/",
  ".", ",", ":", ";",
  "=", "<>", "<", "<=", ">", ">=",
  "(", ")", "[", "]",
  ":=", "..", "^",
  "integer constant", "real constant", "string constant",
  "identifier", "end of file"
};

void token_reset(void);
void expand_token(void);
void token_add(int);
void white_space(void);
void comment(void);
void identifier_or_keyword(void);
void number_symbol(void);
void string_symbol(void);
void character_symbol(void);
Token_Type follow(char *, ...);
int peek_char(void);
int next_char(void);
void back_char(int);

void next_token() {
  int c;

  token_reset();
  white_space();
  c = peek_char();
  if (c == EOF)
    token_type = END_OF_FILE_TOKEN;
  else if (isalpha(c))
    identifier_or_keyword();
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
    else if (!isspace(c)) {
      back_char(c);
      return;
    }
    if (c == '\n')
      line_number++;
  }
}

void comment() {
  int c;

  for (;;) {
    c = next_char();
    if (c == '*') {
      do
	c = next_char();
      while (c == '*');
      if (c == ')')
	return;
    }
    if (c == '}')
      return;
    if (c == EOF)
      fatal_error("missing end of comment");
    if (c == '\n')
      line_number++;
  }
}	

void identifier_or_keyword() {
  int high, low, mid;
  int t, c;

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
    mid = (low + high)/2;
    t = strcmp(token, token_name[mid]);
    if (t < 0)
      high = mid;
    else if (t > 0)
      low = mid + 1;
    else {
      token_type = mid;
      return;
    }
  }
}

void number_symbol() {
  int c, s;

  token_type = INTEGER_TOKEN;
  for (;;) {
    c = next_char();
    if (!isdigit(c))
      break;
    token_add(c);
  }
  if (c == '.') {
    c = next_char();
    if (!isdigit(c)) {
      back_char(c);
      back_char('.');
      return;
    }
    token_type = REAL_TOKEN;
    token_add('.');
    do {
      token_add(c);
      c = next_char();
    } while (isdigit(c));
  }
  if (c == 'E' || c == 'e') {
    c = next_char();
    if (c == '+') {
      s = 1;
      c = next_char();
    }
    else if (c == '-') {
      s = -1;
      c = next_char();
    }
    if (!isdigit(c)) {
      back_char(c);
      if (s == 1)
	back_char('+');
      else if (s == -1)
	back_char('-');
      back_char('e');
      return;
    }
    token_add('e');
    if (s == -1)
      token_add('-');
    token_type = REAL_TOKEN;
    do {
      token_add(c);
      c = next_char();
    } while (isdigit(c));
  }
  back_char(c);
}

void string_symbol() {
  int c;

  token_type = STRING_TOKEN;
  c = next_char();
  token_add(c);
  for (;;) {
    c = next_char();
    token_add(c);
    if (c == '\'') {
      c = next_char();
      if (c == '\'')
	token_add(c);
      else {
	back_char(c);
	return;
      }
    }
    else if (c == '\n' || c == EOF) {
      error("missing quote");
      token_add('\'');
      back_char(c);
      return;
    }
  }
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
    token_type = follow("=>", LE_TOKEN, NE_TOKEN, LT_TOKEN);
    break;
  case '>':
    token_type = follow("=", GE_TOKEN, GT_TOKEN);
    break;
  case '[':
    token_type = LBRACK_TOKEN;
    break;
  case ']':
    token_type = RBRACK_TOKEN;
    break;
  case '.':
    token_type = follow(").", RBRACK_TOKEN, ELLIPSIS_TOKEN, PERIOD_TOKEN);
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
  case '^':
  case '@':
    token_type = ARROW_TOKEN;
    break;
  case '(':
    token_type = follow(".", LBRACK_TOKEN, LPAREN_TOKEN);
    break;
  case ')':
    token_type = RPAREN_TOKEN;
    break;
  }
}

Token_Type follow(char *s, ...) {
  va_list args;
  Token_Type arg;
  int c;

  c = next_char();
  va_start(args, s);
  while (*s) {
    arg = va_arg(args, Token_Type);
    if (*s++ == c) {
      token_add(c);
      va_end(args);
      return arg;
    }
  }
  back_char(c);
  arg = va_arg(args, Token_Type);
  va_end(args);
  return arg;
}

void token_reset() {
  if (token_size < 1)
    expand_token();
  token_index = 0;
  token[0] = '\0';
}

void token_add(int c) {
  if (token_size - 1 <= token_index)
    expand_token();
  token[token_index++] = c;
  token[token_index] = '\0';
}

void expand_token() {
  token_size += TOKEN_DELTA;
  token = realloc(token, token_size);
}

int peek_char() {
  if (char_index)
    return char_stack[char_index-1];
  char_index = 1;
  return char_stack[0] = getc(input);
}

int next_char() {
  if (char_index)
    return char_stack[--char_index];
  return getc(input);
}

void back_char(int c) {
  char_stack[char_index++] = c;
}
