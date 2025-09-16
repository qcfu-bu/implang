#include "parsing.h"
#include <cctype>
#include <ctype.h>

namespace implang {
namespace lexer {

int Lexer::int_literal() { return std::stoi(buffer.substr(begin, len)); }
std::string Lexer::identifier() { return buffer.substr(begin, len); }

Token Lexer::next_token() {
  if (peek_cached) {
    peek_cached = false;
    pos = peek_pos;
    return peek_tok;
  }

  while (has_char() && isspace(peek_char())) {
    next_pos();
  }

  tok_begin();
  if (!has_char()) {
    tok_end();
    return LEXING_END;
  }

  switch (peek_char()) {
  case '(':
    next_pos();
    tok_end();
    return LPAREN;
  case ')':
    next_pos();
    tok_end();
    return RPAREN;
  case '{':
    next_pos();
    tok_end();
    return LBRACE;
  case '}':
    next_pos();
    tok_end();
    return RBRACE;
  case '+':
    next_pos();
    tok_end();
    return ADD;
  case '-':
    next_pos();
    if (has_char() && peek_char() == '>') {
      next_pos();
      tok_end();
      return ARROW;
    } else {
      tok_end();
      return SUB;
    }
  case '*':
    next_pos();
    tok_end();
    return MUL;
  case '/':
    next_pos();
    // skip single line comment
    if (has_char() && peek_char() == '/') {
      while (has_char() && peek_char() != '\n') {
        next_pos();
      }
      next_pos();
      return next_token();
    }
    // skip multi-line comment
    if (has_char() && peek_char() == '*') {
      next_pos();
      while (has_char()) {
        if (peek_char() == '*') {
          next_pos();
          if (has_char() && peek_char() == '/') {
            next_pos();
            return next_token();
          }
        } else {
          next_pos();
        }
      }
      throw LexingException(begin);
    }
    // otherwise it's a division operator
    tok_end();
    return DIV;
  case '%':
    next_pos();
    tok_end();
    return MOD;
  case '.':
    next_pos();
    tok_end();
    return DOT;
  case ';':
    next_pos();
    tok_end();
    return SEMI;
  case ':':
    next_pos();
    tok_end();
    return COLON;
  case ',':
    next_pos();
    tok_end();
    return COMMA;
  case '&':
    next_pos();
    if (has_char() && peek_char() == '&') {
      next_pos();
      tok_end();
      return AND;
    }
    throw LexingException(begin);
  case '|':
    next_pos();
    if (has_char() && peek_char() == '|') {
      next_pos();
      tok_end();
      return OR;
    }
    throw LexingException(begin);
  case '<':
    next_pos();
    if (has_char() && peek_char() == '=') {
      next_pos();
      tok_end();
      return LE;
    } else {
      tok_end();
      return LT;
    }
  case '>':
    next_pos();
    if (has_char() && peek_char() == '=') {
      next_pos();
      tok_end();
      return GE;
    } else {
      tok_end();
      return GT;
    }
  case '=':
    next_pos();
    if (has_char() && peek_char() == '=') {
      next_pos();
      tok_end();
      return EQ;
    } else {
      tok_end();
      return ASSIGN;
    }
  case '!':
    if (has_char() && peek_char() == '=') {
      next_pos();
      tok_end();
      return NE;
    } else {
      tok_end();
      return NOT;
    }
  }

  if (isnumber(peek_char())) {
    do {
      next_pos();
    } while (has_char() && isnumber(peek_char()));
    tok_end();
    return INT_LITERAL;
  }

  if (isalpha(peek_char()) || peek_char() == '_') {
    do {
      next_pos();
    } while (has_char() && (isalnum(peek_char()) || peek_char() == '_'));
    tok_end();
    std::string str = buffer.substr(begin, len);
    if (str == "void")
      return VOID;
    if (str == "bool")
      return BOOL;
    if (str == "int")
      return INT;
    if (str == "true")
      return TRUE;
    if (str == "false")
      return FALSE;
    if (str == "let")
      return LET;
    if (str == "var")
      return VAR;
    if (str == "if")
      return IF;
    if (str == "else")
      return ELSE;
    if (str == "for")
      return FOR;
    if (str == "while")
      return WHILE;
    if (str == "continue")
      return CONTINUE;
    if (str == "break")
      return BREAK;
    if (str == "return")
      return RETURN;
    if (str == "extern")
      return EXTERN;
    if (str == "func")
      return FUNC;
    return IDENTIFIER;
  };

  throw LexingException(pos);
}

Token Lexer::peek_token() {
  if (peek_cached) {
    return peek_tok;
  }
  int saved_pos = pos;
  Token tok = next_token();
  // cache the peeked token
  peek_cached = true;
  peek_tok = tok;
  peek_pos = pos;
  // restore the lexing state
  pos = saved_pos;
  return tok;
}

} // namespace lexer
} // namespace implang