#pragma once
#include "../language/language.h"
#include <exception>
#include <map>
#include <string>

/******************************************************************************/
// Lexing

namespace implang {
namespace lexer {

enum Token {
  //
  LEXING_END,
  // delimiters
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  // types
  VOID,
  BOOL,
  INT,
  ARROW,
  // literals
  INT_LITERAL,
  // operators
  NOT,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  AND,
  OR,
  LT,
  GT,
  LE,
  GE,
  EQ,
  NE,
  // separators
  DOT,
  ASSIGN,
  SEMI,
  COLON,
  COMMA,
  // keywords
  TRUE,
  FALSE,
  LET,
  VAR,
  IF,
  ELSE,
  FOR,
  WHILE,
  CONTINUE,
  BREAK,
  RETURN,
  EXTERN,
  FUNC,
  // identifier
  IDENTIFIER
};

class LexingException : public std::exception {
public:
  explicit LexingException(const int pos)
      : msg("lexing error at: " + std::to_string(pos)) {}

  const char *what() const noexcept override { return msg.c_str(); }

private:
  std::string msg;
};

class Lexer {
public:
  Lexer(std::string buffer) : buffer(buffer), pos(0), begin(0), len(0) {}

  int int_literal();
  std::string identifier();

  bool has_char() { return pos < buffer.length(); }
  int get_pos() { return pos; }
  Token next_token();
  Token peek_token();

private:
  // lexing buffer
  int pos, begin, len;
  std::string buffer;

  // caching for peek_token
  bool peek_cached = false;
  Token peek_tok;
  int peek_pos;

  void tok_begin() { begin = pos; }
  void tok_end() { len = pos - begin; }

  void next_pos() { pos++; }
  char peek_char() { return buffer[pos]; }
};

} // namespace lexer

/******************************************************************************/
// Parsing

namespace parser {

class ParsingException : public std::exception {
public:
  explicit ParsingException(int pos)
      : msg("parsing error at: " + std::to_string(pos)) {}

  explicit ParsingException(std::string msg) : msg(msg) {}

  const char *what() const noexcept override { return msg.c_str(); }

private:
  std::string msg;
};

class Parser {
public:
  Parser(lexer::Lexer &lexer) : lexer(lexer) {}

  Expr parse_Expr() { return parse_BinaryOpExpr6(); };
  Cmd parse_Cmd();
  Decl parse_Decl();
  Prog parse_Prog();

private:
  lexer::Lexer &lexer;
  std::vector<std::map<std::string, Id>> scope;
  int loop_depth = 0;

  Id find_id(std::string);

  void expect_token(lexer::Token tok) {
    if (lexer.next_token() != tok) {
      std::string tok_str = std::to_string((int)tok);
      std::string pos_str = std::to_string(lexer.get_pos());
      throw ParsingException("expected token (" + tok_str + ") at: " + pos_str);
    }
  }

  Type parse_AtomType();
  Type parse_TupleType();
  Type parse_Type();

  Expr parse_FuncExpr();      // functions
  Expr parse_TupleExpr();     // tuples
  Expr parse_AtomExpr();      // atoms
  Expr parse_AppExpr();       // application
  Expr parse_PrjExpr();       // projection
  Expr parse_UnaryOpExpr();   // unary
  Expr parse_BinaryOpExpr0(); // *, /, %
  Expr parse_BinaryOpExpr1(); // +, -
  Expr parse_BinaryOpExpr2(); // <, >, <=, >=
  Expr parse_BinaryOpExpr3(); // ==, !=
  Expr parse_BinaryOpExpr4(); // &&
  Expr parse_BinaryOpExpr5(); // ||
  Expr parse_BinaryOpExpr6(); // =

  Cmd parse_LetCmd();
  Cmd parse_VarCmd();
  Cmd parse_FuncCmd();
  Cmd parse_BlockCmd();
  Cmd parse_IteCmd();
  Cmd parse_ForCmd();
  Cmd parse_WhileCmd();
  Cmd parse_ContinueCmd();
  Cmd parse_BreakCmd();
  Cmd parse_ReturnCmd();

  Decl parse_ExternDecl();
  Decl parse_FuncDecl();
  Decl parse_LetDecl();
  Decl parse_VarDecl();
};

} // namespace parser
} // namespace implang