#include "parsing.h"
#include <memory>
#include <optional>
#include <string>
#include <utility>

namespace implang {
namespace parser {

/******************************************************************************/
// Scoping

Id Parser::find_id(std::string name) {
  for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
    if (it->find(name) != it->end()) {
      return it->at(name);
    }
  }
  throw ParsingException("cannot find identifier: " + name);
}

/******************************************************************************/
// Types

Type Parser::parse_TupleType() {
  expect_token(lexer::LPAREN);
  std::vector<Type> types;
  if (lexer.peek_token() != lexer::RPAREN) {
    types.push_back(parse_Type());
    while (lexer.peek_token() == lexer::COMMA) {
      lexer.next_token(); // consume ','
      types.push_back(parse_Type());
    }
  }
  expect_token(lexer::RPAREN);
  // function type
  if (lexer.peek_token() == lexer::ARROW) {
    lexer.next_token(); // consume '->'
    Type out_t = parse_Type();
    return FuncType{types, std::make_shared<Type>(out_t)};
  }
  // tuple type
  if (types.size() == 0) {
    throw ParsingException("tuple must have at least 1 element");
  }
  if (types.size() == 1) {
    return std::move(types[0]);
  }
  return TupleType{std::move(types)};
}

Type Parser::parse_AtomType() {
  auto tok = lexer.peek_token();
  switch (tok) {
  case lexer::VOID:
    lexer.next_token(); // consume 'void'
    return ScalarType::VOID;
  case lexer::BOOL:
    lexer.next_token(); // consume 'bool'
    return ScalarType::BOOL;
  case lexer::INT:
    lexer.next_token(); // consume 'int'
    return ScalarType::INT;
  case lexer::LPAREN:
    return parse_TupleType();
  default:
    throw ParsingException(lexer.get_pos());
  }
}

Type Parser::parse_Type() {
  Type t = parse_AtomType();
  if (lexer.peek_token() == lexer::ARROW) {
    lexer.next_token(); // consume '->'
    Type out_t = parse_Type();
    return FuncType{{t}, std::make_shared<Type>(out_t)};
  }
  return t;
}

/******************************************************************************/
// Expressions

Expr Parser::parse_FuncExpr() {
  expect_token(lexer::FUNC);
  // parse arguments
  expect_token(lexer::LPAREN);
  std::vector<Id> params;

  while (lexer.peek_token() != lexer::RPAREN) {
    expect_token(lexer::IDENTIFIER);
    std::string name = lexer.identifier();
    Id param{name, false};

    // optional type annotation
    if (lexer.peek_token() == lexer::COLON) {
      lexer.next_token(); // consume ':'
      param.type = parse_Type();
    }
    params.push_back(param);

    if (lexer.peek_token() == lexer::COMMA) {
      lexer.next_token(); // consume ','
    } else {
      break;
    }
  }
  expect_token(lexer::RPAREN);

  // optional type
  Type out_t = Meta{};
  if (lexer.peek_token() == lexer::COLON) {
    lexer.next_token(); // consume ':'
    out_t = parse_Type();
  }

  // parse body
  expect_token(lexer::LBRACE); // new scope
  scope.emplace_back();
  int loop_depth_outer = loop_depth;
  loop_depth = 0;

  std::vector<Cmd> cmds;
  for (auto &param : params) {
    scope.back().insert_or_assign(param.name, param);
  }
  while (lexer.peek_token() != lexer::RBRACE) {
    cmds.push_back(parse_Cmd());
  }

  expect_token(lexer::RBRACE); // exit scope
  scope.pop_back();
  loop_depth = loop_depth_outer;

  return FuncExpr{std::move(params), out_t, std::move(cmds)};
}

Expr Parser::parse_TupleExpr() {
  expect_token(lexer::LPAREN);
  auto expr = parse_Expr();
  std::vector<Expr> exprs;
  exprs.push_back(std::move(expr));
  while (lexer.peek_token() == lexer::COMMA) {
    lexer.next_token(); // consume ','
    exprs.push_back(parse_Expr());
  }
  expect_token(lexer::RPAREN);
  if (exprs.size() == 1) {
    return std::move(exprs[0]);
  }
  return TupleExpr{std::move(exprs)};
}

Expr Parser::parse_AtomExpr() {
  auto tok = lexer.peek_token();
  switch (tok) {
  case lexer::TRUE:
    lexer.next_token(); // consume 'true'
    return true;
  case lexer::FALSE:
    lexer.next_token(); // consume 'false'
    return false;
  case lexer::INT_LITERAL:
    lexer.next_token(); // consume literal
    return lexer.int_literal();
  case lexer::IDENTIFIER:
    lexer.next_token(); // consume identifier
    return find_id(lexer.identifier());
  case lexer::FUNC:
    return parse_FuncExpr();
  case lexer::LPAREN:
    return parse_TupleExpr();
  default:
    throw ParsingException(lexer.get_pos());
  }
}

Expr Parser::parse_AppExpr() {
  auto lhs = parse_AtomExpr();
  while (lexer.peek_token() == lexer::LPAREN) {
    lexer.next_token(); // consume '('
    std::vector<Expr> args;
    if (lexer.peek_token() != lexer::RPAREN) {
      args.push_back(parse_Expr());
      while (lexer.peek_token() == lexer::COMMA) {
        lexer.next_token(); // consume ','
        args.push_back(parse_Expr());
      }
    }
    expect_token(lexer::RPAREN);
    lhs = AppExpr{std::make_unique<Expr>(std::move(lhs)), std::move(args)};
  }
  return lhs;
}

Expr Parser::parse_PrjExpr() {
  auto rhs = parse_AppExpr();
  while (lexer.peek_token() == lexer::DOT) {
    lexer.next_token(); // consume '.'
    expect_token(lexer::INT_LITERAL);
    unsigned prj = lexer.int_literal();
    rhs = PrjExpr{prj, std::make_unique<Expr>(std::move(rhs))};
  }
  return rhs;
}

Expr Parser::parse_UnaryOpExpr() {
  auto tok = lexer.peek_token();
  if (tok == lexer::NOT || tok == lexer::SUB) {
    lexer.next_token(); // consume the unary operator
    UnaryOp op = (tok == lexer::NOT) ? UnaryOp::NOT : UnaryOp::NEG;
    auto rhs = parse_UnaryOpExpr();
    return UnaryOpExpr{op, std::make_unique<Expr>(std::move(rhs))};
  }
  return parse_PrjExpr();
}

Expr Parser::parse_BinaryOpExpr0() {
  auto lhs = parse_UnaryOpExpr();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::MUL && tok != lexer::DIV && tok != lexer::MOD)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op;
    if (tok == lexer::MUL)
      op = BinaryOp::MUL;
    else if (tok == lexer::DIV)
      op = BinaryOp::DIV;
    else
      op = BinaryOp::MOD;
    auto rhs = parse_UnaryOpExpr();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr1() {
  auto lhs = parse_BinaryOpExpr0();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::ADD && tok != lexer::SUB)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op = (tok == lexer::ADD) ? BinaryOp::ADD : BinaryOp::SUB;
    auto rhs = parse_BinaryOpExpr0();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr2() {
  auto lhs = parse_BinaryOpExpr1();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::LT && tok != lexer::GT && tok != lexer::LE &&
        tok != lexer::GE)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op;
    if (tok == lexer::LT)
      op = BinaryOp::LT;
    else if (tok == lexer::GT)
      op = BinaryOp::GT;
    else if (tok == lexer::LE)
      op = BinaryOp::LE;
    else
      op = BinaryOp::GE;
    auto rhs = parse_BinaryOpExpr1();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr3() {
  auto lhs = parse_BinaryOpExpr2();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::EQ && tok != lexer::NE)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op = (tok == lexer::EQ) ? BinaryOp::EQ : BinaryOp::NE;
    auto rhs = parse_BinaryOpExpr2();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr4() {
  auto lhs = parse_BinaryOpExpr3();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::AND)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op = BinaryOp::AND;
    auto rhs = parse_BinaryOpExpr3();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr5() {
  auto lhs = parse_BinaryOpExpr4();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::OR)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op = BinaryOp::OR;
    auto rhs = parse_BinaryOpExpr4();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

Expr Parser::parse_BinaryOpExpr6() {
  auto lhs = parse_BinaryOpExpr5();
  while (true) {
    auto tok = lexer.peek_token();
    if (tok != lexer::ASSIGN)
      break;
    lexer.next_token(); // consume the binary operator
    BinaryOp op = BinaryOp::ASSIGN;
    auto rhs = parse_BinaryOpExpr5();
    lhs = BinaryOpExpr{op, std::make_unique<Expr>(std::move(lhs)),
                       std::make_unique<Expr>(std::move(rhs))};
  }
  return lhs;
}

/******************************************************************************/
// Commands

Cmd Parser::parse_LetCmd() {
  expect_token(lexer::LET);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, false};
  if (lexer.peek_token() == lexer::COLON) {
    lexer.next_token(); // consume ':'
    id.type = parse_Type();
  }
  expect_token(lexer::ASSIGN);
  auto expr = parse_Expr();
  expect_token(lexer::SEMI);

  scope.back().insert_or_assign(s, id);

  return LetCmd{id, std::move(expr)};
}

Cmd Parser::parse_VarCmd() {
  expect_token(lexer::VAR);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, true};
  if (lexer.peek_token() == lexer::COLON) {
    lexer.next_token(); // consume ':'
    id.type = parse_Type();
  }
  std::optional<Expr> expr = std::nullopt;
  if (lexer.peek_token() == lexer::ASSIGN) {
    lexer.next_token(); // consume '='
    expr.emplace(parse_Expr());
  }
  expect_token(lexer::SEMI);
  scope.back().insert_or_assign(s, id);
  return VarCmd{id, std::move(expr)};
}

Cmd Parser::parse_FuncCmd() {
  // Function command
  expect_token(lexer::FUNC);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, false};

  // Parameters
  expect_token(lexer::LPAREN);
  std::vector<Id> params;
  while (lexer.peek_token() != lexer::RPAREN) {
    expect_token(lexer::IDENTIFIER);
    std::string name = lexer.identifier();
    Id param{name, false};
    if (lexer.peek_token() == lexer::COLON) {
      lexer.next_token(); // consume ':'
      param.type = parse_Type();
    }
    params.emplace_back(param);
    if (lexer.peek_token() == lexer::COMMA) {
      lexer.next_token(); // consume ','
    } else {
      break;
    }
  }
  expect_token(lexer::RPAREN);

  // Return type
  Type return_type = (lexer.peek_token() == lexer::COLON)
                         ? (lexer.next_token(), parse_Type())
                         : Meta{};

  // Function type
  std::vector<Type> in_types;
  for (const auto &param : params) {
    in_types.push_back(param.type);
  }
  Type func_type =
      FuncType{std::move(in_types), std::make_shared<Type>(return_type)};
  id.type = func_type;
  scope.back().insert_or_assign(s, id);

  // Function body
  expect_token(lexer::LBRACE); // new scope
  scope.emplace_back();
  int loop_depth_outer = loop_depth;
  loop_depth = 0;

  for (auto &param : params) {
    scope.back().insert_or_assign(param.name, param);
  }
  std::vector<Cmd> cmds;
  while (lexer.peek_token() != lexer::RBRACE) {
    cmds.push_back(parse_Cmd());
  }

  expect_token(lexer::RBRACE); // exit scope
  scope.pop_back();
  loop_depth = loop_depth_outer;

  return FuncCmd{id, std::move(params), std::move(cmds)};
}

Cmd Parser::parse_BlockCmd() {
  expect_token(lexer::LBRACE);
  scope.emplace_back(); // new scope
  std::vector<Cmd> cmds;
  while (true) {
    auto tok = lexer.peek_token();
    if (tok == lexer::RBRACE)
      break;
    cmds.push_back(parse_Cmd());
  }
  expect_token(lexer::RBRACE);
  scope.pop_back(); // exit scope
  return BlockCmd{std::move(cmds)};
}

Cmd Parser::parse_IteCmd() {
  expect_token(lexer::IF);
  expect_token(lexer::LPAREN);
  auto cond = parse_Expr();
  expect_token(lexer::RPAREN);
  auto br_t = std::make_unique<Cmd>(parse_Cmd());
  std::unique_ptr<Cmd> br_f = nullptr;
  if (lexer.peek_token() == lexer::ELSE) {
    lexer.next_token(); // consume 'else'
    br_f = std::make_unique<Cmd>(parse_Cmd());
  }
  return IteCmd{std::move(cond), std::move(br_t), std::move(br_f)};
}

Cmd Parser::parse_ForCmd() {
  expect_token(lexer::FOR);
  expect_token(lexer::LPAREN);
  scope.emplace_back(); // new scope for loop variable
  auto init = std::make_unique<Cmd>(parse_VarCmd());
  auto cond = parse_Expr();
  expect_token(lexer::SEMI);
  auto incr = parse_Expr();
  expect_token(lexer::RPAREN);
  loop_depth++;
  auto body = std::make_unique<Cmd>(parse_Cmd());
  loop_depth--;
  scope.pop_back(); // exit scope
  return ForCmd{std::move(init), std::move(cond), std::move(incr),
                std::move(body)};
}

Cmd Parser::parse_WhileCmd() {
  expect_token(lexer::WHILE);
  expect_token(lexer::LPAREN);
  auto cond = parse_Expr();
  expect_token(lexer::RPAREN);
  loop_depth++;
  auto body = std::make_unique<Cmd>(parse_Cmd());
  loop_depth--;
  return WhileCmd{std::move(cond), std::move(body)};
}

Cmd Parser::parse_ContinueCmd() {
  if (!loop_depth)
    throw ParsingException("continue statement not within a loop");
  expect_token(lexer::CONTINUE);
  expect_token(lexer::SEMI);
  return ContinueCmd{};
}

Cmd Parser::parse_BreakCmd() {
  if (!loop_depth)
    throw ParsingException("break statement not within a loop");
  expect_token(lexer::BREAK);
  expect_token(lexer::SEMI);
  return BreakCmd{};
}

Cmd Parser::parse_ReturnCmd() {
  expect_token(lexer::RETURN);
  std::optional<Expr> ret = std::nullopt;
  if (lexer.peek_token() != lexer::SEMI) {
    ret.emplace(parse_Expr());
  }
  expect_token(lexer::SEMI);
  return ReturnCmd{std::move(ret)};
}

Cmd Parser::parse_Cmd() {
  auto tok = lexer.peek_token();
  switch (tok) {
  case lexer::LET:
    return parse_LetCmd();
  case lexer::VAR:
    return parse_VarCmd();
  case lexer::FUNC:
    return parse_FuncCmd();
  case lexer::LBRACE:
    return parse_BlockCmd();
  case lexer::IF:
    return parse_IteCmd();
  case lexer::FOR:
    return parse_ForCmd();
  case lexer::WHILE:
    return parse_WhileCmd();
  case lexer::CONTINUE:
    return parse_ContinueCmd();
  case lexer::BREAK:
    return parse_BreakCmd();
  case lexer::RETURN:
    return parse_ReturnCmd();
  default:
    auto expr = parse_Expr();
    expect_token(lexer::SEMI);
    return expr;
  }
}

/******************************************************************************/
// Declarations

Decl Parser::parse_ExternDecl() {
  // Extern function declaration
  expect_token(lexer::EXTERN);
  expect_token(lexer::FUNC);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, false};

  // Parameters
  expect_token(lexer::LPAREN);
  std::vector<Id> params;
  while (lexer.peek_token() != lexer::RPAREN) {
    expect_token(lexer::IDENTIFIER);
    std::string param_name = lexer.identifier();
    Id param{param_name, false};
    expect_token(lexer::COLON);
    param.type = parse_Type();
    params.emplace_back(param);
    if (lexer.peek_token() == lexer::COMMA) {
      lexer.next_token(); // consume ','
    } else {
      break;
    }
  }
  expect_token(lexer::RPAREN);

  // Return type
  Type return_type = (lexer.peek_token() == lexer::COLON)
                         ? (lexer.next_token(), parse_Type())
                         : ScalarType::VOID;
  expect_token(lexer::SEMI);

  // Function type
  std::vector<Type> in_types;
  for (const auto &param : params) {
    in_types.push_back(param.type);
  }
  Type func_type =
      FuncType{std::move(in_types), std::make_shared<Type>(return_type)};
  id.type = func_type;
  scope.back().insert_or_assign(s, id);

  return ExternDecl{id, std::move(params)};
}

Decl Parser::parse_FuncDecl() {
  // Function declaration
  expect_token(lexer::FUNC);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, false};

  // Parameters
  expect_token(lexer::LPAREN);
  std::vector<Id> params;
  while (lexer.peek_token() != lexer::RPAREN) {
    expect_token(lexer::IDENTIFIER);
    std::string name = lexer.identifier();
    Id param{name, false};
    if (lexer.peek_token() == lexer::COLON) {
      lexer.next_token(); // consume ':'
      param.type = parse_Type();
    }
    params.emplace_back(param);
    if (lexer.peek_token() == lexer::COMMA) {
      lexer.next_token(); // consume ','
    } else {
      break;
    }
  }
  expect_token(lexer::RPAREN);

  // Return type
  Type return_type = (lexer.peek_token() == lexer::COLON)
                         ? (lexer.next_token(), parse_Type())
                         : Meta{};

  // Function type
  std::vector<Type> in_types;
  for (const auto &param : params) {
    in_types.push_back(param.type);
  }
  Type func_type =
      FuncType{std::move(in_types), std::make_shared<Type>(return_type)};
  id.type = func_type;
  scope.back().insert_or_assign(s, id);

  // Function body
  expect_token(lexer::LBRACE);
  scope.emplace_back(); // new scope
  for (auto &param : params) {
    scope.back().insert_or_assign(param.name, param);
  }
  std::vector<Cmd> cmds;
  while (lexer.peek_token() != lexer::RBRACE) {
    cmds.push_back(parse_Cmd());
  }
  expect_token(lexer::RBRACE);
  scope.pop_back(); // exit scope

  return FuncDecl{id, std::move(params), std::move(cmds)};
}

Decl Parser::parse_LetDecl() {
  expect_token(lexer::LET);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, false};
  if (lexer.peek_token() == lexer::COLON) {
    lexer.next_token(); // consume ':'
    id.type = parse_Type();
  }
  expect_token(lexer::ASSIGN);
  auto expr = parse_Expr();
  expect_token(lexer::SEMI);
  scope.back().insert_or_assign(s, id);
  return LetDecl{id, std::move(expr)};
}

Decl Parser::parse_VarDecl() {
  expect_token(lexer::VAR);
  expect_token(lexer::IDENTIFIER);
  std::string s = lexer.identifier();
  Id id{s, true};
  if (lexer.peek_token() == lexer::COLON) {
    lexer.next_token(); // consume ':'
    id.type = parse_Type();
  }
  std::optional<Expr> expr = std::nullopt;
  if (lexer.peek_token() == lexer::ASSIGN) {
    lexer.next_token(); // consume '='
    expr.emplace(parse_Expr());
  }
  expect_token(lexer::SEMI);
  scope.back().insert_or_assign(s, id);
  return VarDecl{id, std::move(expr)};
}

Decl Parser::parse_Decl() {
  auto tok = lexer.peek_token();
  switch (tok) {
  case lexer::EXTERN:
    return parse_ExternDecl();
  case lexer::FUNC:
    return parse_FuncDecl();
  case lexer::LET:
    return parse_LetDecl();
  case lexer::VAR:
    return parse_VarDecl();
  default: {
    std::string tok_str = std::to_string((int)tok);
    std::string pos_str = std::to_string(lexer.get_pos());
    throw ParsingException("unexpected token (" + tok_str + ") at: " + pos_str);
  }
  }
}

/******************************************************************************/
// Program

Prog Parser::parse_Prog() {
  scope.emplace_back(); // global scope
  std::vector<Decl> decls;
  while (lexer.has_char() && lexer.peek_token() != lexer::LEXING_END) {
    decls.push_back(parse_Decl());
  }
  scope.pop_back(); // exit global scope
  return Prog{std::move(decls)};
}

} // namespace parser
} // namespace implang