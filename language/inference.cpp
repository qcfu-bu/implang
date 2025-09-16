#include "language.h"
#include <optional>
#include <variant>
#include <vector>

namespace implang {

/******************************************************************************/
// Predicates

bool is_lvalue(Expr &expr) {
  if (auto *id = std::get_if<Id>(&expr)) {
    return id->is_mutable;
  }
  if (auto *prj = std::get_if<PrjExpr>(&expr)) {
    return is_lvalue(*prj->rhs);
  }
  return false;
}

bool is_eqtype(Type &t) {
  t = resolve(t);
  if (auto *pt = std::get_if<ScalarType>(&t)) {
    return *pt == ScalarType::BOOL || *pt == ScalarType::INT;
  }
  if (auto *tu = std::get_if<TupleType>(&t)) {
    for (auto &prj_t : tu->prj_t) {
      if (!is_eqtype(prj_t))
        return false;
    }
    return true;
  }
  return false;
}

bool is_void(Type &t) {
  t = resolve(t);
  if (auto *pt = std::get_if<ScalarType>(&t)) {
    return *pt == ScalarType::VOID;
  }
  return false;
}

/******************************************************************************/
// Unification

Type resolve(Type &t) {
  if (auto *meta = std::get_if<Meta>(&t)) {
    return meta->get_inst();
  } else {
    return t;
  }
}

bool type_equal(Type &t1, Type &t2) {
  t1 = resolve(t1);
  t2 = resolve(t2);

  // Meta - Meta
  if (auto *p1 = std::get_if<Meta>(&t1), *p2 = std::get_if<Meta>(&t2);
      p1 && p2) {
    return p1->uid == p2->uid;
  }

  // ScalarType - ScalarType
  if (auto *p1 = std::get_if<ScalarType>(&t1),
      *p2 = std::get_if<ScalarType>(&t2);
      p1 && p2) {
    return *p1 == *p2;
  }

  // FuncType - FuncType
  if (auto *f1 = std::get_if<FuncType>(&t1), *f2 = std::get_if<FuncType>(&t2);
      f1 && f2) {
    if (f1->in_t.size() != f2->in_t.size())
      return false;
    for (size_t i = 0; i < f1->in_t.size(); i++) {
      if (!type_equal(f1->in_t[i], f2->in_t[i]))
        return false;
    }
    return type_equal(*f1->out_t, *f2->out_t);
  }

  // TupleType - TupleType
  if (auto *tu1 = std::get_if<TupleType>(&t1),
      *tu2 = std::get_if<TupleType>(&t2);
      tu1 && tu2) {
    if (tu1->prj_t.size() != tu2->prj_t.size())
      return false;
    for (size_t i = 0; i < tu1->prj_t.size(); i++) {
      if (!type_equal(tu1->prj_t[i], tu2->prj_t[i]))
        return false;
    }
    return true;
  }

  // otherwise
  return false;
}

bool meta_occurs(Meta &m, Type &t) {
  t = resolve(t);
  // Meta
  if (auto *m2 = std::get_if<Meta>(&t)) {
    return m.uid == m2->uid;
  }
  // FuncType
  if (auto *f = std::get_if<FuncType>(&t)) {
    for (auto &in_t : f->in_t) {
      if (meta_occurs(m, in_t))
        return true;
    }
    return meta_occurs(m, *f->out_t);
  }
  // TupleType
  if (auto *tu = std::get_if<TupleType>(&t)) {
    for (auto &prj_t : tu->prj_t) {
      if (meta_occurs(m, prj_t))
        return true;
    }
    return false;
  }
  // otherwise
  return false;
}

bool type_unify(Type &t1, Type &t2) {
  t1 = resolve(t1);
  t2 = resolve(t2);

  // meta - T
  if (auto *x1 = std::get_if<Meta>(&t1)) {
    if (meta_occurs(*x1, t2)) {
      throw TypeException("meta occurs check failed");
    } else {
      x1->set_inst(t2);
      return true;
    }
  }

  // T - meta
  if (auto *x2 = std::get_if<Meta>(&t2)) {
    if (meta_occurs(*x2, t1)) {
      throw TypeException("meta occurs check failed");
    } else {
      x2->set_inst(t1);
      return true;
    }
  }

  // ScalarType - ScalarType
  if (auto *p1 = std::get_if<ScalarType>(&t1),
      *p2 = std::get_if<ScalarType>(&t2);
      p1 && p2) {
    return *p1 == *p2;
  }

  // FuncType - FuncType
  if (auto *f1 = std::get_if<FuncType>(&t1), *f2 = std::get_if<FuncType>(&t2);
      f1 && f2) {
    if (f1->in_t.size() != f2->in_t.size())
      return false;
    for (size_t i = 0; i < f1->in_t.size(); i++) {
      if (!type_unify(f1->in_t[i], f2->in_t[i]))
        return false;
    }
    return type_unify(*f1->out_t, *f2->out_t);
  }

  // TupleType - TupleType
  if (auto *tu1 = std::get_if<TupleType>(&t1),
      *tu2 = std::get_if<TupleType>(&t2);
      tu1 && tu2) {
    if (tu1->prj_t.size() != tu2->prj_t.size())
      return false;
    for (size_t i = 0; i < tu1->prj_t.size(); i++) {
      if (!type_unify(tu1->prj_t[i], tu2->prj_t[i]))
        return false;
    }
    return true;
  }

  // otherwise
  return false;
}

/******************************************************************************/
// Expressions

struct ExprInfer {
  Type operator()(bool &b) { return ScalarType::BOOL; }
  Type operator()(int &i) { return ScalarType::INT; }
  Type operator()(Id &id) { return id.type; }
  Type operator()(UnaryOpExpr &uop);
  Type operator()(BinaryOpExpr &bop);
  Type operator()(FuncExpr &app);
  Type operator()(AppExpr &app);
  Type operator()(TupleExpr &tup);
  Type operator()(PrjExpr &prj);
};

Type infer_type(Expr &expr) { return std::visit(ExprInfer(), expr); }

Type ExprInfer::operator()(UnaryOpExpr &uop) {
  Type rhs_t = infer_type(*uop.rhs);
  switch (uop.op) {
  case UnaryOp::NOT: {
    Type bool_t = ScalarType::BOOL;
    if (!type_unify(rhs_t, bool_t)) {
      throw TypeException("type error: expected bool");
    }
    return bool_t;
  }
  case UnaryOp::NEG: {
    Type int_t = ScalarType::INT;
    if (!type_unify(rhs_t, int_t)) {
      throw TypeException("type error: expected int");
    }
    return ScalarType::INT;
  }
  }
}

Type ExprInfer::operator()(BinaryOpExpr &bop) {
  Type lhs_t = infer_type(*bop.lhs);
  Type rhs_t = infer_type(*bop.rhs);
  switch (bop.op) {
  case BinaryOp::ADD:
  case BinaryOp::SUB:
  case BinaryOp::MUL:
  case BinaryOp::DIV:
  case BinaryOp::MOD: {
    Type int_t = ScalarType::INT;
    if (!type_unify(lhs_t, int_t) || !type_unify(rhs_t, int_t)) {
      throw TypeException("type error: expected int");
    }
    return int_t;
  }
  case BinaryOp::AND:
  case BinaryOp::OR: {
    Type bool_t = ScalarType::BOOL;
    if (!type_unify(lhs_t, bool_t) || !type_unify(rhs_t, bool_t)) {
      throw TypeException("type error: expected bool");
    }
    return bool_t;
  }
  case BinaryOp::LT:
  case BinaryOp::GT:
  case BinaryOp::LE:
  case BinaryOp::GE: {
    Type int_t = ScalarType::INT;
    if (!type_unify(lhs_t, int_t) || !type_unify(rhs_t, int_t)) {
      throw TypeException("type error: expected int");
    }
    return ScalarType::BOOL;
  }
  case BinaryOp::EQ:
  case BinaryOp::NE: {
    if (!type_unify(lhs_t, rhs_t)) {
      throw TypeException("type error: mismatched types");
    }
    if (!is_eqtype(lhs_t)) {
      throw TypeException("type error: expected eqtype");
    }
    return ScalarType::BOOL;
  }
  case BinaryOp::ASSIGN: {
    if (!is_lvalue(*bop.lhs)) {
      throw TypeException("type error: lvalue side is not assignable");
    }
    if (!type_unify(lhs_t, rhs_t)) {
      throw TypeException("type error: mismatched types");
    }
    if (is_void(lhs_t)) {
      throw TypeException("type error: cannot assign void");
    }
    return ScalarType::VOID;
  }
  }
}

Type ExprInfer::operator()(FuncExpr &func) {
  std::vector<Type> args_t;
  for (auto &arg : func.args) {
    args_t.push_back(arg.type);
  }
  auto func_t = FuncType{args_t, std::make_shared<Type>(func.out_t)};
  Type ret_t = ScalarType::VOID;
  for (auto &cmd : func.body) {
    // first returned type
    if (auto t = infer_type(cmd)) {
      ret_t = *t;
      break;
    }
  }
  if (!type_unify(*func_t.out_t, ret_t)) {
    throw TypeException("type error: mismatched return type in function");
  }
  return func_t;
}

Type ExprInfer::operator()(AppExpr &app) {
  Type func_t = infer_type(*app.func);
  std::vector<Type> args_t;
  for (auto &arg : app.args) {
    args_t.push_back(infer_type(arg));
  }
  Type m = Meta{};
  Type func_expect = FuncType{args_t, std::make_shared<Type>(m)};
  if (!type_unify(func_t, func_expect)) {
    throw TypeException("type error: function application mismatch");
  }
  return resolve(m);
}

Type ExprInfer::operator()(TupleExpr &tup) {
  std::vector<Type> prj_t;
  for (auto &arg : tup.args) {
    prj_t.push_back(infer_type(arg));
  }
  return TupleType{prj_t};
}

Type ExprInfer::operator()(PrjExpr &prj) {
  Type rhs_t = infer_type(*prj.rhs);
  rhs_t = resolve(rhs_t);
  if (auto *tu = std::get_if<TupleType>(&rhs_t)) {
    if (prj.prj < 0 || prj.prj >= tu->prj_t.size()) {
      throw TypeException("type error: tuple index out of bounds");
    }
    return tu->prj_t[prj.prj];
  } else {
    throw TypeException("type error: expected tuple type");
  }
}

/******************************************************************************/
// Commands

struct CmdInfer {
  std::optional<Type> operator()(Expr &expr);
  std::optional<Type> operator()(LetCmd &cmd);
  std::optional<Type> operator()(VarCmd &cmd);
  std::optional<Type> operator()(FuncCmd &cmd);
  std::optional<Type> operator()(BlockCmd &cmd);
  std::optional<Type> operator()(IteCmd &cmd);
  std::optional<Type> operator()(ForCmd &cmd);
  std::optional<Type> operator()(WhileCmd &cmd);
  std::optional<Type> operator()(ContinueCmd &cmd);
  std::optional<Type> operator()(BreakCmd &cmd);
  std::optional<Type> operator()(ReturnCmd &cmd);
};

std::optional<Type> infer_type(Cmd &cmd) { return std::visit(CmdInfer(), cmd); }

std::optional<Type> CmdInfer::operator()(Expr &expr) {
  infer_type(expr);
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(LetCmd &cmd) {
  Type expr_t = infer_type(cmd.expr);
  if (!type_unify(cmd.id.type, expr_t)) {
    throw TypeException("type error: mismatched types in const declaration");
  }
  if (is_void(expr_t)) {
    throw TypeException("type error: cannot declare void const");
  }
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(VarCmd &cmd) {
  if (cmd.expr) {
    Type expr_t = infer_type(*cmd.expr);
    if (!type_unify(cmd.id.type, expr_t)) {
      throw TypeException("type error: mismatched types in var declaration");
    }
    if (is_void(expr_t)) {
      throw TypeException("type error: cannot declare void var");
    }
  }
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(FuncCmd &cmd) {
  auto func_t = std::get_if<FuncType>(&cmd.id.type);
  Type ret_t = ScalarType::VOID;
  for (auto &cmd : cmd.body) {
    // first returned type
    if (auto t = infer_type(cmd)) {
      ret_t = *t;
      break;
    }
  }
  if (!type_unify(*func_t->out_t, ret_t)) {
    throw TypeException("type error: mismatched return type in function");
  }
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(BlockCmd &cmd) {
  for (auto &c : cmd.cmds) {
    // first returned type
    if (auto t = std::visit(*this, c)) {
      return t;
    }
  }
  // otherwise
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(IteCmd &cmd) {
  Type cond_t = infer_type(cmd.cond);
  Type bool_t = ScalarType::BOOL;
  if (!type_unify(cond_t, bool_t))
    throw TypeException("type error: expected bool in if condition");
  auto t1 = std::visit(*this, *cmd.br_t);
  // has else branch
  if (cmd.br_f) {
    auto t2 = std::visit(*this, *cmd.br_f);
    if (t1 && t2) { // both branches return
      if (type_unify(*t1, *t2))
        return t1;
      throw TypeException("type error: mismatched types in if branches");
    }
  }
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(ForCmd &cmd) {
  std::visit(*this, *cmd.init);
  Type cond_t = infer_type(cmd.cond);
  Type bool_t = ScalarType::BOOL;
  if (!type_unify(cond_t, bool_t))
    throw TypeException("type error: expected bool in for condition");
  infer_type(cmd.incr);
  std::visit(*this, *cmd.body);
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(WhileCmd &cmd) {
  Type cond_t = infer_type(cmd.cond);
  Type bool_t = ScalarType::BOOL;
  if (!type_unify(cond_t, bool_t))
    throw TypeException("type error: expected bool in while condition");
  std::visit(*this, *cmd.body);
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(ContinueCmd &cmd) {
  return std::nullopt;
}

std::optional<Type> CmdInfer::operator()(BreakCmd &cmd) { return std::nullopt; }

std::optional<Type> CmdInfer::operator()(ReturnCmd &cmd) {
  if (cmd.ret) {
    return infer_type(*cmd.ret);
  } else {
    return ScalarType::VOID;
  }
}

/******************************************************************************/
// Declarations

struct DeclInfer {
  void operator()(ExternDecl &decl) {};
  void operator()(FuncDecl &decl);
  void operator()(LetDecl &decl);
  void operator()(VarDecl &decl);
};

void infer_type(Decl &decl) { std::visit(DeclInfer(), decl); }

void DeclInfer::operator()(FuncDecl &decl) {
  auto func_t = std::get_if<FuncType>(&decl.id.type);
  Type ret_t = ScalarType::VOID;
  for (auto &cmd : decl.body) {
    // first returned type
    if (auto t = infer_type(cmd)) {
      ret_t = *t;
      break;
    }
  }
  if (!type_unify(*func_t->out_t, ret_t)) {
    throw TypeException("type error: mismatched return type in function");
  }
  if (decl.id.name == "main") {
    Type main_t = FuncType{{}, std::make_shared<Type>(ScalarType::VOID)};
    if (!type_unify(decl.id.type, main_t)) {
      throw TypeException(
          "type error: main function must have type () -> void");
    }
  }
}

void DeclInfer::operator()(LetDecl &decl) {
  Type expr_t = infer_type(decl.expr);
  if (!type_unify(decl.id.type, expr_t)) {
    throw TypeException("type error: mismatched types in const declaration");
  }
  if (is_void(expr_t)) {
    throw TypeException("type error: cannot declare void const");
  }
}

void DeclInfer::operator()(VarDecl &decl) {
  if (decl.expr) {
    Type expr_t = infer_type(*decl.expr);
    if (!type_unify(decl.id.type, expr_t)) {
      throw TypeException("type error: mismatched types in var declaration");
    }
    if (is_void(expr_t)) {
      throw TypeException("type error: cannot declare void var");
    }
  }
}

/******************************************************************************/
// Programs

void infer_type(Prog &prog) {
  for (auto &decl : prog.decls) {
    infer_type(decl);
  }
}

} // namespace implang