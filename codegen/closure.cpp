#include "codegen.h"

namespace implang {
namespace codegen {

struct ExprFVars {
private:
  std::set<Id> &bound;

public:
  ExprFVars(std::set<Id> &bound) : bound(bound) {}

  std::set<Id> operator()(bool &b) { return {}; }
  std::set<Id> operator()(int &n) { return {}; }
  std::set<Id> operator()(Id &id);
  std::set<Id> operator()(UnaryOpExpr &uop);
  std::set<Id> operator()(BinaryOpExpr &bop);
  std::set<Id> operator()(FuncExpr &func);
  std::set<Id> operator()(AppExpr &app);
  std::set<Id> operator()(TupleExpr &tup);
  std::set<Id> operator()(PrjExpr &prj);
};

struct CmdFVars {
private:
  std::set<Id> &bound;

public:
  CmdFVars(std::set<Id> &bound) : bound(bound) {}

  std::set<Id> operator()(Expr &expr);
  std::set<Id> operator()(ConstCmd &cmd);
  std::set<Id> operator()(VarCmd &cmd);
  std::set<Id> operator()(FuncCmd &cmd);
  std::set<Id> operator()(BlockCmd &cmd);
  std::set<Id> operator()(IteCmd &cmd);
  std::set<Id> operator()(ForCmd &cmd);
  std::set<Id> operator()(WhileCmd &cmd);
  std::set<Id> operator()(ContinueCmd &cmd) { return {}; }
  std::set<Id> operator()(BreakCmd &cmd) { return {}; }
  std::set<Id> operator()(ReturnCmd &cmd);
};

std::set<Id> get_fvars(std::set<Id> &bound, Expr &expr) {
  return std::visit(ExprFVars(bound), expr);
}

std::set<Id> get_fvars(std::set<Id> &bound, Cmd &cmd) {
  return std::visit(CmdFVars(bound), cmd);
}

/******************************************************************************/
// Expressions

std::set<Id> ExprFVars::operator()(Id &id) {
  if (bound.find(id) == bound.end()) {
    return {id};
  } else {
    return {};
  }
}

std::set<Id> ExprFVars::operator()(UnaryOpExpr &uop) {
  return std::visit(*this, *uop.rhs);
}

std::set<Id> ExprFVars::operator()(BinaryOpExpr &bop) {
  auto lhs_fv = std::visit(*this, *bop.lhs);
  auto rhs_fv = std::visit(*this, *bop.rhs);
  lhs_fv.merge(rhs_fv);
  return lhs_fv;
}

std::set<Id> ExprFVars::operator()(FuncExpr &func) {
  std::set<Id> fv;
  for (auto &arg : func.args) {
    bound.insert(arg);
  }
  for (auto &cmd : func.body) {
    fv.merge(std::visit(CmdFVars(bound), cmd));
  }
  return fv;
}

std::set<Id> ExprFVars::operator()(AppExpr &app) {
  auto func_fv = std::visit(*this, *app.func);
  for (auto &arg : app.args) {
    func_fv.merge(std::visit(*this, arg));
  }
  return func_fv;
}

std::set<Id> ExprFVars::operator()(TupleExpr &tup) {
  std::set<Id> fv;
  for (auto &arg : tup.args) {
    fv.merge(std::visit(*this, arg));
  }
  return fv;
}

std::set<Id> ExprFVars::operator()(PrjExpr &prj) {
  return std::visit(*this, *prj.rhs);
}

/******************************************************************************/
// Commands

std::set<Id> CmdFVars::operator()(Expr &expr) {
  return std::visit(ExprFVars(bound), expr);
}

std::set<Id> CmdFVars::operator()(ConstCmd &cmd) {
  bound.insert(cmd.id);
  auto fv = std::visit(ExprFVars(bound), cmd.expr);
  return fv;
}

std::set<Id> CmdFVars::operator()(VarCmd &cmd) {
  bound.insert(cmd.id);
  if (cmd.expr) {
    return std::visit(ExprFVars(bound), *cmd.expr);
  } else {
    return {};
  }
}

std::set<Id> CmdFVars::operator()(FuncCmd &cmd) {
  bound.insert(cmd.id);
  for (auto &arg : cmd.args) {
    bound.insert(arg);
  }
  std::set<Id> fv;
  for (auto &c : cmd.body) {
    fv.merge(std::visit(*this, c));
  }
  return fv;
}

std::set<Id> CmdFVars::operator()(BlockCmd &cmd) {
  std::set<Id> fv;
  for (auto &c : cmd.cmds) {
    fv.merge(std::visit(*this, c));
  }
  return fv;
}

std::set<Id> CmdFVars::operator()(IteCmd &cmd) {
  auto cond_fv = std::visit(ExprFVars(bound), cmd.cond);
  auto then_fv = std::visit(*this, *cmd.br_t);
  cond_fv.merge(then_fv);
  if (cmd.br_f) {
    auto else_fv = std::visit(*this, *cmd.br_f);
    cond_fv.merge(else_fv);
  }
  return cond_fv;
}

std::set<Id> CmdFVars::operator()(ForCmd &cmd) {
  std::set<Id> fv;
  auto init_fv = std::visit(*this, *cmd.init);
  auto incr_fv = std::visit(ExprFVars(bound), cmd.incr);
  auto cond_fv = std::visit(ExprFVars(bound), cmd.cond);
  auto body_fv = std::visit(*this, *cmd.body);
  fv.merge(init_fv);
  fv.merge(cond_fv);
  fv.merge(incr_fv);
  fv.merge(body_fv);
  return fv;
}

std::set<Id> CmdFVars::operator()(WhileCmd &cmd) {
  auto cond_fv = std::visit(ExprFVars(bound), cmd.cond);
  auto body_fv = std::visit(*this, *cmd.body);
  cond_fv.merge(body_fv);
  return cond_fv;
}

std::set<Id> CmdFVars::operator()(ReturnCmd &cmd) {
  if (cmd.ret) {
    return std::visit(ExprFVars(bound), *cmd.ret);
  } else {
    return {};
  }
}

} // namespace codegen
} // namespace implang
