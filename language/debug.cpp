#include "language.h"
#include <memory>
#include <string>
#include <variant>

namespace implang {

/******************************************************************************/
// Types

int Meta::uid_stamp = 1;

struct TypeFmt {
  std::string operator()(ScalarType &t);
  std::string operator()(FuncType &t);
  std::string operator()(TupleType &t);
  std::string operator()(Meta &t);
};

std::string to_string(Type &t) { return std::visit(TypeFmt(), t); }

std::string TypeFmt::operator()(ScalarType &t) {
  switch (t) {
  case ScalarType::VOID:
    return "void";
  case ScalarType::BOOL:
    return "bool";
  case ScalarType::INT:
    return "int";
  }
}

std::string TypeFmt::operator()(FuncType &t) {
  std::string in_ts;
  std::string out_t = to_string(*t.out_t);
  for (auto &in_t : t.in_t) {
    if (!in_ts.empty())
      in_ts += ", ";
    in_ts += to_string(in_t);
  }
  return "(" + in_ts + ") -> " + out_t;
}

std::string TypeFmt::operator()(TupleType &t) {
  std::string prj_ts;
  for (auto &prj_t : t.prj_t) {
    if (!prj_ts.empty())
      prj_ts += ", ";
    prj_ts += to_string(prj_t);
  }
  return "(" + prj_ts + ")";
}

std::string TypeFmt::operator()(Meta &t) {
  auto inst_t = t.get_inst();
  if (auto *meta = std::get_if<Meta>(&inst_t)) {
    return "?" + std::to_string(t.uid);
  } else {
    return to_string(inst_t);
  }
}

/******************************************************************************/
// Identifiers

int Id::uid_stamp = 1;

std::string to_string(Id &id) { return id.name + "@" + std::to_string(id.uid); }

/******************************************************************************/
// Operators

std::string to_string(UnaryOp &op) { return (op == UnaryOp::NOT) ? "!" : "-"; }

std::string to_string(BinaryOp &op) {
  switch (op) {
  case BinaryOp::ASSIGN:
    return "=";
  case BinaryOp::ADD:
    return "+";
  case BinaryOp::SUB:
    return "-";
  case BinaryOp::MUL:
    return "*";
  case BinaryOp::DIV:
    return "/";
  case BinaryOp::MOD:
    return "%";
  case BinaryOp::LT:
    return "<";
  case BinaryOp::GT:
    return ">";
  case BinaryOp::LE:
    return "<=";
  case BinaryOp::GE:
    return ">=";
  case BinaryOp::EQ:
    return "==";
  case BinaryOp::NE:
    return "!=";
  case BinaryOp::AND:
    return "&&";
  case BinaryOp::OR:
    return "||";
  }
}

/******************************************************************************/
// Expressions

struct ExprFmt {
  std::string operator()(bool &b);
  std::string operator()(int &i);
  std::string operator()(Id &id);
  std::string operator()(UnaryOpExpr &uop);
  std::string operator()(BinaryOpExpr &bop);
  std::string operator()(FuncExpr &app);
  std::string operator()(AppExpr &app);
  std::string operator()(TupleExpr &tup);
  std::string operator()(PrjExpr &prj);
};

std::string to_string(Expr &expr) { return std::visit(ExprFmt(), expr); }

std::string ExprFmt::operator()(bool &b) { return b ? "true" : "false"; }
std::string ExprFmt::operator()(int &i) { return std::to_string(i); }
std::string ExprFmt::operator()(Id &id) { return to_string(id); }

std::string ExprFmt::operator()(UnaryOpExpr &uop) {
  return to_string(uop.op) + " " + to_string(*uop.rhs);
}

std::string ExprFmt::operator()(BinaryOpExpr &bop) {
  std::string op = to_string(bop.op);
  std::string lhs = to_string(*bop.lhs);
  std::string rhs = to_string(*bop.rhs);
  return "(" + lhs + " " + op + " " + rhs + ")";
}

std::string ExprFmt::operator()(FuncExpr &func) {
  std::string args;
  for (auto &arg : func.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg) + ": " + to_string(arg.type);
  }
  std::string out_t = to_string(func.out_t);
  std::string body;
  for (auto &cmd : func.body) {
    body += to_string(cmd) + "\n";
  }
  return "func(" + args + "): " + out_t + " {\n" + body + "}";
}

std::string ExprFmt::operator()(AppExpr &app) {
  std::string args;
  for (auto &arg : app.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg);
  }
  return to_string(*app.func) + "(" + args + ")";
}

std::string ExprFmt::operator()(TupleExpr &tup) {
  std::string args;
  for (auto &arg : tup.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg);
  }
  return "(" + args + ")";
}

std::string ExprFmt::operator()(PrjExpr &prj) {
  return to_string(*prj.rhs) + "." + std::to_string(prj.prj);
}

/******************************************************************************/
// Commands

struct CmdFmt {
  std::string operator()(Expr &expr);
  std::string operator()(ConstCmd &cmd);
  std::string operator()(VarCmd &cmd);
  std::string operator()(FuncCmd &cmd);
  std::string operator()(BlockCmd &cmd);
  std::string operator()(IteCmd &cmd);
  std::string operator()(ForCmd &cmd);
  std::string operator()(WhileCmd &cmd);
  std::string operator()(ContinueCmd &cmd);
  std::string operator()(BreakCmd &cmd);
  std::string operator()(ReturnCmd &cmd);
};

std::string to_string(Cmd &cmd) { return std::visit(CmdFmt(), cmd); }

std::string CmdFmt::operator()(Expr &expr) { return to_string(expr) + ";"; }

std::string CmdFmt::operator()(ConstCmd &cmd) {
  std::string id = to_string(cmd.id);
  std::string type = to_string(cmd.id.type);
  std::string expr = to_string(cmd.expr);
  return "const " + id + ": " + type + " = " + expr + ";";
}

std::string CmdFmt::operator()(VarCmd &cmd) {
  std::string id = to_string(cmd.id);
  std::string type = to_string(cmd.id.type);
  std::string expr = cmd.expr ? to_string(*cmd.expr) : "undefined";
  return "var " + id + ": " + type + " = " + expr + ";";
}

std::string CmdFmt::operator()(FuncCmd &cmd) {
  std::string args;
  for (auto &arg : cmd.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg) + ": " + to_string(arg.type);
  }
  std::string id = to_string(cmd.id);
  std::string out_t = to_string(*std::get_if<FuncType>(&cmd.id.type)->out_t);
  std::string body;
  for (auto &c : cmd.body) {
    body += to_string(c) + "\n";
  }
  return "func " + id + "(" + args + "): " + out_t + " {\n" + body + "}";
}

std::string CmdFmt::operator()(BlockCmd &cmd) {
  std::string cmds;
  for (auto &c : cmd.cmds) {
    cmds += to_string(c) + "\n";
  }
  return "{\n" + cmds + "}";
}

std::string CmdFmt::operator()(IteCmd &cmd) {
  std::string cond = to_string(cmd.cond);
  std::string br_t = to_string(*cmd.br_t);
  std::string br_f = cmd.br_f ? "\nelse\n" + to_string(*cmd.br_f) : "";
  return "if (" + cond + ")\n" + br_t + br_f;
}

std::string CmdFmt::operator()(ForCmd &cmd) {
  std::string init = to_string(*cmd.init);
  std::string cond = to_string(cmd.cond);
  std::string incr = to_string(cmd.incr);
  std::string body = to_string(*cmd.body);
  return "for (" + init + " " + cond + "; " + incr + ")\n" + body;
}

std::string CmdFmt::operator()(WhileCmd &cmd) {
  return "while (" + to_string(cmd.cond) + ")\n" + to_string(*cmd.body);
}

std::string CmdFmt::operator()(ContinueCmd &cmd) { return "continue;"; }

std::string CmdFmt::operator()(BreakCmd &cmd) { return "break;"; }

std::string CmdFmt::operator()(ReturnCmd &cmd) {
  if (cmd.ret) {
    return "return " + to_string(*cmd.ret) + ";";
  } else {
    return "return;";
  }
}

/******************************************************************************/
// Declarations

struct DeclFmt {
  std::string operator()(ExternDecl &func);
  std::string operator()(FuncDecl &func);
  std::string operator()(ConstDecl &cst);
  std::string operator()(VarDecl &var);
};

std::string to_string(Decl &decl) { return std::visit(DeclFmt(), decl); }

std::string DeclFmt::operator()(ExternDecl &func) {
  std::string args;
  for (auto &arg : func.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg) + ": " + to_string(arg.type);
  }
  std::string id = to_string(func.id);
  std::string out_t = to_string(*std::get_if<FuncType>(&func.id.type)->out_t);
  return "extern func " + id + "(" + args + ") -> " + out_t + ";";
}

std::string DeclFmt::operator()(FuncDecl &func) {
  std::string args;
  for (auto &arg : func.args) {
    if (!args.empty())
      args += ", ";
    args += to_string(arg) + ": " + to_string(arg.type);
  }
  std::string id = to_string(func.id);
  std::string out_t = to_string(*std::get_if<FuncType>(&func.id.type)->out_t);
  std::string body;
  for (auto &c : func.body) {
    body += to_string(c) + "\n";
  }
  return "func " + id + "(" + args + "): " + out_t + " {\n" + body + "}";
}

std::string DeclFmt::operator()(ConstDecl &cst) {
  std::string id = to_string(cst.id);
  std::string type = to_string(cst.id.type);
  std::string expr = to_string(cst.expr);
  return "const " + id + ": " + type + " = " + expr + ";";
}

std::string DeclFmt::operator()(VarDecl &var) {
  std::string id = to_string(var.id);
  std::string type = to_string(var.id.type);
  std::string expr = var.expr ? to_string(*var.expr) : "undefined";
  return "var " + id + ": " + type + " = " + expr + ";";
}

/******************************************************************************/
// Program

std::string to_string(Prog &prog) {
  std::string decls;
  for (auto &decl : prog.decls) {
    decls += to_string(decl) + "\n\n";
  }
  return decls;
}

} // namespace implang