#pragma once
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <variant>
#include <vector>

namespace implang {

class TypeException : public std::exception {
public:
  explicit TypeException(const std::string &msg) : msg(msg) {}
  const char *what() const noexcept override { return msg.c_str(); }

private:
  std::string msg;
};

enum ScalarType {
  VOID,
  BOOL,
  INT,
};

struct FuncType;
struct TupleType;
struct Meta;

enum UnaryOp {
  NOT,
  NEG,
};

enum BinaryOp {
  ASSIGN,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  AND,
  OR,
  LE,
  GE,
  LT,
  GT,
  EQ,
  NE,
};

struct Id;

struct UnaryOpExpr;
struct BinaryOpExpr;
struct FuncExpr;
struct AppExpr;
struct TupleExpr;
struct PrjExpr;

struct ConstCmd;
struct VarCmd;
struct FuncCmd;
struct BlockCmd;
struct IteCmd;
struct ForCmd;
struct WhileCmd;
struct ContinueCmd;
struct BreakCmd;
struct ReturnCmd;

struct ExternDecl;
struct FuncDecl;
struct ConstDecl;
struct VarDecl;

struct Prog;

using Type = std::variant<ScalarType, FuncType, TupleType, Meta>;
using Expr = std::variant<bool, int, Id, UnaryOpExpr, BinaryOpExpr, FuncExpr,
                          AppExpr, TupleExpr, PrjExpr>;
using Cmd = std::variant<Expr, ConstCmd, VarCmd, FuncCmd, BlockCmd, IteCmd,
                         ForCmd, WhileCmd, ContinueCmd, BreakCmd, ReturnCmd>;
using Decl = std::variant<ExternDecl, FuncDecl, ConstDecl, VarDecl>;

std::string to_string(Type &t);
std::string to_string(Id &id);
std::string to_string(UnaryOp &op);
std::string to_string(BinaryOp &op);
std::string to_string(Expr &expr);
std::string to_string(Cmd &cmd);
std::string to_string(Decl &decl);
std::string to_string(Prog &prog);

Type resolve(Type &t);
bool type_equal(Type &t1, Type &t2);
bool type_unify(Type &t1, Type &t2);

Type infer_type(Expr &expr);
std::optional<Type> infer_type(Cmd &cmd);
void infer_type(Decl &decl);
void infer_type(Prog &prog);

/******************************************************************************/
// Types

struct FuncType {
  std::vector<Type> in_t;
  std::shared_ptr<Type> out_t;
};

struct TupleType {
  std::vector<Type> prj_t;
};

struct Meta {
  static int uid_stamp;
  int uid;
  std::shared_ptr<std::optional<Type>> inst_t;

  Meta() : uid(uid_stamp++), inst_t(std::make_shared<std::optional<Type>>()) {}
  Meta(const Meta &other) : uid(other.uid), inst_t(other.inst_t) {}

  bool is_terminal() { return inst_t && !(*inst_t); }

  Type get_inst() {
    if (is_terminal())
      return *this;
    // path compression
    if (auto *meta = std::get_if<Meta>(&**inst_t)) {
      auto t = meta->get_inst();
      inst_t->emplace(t);
      return t;
    } else {
      return **inst_t;
    }
  }

  void set_inst(Type t) { inst_t->emplace(t); }

  bool operator<(const Meta &other) const { return uid < other.uid; }
};

/******************************************************************************/
// Identifiers

struct Id {
  static int uid_stamp;
  int uid;
  std::string name;
  Type type;
  bool is_mutable = false;

  Id() = default;
  Id(std::string name, bool is_mutable)
      : uid(uid_stamp++), name(name), type(Meta()), is_mutable(is_mutable) {}

  bool operator==(Id &other) { return uid == other.uid; }
  bool operator<(const Id &other) const { return uid < other.uid; }
};

/******************************************************************************/
// Expressions

struct UnaryOpExpr {
  UnaryOp op;
  std::unique_ptr<Expr> rhs;
};

struct BinaryOpExpr {
  BinaryOp op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
};

struct TupleExpr {
  std::vector<Expr> args;
};

struct PrjExpr {
  unsigned prj;
  std::unique_ptr<Expr> rhs;
};

struct FuncExpr {
  std::vector<Id> args;
  Type out_t;
  std::vector<Cmd> body;
};

struct AppExpr {
  std::unique_ptr<Expr> func;
  std::vector<Expr> args;
};

/******************************************************************************/
// Commands

struct ConstCmd {
  Id id;
  Expr expr;
};

struct VarCmd {
  Id id;
  std::optional<Expr> expr;
};

struct FuncCmd {
  Id id;
  std::vector<Id> args;
  std::vector<Cmd> body;
};

struct BlockCmd {
  std::vector<Cmd> cmds;
};

struct IteCmd {
  Expr cond;
  std::unique_ptr<Cmd> br_t;
  std::unique_ptr<Cmd> br_f; // possibly null
};

struct ForCmd {
  std::unique_ptr<Cmd> init;
  Expr cond;
  Expr incr;
  std::unique_ptr<Cmd> body;
};

struct WhileCmd {
  Expr cond;
  std::unique_ptr<Cmd> body;
};

struct ContinueCmd {};
struct BreakCmd {};

struct ReturnCmd {
  std::optional<Expr> ret;
};

/******************************************************************************/
// Declarations

struct ExternDecl {
  Id id;
  std::vector<Id> args;
};

struct FuncDecl {
  Id id;
  std::vector<Id> args;
  std::vector<Cmd> body;
};

struct ConstDecl {
  Id id;
  Expr expr;
};

struct VarDecl {
  Id id;
  std::optional<Expr> expr;
};

/******************************************************************************/
// Program

struct Prog {
  std::vector<Decl> decls;
};

} // namespace implang