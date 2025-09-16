#include "codegen.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/TargetParser/Host.h"
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace implang {
namespace codegen {
using namespace llvm;

struct Compile {
  CodeGen *codegen;
  LLVMContext &context;
  Module &module;
  IRBuilder<> &builder;
  Function *allocator;
  SymbolTable &table;

  Compile(CodeGen *codegen)
      : codegen{codegen}, context(codegen->context), module(codegen->module),
        builder(codegen->builder), table(codegen->table),
        allocator(codegen->allocator) {}
};

struct ClosureType {
  static StructType *get(LLVMContext &context);
};

struct ClosureFuncType {
  static FunctionType *get(LLVMContext &context, Type &func_t);
};

struct TypeCompile {
private:
  LLVMContext &context;
  TypeCompile(LLVMContext &context) : context(context) {}

public:
  llvm::Type *operator()(ScalarType &pt);
  llvm::Type *operator()(FuncType &ft);
  llvm::Type *operator()(TupleType &tt);
  llvm::Type *operator()(Meta &mt);

  static llvm::Type *get(LLVMContext &context, Type &t) {
    return std::visit(TypeCompile{context}, t);
  }
};

struct ExprCompile : public Compile {
  ExprCompile(CodeGen *codegen) : Compile{codegen} {}

  Value *get_lvalue(Expr *expr);
  Value *operator()(bool b);
  Value *operator()(int n);
  Value *operator()(Id &id);
  Value *operator()(UnaryOpExpr &uop);
  Value *operator()(BinaryOpExpr &bop);
  Value *operator()(FuncExpr &func);
  Value *operator()(AppExpr &app);
  Value *operator()(TupleExpr &tup);
  Value *operator()(PrjExpr &prj);
};

struct CmdCompile : public Compile {
  BasicBlock *continue_bb = nullptr;
  BasicBlock *break_bb = nullptr;

  CmdCompile(CodeGen *codegen) : Compile{codegen} {}

  AllocaInst *alloc_var(Id &id);
  void operator()(Expr &expr);
  void operator()(LetCmd &cmd);
  void operator()(VarCmd &cmd);
  void operator()(FuncCmd &cmd);
  void operator()(BlockCmd &cmd);
  void operator()(IteCmd &cmd);
  void operator()(ForCmd &cmd);
  void operator()(WhileCmd &cmd);
  void operator()(ContinueCmd &cmd);
  void operator()(BreakCmd &cmd);
  void operator()(ReturnCmd &cmd);
};

struct DeclCompile : public Compile {
  DeclCompile(CodeGen *codegen) : Compile{codegen} {}

  void operator()(ExternDecl &decl);
  void operator()(FuncDecl &decl);
  void operator()(LetDecl &decl);
  void operator()(VarDecl &decl);
};

Value *find_id(const Id &id, SymbolTable &env) {
  for (auto it = env.rbegin(); it != env.rend(); ++it) {
    if (it->find(id) != it->end()) {
      return it->at(id);
    }
  }
  return nullptr;
}

/******************************************************************************/
// Types

llvm::Type *TypeCompile::operator()(ScalarType &pt) {
  switch (pt) {
  case ScalarType::VOID:
    return llvm::Type::getVoidTy(context);
  case ScalarType::BOOL:
    return llvm::Type::getInt1Ty(context);
  case ScalarType::INT:
    return llvm::Type::getInt32Ty(context);
  default:
    throw CompileException("unknown primitive type");
  }
}

llvm::Type *TypeCompile::operator()(FuncType &ft) {
  return ClosureType::get(context);
}

llvm::Type *TypeCompile::operator()(TupleType &tt) {
  std::vector<llvm::Type *> prj_t;
  for (auto &t : tt.prj_t) {
    prj_t.push_back(std::visit(*this, t));
  }
  return llvm::StructType::get(context, prj_t);
}

llvm::Type *TypeCompile::operator()(Meta &mt) {
  auto t = mt.get_inst();
  if (std::holds_alternative<Meta>(t)) {
    throw CompileException("cannot compile meta variable");
  }
  return std::visit(*this, t);
}

StructType *ClosureType::get(LLVMContext &context) {
  llvm::Type *funcptr_t = PointerType::get(context, 0);
  llvm::Type *dataptr_t = PointerType::get(context, 0);
  return StructType::get(context, {funcptr_t, dataptr_t});
}

FunctionType *ClosureFuncType::get(LLVMContext &context, Type &func_t) {
  std::vector<llvm::Type *> args_t;
  llvm::Type *out_t = nullptr;
  auto ft = std::get<FuncType>(resolve(func_t));
  for (auto &t : ft.in_t) {
    args_t.push_back(TypeCompile::get(context, t));
  }
  args_t.push_back(ClosureType::get(context)); // closure argument
  out_t = TypeCompile::get(context, *ft.out_t);
  return llvm::FunctionType::get(out_t, args_t, false);
}

/******************************************************************************/
// Expressions

Value *ExprCompile::get_lvalue(Expr *lhs) {
  std::vector<Value *> idx_list;

  // indexing location
  while (auto *prj = std::get_if<PrjExpr>(lhs)) {
    idx_list.push_back(builder.getInt32(prj->prj));
    lhs = prj->rhs.get();
  }

  // get writable location
  Value *lvalue = nullptr;
  llvm::Type *ltype = nullptr;
  if (auto *id = std::get_if<Id>(lhs)) {
    lvalue = find_id(*id, table);
    ltype = TypeCompile::get(context, id->type);
  }

  // compute write address
  if (idx_list.size() == 1) {
    return lvalue;
  } else {
    return builder.CreateGEP(ltype, lvalue, idx_list, "geptmp");
  }
}

Value *ExprCompile::operator()(bool b) { return builder.getInt1(b); }
Value *ExprCompile::operator()(int n) { return builder.getInt32(n); }

Value *ExprCompile::operator()(Id &id) {
  Value *val = find_id(id, table);
  if (isa<AllocaInst>(val) || isa<GlobalVariable>(val)) {
    llvm::Type *type = TypeCompile::get(context, id.type);
    return builder.CreateLoad(type, val, id.name.c_str());
  } else {
    return val;
  }
}

Value *ExprCompile::operator()(UnaryOpExpr &uop) {
  Value *rhs = std::visit(*this, *uop.rhs);
  switch (uop.op) {
  case UnaryOp::NOT:
    return builder.CreateNot(rhs, "nottmp");
  case UnaryOp::NEG:
    return builder.CreateNeg(rhs, "negtmp");
  default:
    throw CompileException("unknown unary operator");
  }
}

Value *ExprCompile::operator()(BinaryOpExpr &bop) {
  // handle assignment separately
  if (bop.op == BinaryOp::ASSIGN) {
    Value *lvalue = get_lvalue(bop.lhs.get());
    Value *rvalue = std::visit(*this, *bop.rhs);
    return builder.CreateStore(rvalue, lvalue);
  }

  // other binary operators
  Value *lhs = std::visit(*this, *bop.lhs);
  Value *rhs = std::visit(*this, *bop.rhs);
  switch (bop.op) {
  case BinaryOp::ADD:
    return builder.CreateAdd(lhs, rhs, "addtmp");
  case BinaryOp::SUB:
    return builder.CreateSub(lhs, rhs, "subtmp");
  case BinaryOp::MUL:
    return builder.CreateMul(lhs, rhs, "multmp");
  case BinaryOp::DIV:
    return builder.CreateSDiv(lhs, rhs, "divtmp");
  case BinaryOp::MOD:
    return builder.CreateSRem(lhs, rhs, "modtmp");
  case BinaryOp::AND:
    return builder.CreateAnd(lhs, rhs, "andtmp");
  case BinaryOp::OR:
    return builder.CreateOr(lhs, rhs, "ortmp");
  case BinaryOp::LE:
    return builder.CreateICmpSLE(lhs, rhs, "letmp");
  case BinaryOp::GE:
    return builder.CreateICmpSGE(lhs, rhs, "getmp");
  case BinaryOp::LT:
    return builder.CreateICmpSLT(lhs, rhs, "lttmp");
  case BinaryOp::GT:
    return builder.CreateICmpSGT(lhs, rhs, "gttmp");
  case BinaryOp::EQ: {
    Type lhs_t = infer_type(*bop.lhs);
    if (std::holds_alternative<ScalarType>(lhs_t)) {
      return builder.CreateICmpEQ(lhs, rhs, "eqtmp");
    }
    if (auto *tu = std::get_if<TupleType>(&lhs_t)) {
      Value *prj_eq = builder.getInt1(true);
      for (unsigned i = 0; i < tu->prj_t.size(); i++) {
        Value *l_prj = builder.CreateExtractValue(lhs, {i}, "lprjtmp");
        Value *r_prj = builder.CreateExtractValue(rhs, {i}, "rprjtmp");
        Value *tmp = builder.CreateICmpEQ(l_prj, r_prj, "prjeqtmp");
        prj_eq = builder.CreateAnd(prj_eq, tmp, "andtmp");
      }
      return prj_eq;
    }
    throw CompileException("cannot compare equality of this type");
  }
  case BinaryOp::NE: {
    Type lhs_t = infer_type(*bop.lhs);
    if (std::holds_alternative<ScalarType>(lhs_t)) {
      return builder.CreateICmpNE(lhs, rhs, "netmp");
    }
    if (auto *tu = std::get_if<TupleType>(&lhs_t)) {
      Value *prj_ne = builder.getInt1(false);
      for (unsigned i = 0; i < tu->prj_t.size(); i++) {
        Value *l_prj = builder.CreateExtractValue(lhs, {i}, "lprjtmp");
        Value *r_prj = builder.CreateExtractValue(rhs, {i}, "rprjtmp");
        Value *tmp = builder.CreateICmpNE(l_prj, r_prj, "prjne");
        prj_ne = builder.CreateOr(prj_ne, tmp, "ortmp");
      }
      return prj_ne;
    }
    throw CompileException("cannot compare equality of this type");
  }
  default:
    throw CompileException("unknown binary operator");
  }
}

// closure conversion
Value *ExprCompile::operator()(FuncExpr &fexpr) {
  // function type
  std::vector<Type> in_t;
  for (auto &arg : fexpr.args) {
    in_t.push_back(arg.type);
  }
  Type func_t = FuncType{in_t, std::make_shared<Type>(fexpr.out_t)};

  // bound variables
  std::set<Id> bound;
  for (const auto &[id, _] : table[0]) {
    bound.insert(id);
  }
  for (auto &arg : fexpr.args) {
    bound.insert(arg);
  }
  std::set<Id> fv;
  for (auto &cmd : fexpr.body) {
    fv.merge(get_fvars(bound, cmd));
  }

  // create closure
  llvm::Type *closure_t = ClosureType::get(context);
  Value *closure_alloc = builder.CreateAlloca(closure_t, nullptr, "closure");
  Value *closure = builder.CreateLoad(closure_t, closure_alloc, "closure");
  std::vector<Id> fv_id;
  std::vector<llvm::Type *> fv_ts;
  for (auto id : fv) {
    fv_id.push_back(id);
    fv_ts.push_back(TypeCompile::get(context, id.type));
  }

  // create closure data
  llvm::Type *data_t = llvm::StructType::get(context, fv_ts);
  Value *data_size =
      builder.getInt64(module.getDataLayout().getTypeAllocSize(data_t));
  DataLayout layout = module.getDataLayout();
  IntegerType *inptr_t = layout.getIntPtrType(context);
  Value *data = builder.CreateMalloc(inptr_t, data_t, data_size, allocator,
                                     nullptr, "data");
  for (unsigned i = 0; i < fv_id.size(); i++) {
    Value *ptr = builder.CreateStructGEP(data_t, data, i);
    Value *val = find_id(fv_id[i], table);
    if (isa<AllocaInst>(val)) {
      val = builder.CreateLoad(fv_ts[i], val, fv_id[i].name.c_str());
    }
    builder.CreateStore(val, ptr);
  }

  // create closure function
  llvm::Type *ft = ClosureFuncType::get(context, func_t);
  Function *func = Function::Create(
      (FunctionType *)ft, Function::InternalLinkage, "anon_func", module);
  closure = builder.CreateInsertValue(closure, func, {0});
  closure = builder.CreateInsertValue(closure, data, {1});
  BasicBlock *outer_bb = builder.GetInsertBlock();

  // function body
  BasicBlock *bb = BasicBlock::Create(context, "entry", func);
  builder.SetInsertPoint(bb);
  table.emplace_back(); // new scope
  for (unsigned i = 0; i < fexpr.args.size(); i++) {
    Id param = fexpr.args[i];
    table.back().emplace(param, func->getArg(i));
  }
  // load free variables from closure data
  Argument *closure_arg = func->getArg(fexpr.args.size());
  Value *data_arg = builder.CreateExtractValue(closure_arg, {1});
  for (unsigned i = 0; i < fv_id.size(); i++) {
    Value *ptr = builder.CreateStructGEP(data_t, data_arg, i);
    Value *val = builder.CreateLoad(fv_ts[i], ptr, fv_id[i].name.c_str());
    table.back().emplace(fv_id[i], val);
  }
  // compile function body
  for (auto &c : fexpr.body) {
    std::visit(CmdCompile{codegen}, c);
  }
  // insert return if the function does not return anything
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
  table.pop_back(); // exit scope

  // verify the function
  verifyFunction(*func);
  builder.SetInsertPoint(outer_bb);

  return closure;
}

Value *ExprCompile::operator()(AppExpr &app) {
  Type func_t = infer_type(*app.func);
  llvm::Type *closure_t = ClosureType::get(context);
  llvm::Type *ft = ClosureFuncType::get(context, func_t);
  llvm::Type *pt = PointerType::get(context, 0);

  Value *closure = std::visit(*this, *app.func);
  std::vector<Value *> args;
  for (auto &arg : app.args) {
    args.push_back(std::visit(*this, arg));
  }
  args.push_back(closure);

  Value *func = builder.CreateExtractValue(closure, {0}, "func");
  return builder.CreateCall((llvm::FunctionType *)ft, func, args);
}

Value *ExprCompile::operator()(TupleExpr &tup) {
  std::vector<Value *> prj_vals;
  std::vector<llvm::Type *> prj_ts;
  for (auto &arg : tup.args) {
    Type t = infer_type(arg);
    prj_vals.push_back(std::visit(*this, arg));
    prj_ts.push_back(TypeCompile::get(context, t));
  }
  llvm::Type *struct_t = llvm::StructType::get(context, prj_ts);
  Value *val = builder.CreateAlloca(struct_t, nullptr, "tuptmp");
  val = builder.CreateLoad(struct_t, val, "loadtuptmp");
  for (unsigned i = 0; i < prj_vals.size(); i++) {
    val = builder.CreateInsertValue(val, prj_vals[i], {i});
  }
  return val;
}

Value *ExprCompile::operator()(PrjExpr &prj) {
  Value *rhs = std::visit(*this, *prj.rhs);
  Type rhs_t = infer_type(*prj.rhs);
  rhs_t = resolve(rhs_t);
  return builder.CreateExtractValue(rhs, {prj.prj}, "prjtmp");
}

/******************************************************************************/
// Commands

AllocaInst *CmdCompile::alloc_var(Id &id) {
  llvm::Type *t = TypeCompile::get(context, id.type);
  // allocate at entry block
  Function *func = builder.GetInsertBlock()->getParent();
  IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(t, nullptr, id.name.c_str());
}

void CmdCompile::operator()(Expr &expr) {
  std::visit(ExprCompile{codegen}, expr);
}

void CmdCompile::operator()(LetCmd &cmd) {
  llvm::Type *t = TypeCompile::get(context, cmd.id.type);
  Value *init = std::visit(ExprCompile{codegen}, cmd.expr);
  table.back().emplace(cmd.id, init);
}

void CmdCompile::operator()(VarCmd &cmd) {
  llvm::Type *t = TypeCompile::get(context, cmd.id.type);
  Value *alloca = alloc_var(cmd.id);
  table.back().emplace(cmd.id, alloca);
  if (cmd.expr) {
    Value *init = std::visit(ExprCompile{codegen}, *cmd.expr);
    builder.CreateStore(init, alloca);
  }
}

// closure conversion
void CmdCompile::operator()(FuncCmd &cmd) {
  // function type
  Type func_t = cmd.id.type;

  // bound variables
  std::set<Id> bound;
  for (const auto &[id, _] : table[0]) {
    bound.insert(id);
  }
  bound.insert(cmd.id);
  for (auto &arg : cmd.args) {
    bound.insert(arg);
  }
  std::set<Id> fv;
  for (auto &cmd : cmd.body) {
    fv.merge(get_fvars(bound, cmd));
  }

  // create closure
  llvm::Type *closure_t = ClosureType::get(context);
  Value *closure_alloc = builder.CreateAlloca(closure_t, nullptr, "closure");
  Value *closure = builder.CreateLoad(closure_t, closure_alloc, "closure");
  std::vector<Id> fv_id;
  std::vector<llvm::Type *> fv_ts;
  for (auto id : fv) {
    fv_id.push_back(id);
    fv_ts.push_back(TypeCompile::get(context, id.type));
  }

  // create closure data
  llvm::Type *data_t = llvm::StructType::get(context, fv_ts);
  Value *data_size =
      builder.getInt64(module.getDataLayout().getTypeAllocSize(data_t));
  DataLayout layout = module.getDataLayout();
  IntegerType *inptr_t = layout.getIntPtrType(context);
  Value *data = builder.CreateMalloc(inptr_t, data_t, data_size, allocator,
                                     nullptr, "data");
  for (unsigned i = 0; i < fv_id.size(); i++) {
    Value *ptr = builder.CreateStructGEP(data_t, data, i);
    Value *val = find_id(fv_id[i], table);
    if (isa<AllocaInst>(val)) {
      val = builder.CreateLoad(fv_ts[i], val, fv_id[i].name.c_str());
    }
    builder.CreateStore(val, ptr);
  }

  // create closure function
  llvm::Type *ft = ClosureFuncType::get(context, func_t);
  Function *func = Function::Create(
      (FunctionType *)ft, Function::InternalLinkage, "anon_func", module);
  closure = builder.CreateInsertValue(closure, func, {0});
  closure = builder.CreateInsertValue(closure, data, {1});
  BasicBlock *outer_bb = builder.GetInsertBlock();

  // function body
  BasicBlock *bb = BasicBlock::Create(context, "entry", func);
  builder.SetInsertPoint(bb);
  table.emplace_back(); // new scope
  for (unsigned i = 0; i < cmd.args.size(); i++) {
    Id param = cmd.args[i];
    table.back().emplace(param, func->getArg(i));
  }
  // load free variables from closure data
  Argument *closure_arg = func->getArg(cmd.args.size());
  table.back().emplace(cmd.id, closure_arg);
  Value *data_arg = builder.CreateExtractValue(closure_arg, {1});
  for (unsigned i = 0; i < fv_id.size(); i++) {
    Value *ptr = builder.CreateStructGEP(data_t, data_arg, i);
    Value *val = builder.CreateLoad(fv_ts[i], ptr, fv_id[i].name.c_str());
    table.back().emplace(fv_id[i], val);
  }
  // compile function body
  for (auto &c : cmd.body) {
    std::visit(CmdCompile{codegen}, c);
  }
  // insert return if the function does not return anything
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
  table.pop_back(); // exit scope

  // verify the function
  verifyFunction(*func);
  builder.SetInsertPoint(outer_bb);
  table.back().emplace(cmd.id, closure);
}

void CmdCompile::operator()(BlockCmd &cmd) {
  table.emplace_back(); // new scope
  for (auto &c : cmd.cmds) {
    std::visit(*this, c);
  }
  table.pop_back(); // exit scope
}

void CmdCompile::operator()(IteCmd &cmd) {
  Value *cond = std::visit(ExprCompile{codegen}, cmd.cond);
  Function *func = builder.GetInsertBlock()->getParent();

  BasicBlock *then_bb = BasicBlock::Create(context, "then", func);
  BasicBlock *else_bb = nullptr;
  BasicBlock *merge_bb = nullptr;

  if (cmd.br_f) {
    else_bb = BasicBlock::Create(context, "else", func);
    builder.CreateCondBr(cond, then_bb, else_bb);
  } else {
    merge_bb = BasicBlock::Create(context, "ifcont", func);
    builder.CreateCondBr(cond, then_bb, merge_bb);
  }

  // then block
  builder.SetInsertPoint(then_bb);
  std::visit(*this, *cmd.br_t);
  if (!builder.GetInsertBlock()->getTerminator()) {
    if (!merge_bb) {
      merge_bb = BasicBlock::Create(context, "ifcont", func);
    }
    builder.CreateBr(merge_bb);
  }

  // else block
  if (else_bb) {
    builder.SetInsertPoint(else_bb);
    std::visit(*this, *cmd.br_f);
    if (!builder.GetInsertBlock()->getTerminator()) {
      if (!merge_bb) {
        merge_bb = BasicBlock::Create(context, "ifcont", func);
      }
      builder.CreateBr(merge_bb);
    }
  }

  // merge block
  if (merge_bb) {
    builder.SetInsertPoint(merge_bb);
  }
}

void CmdCompile::operator()(ForCmd &cmd) {
  Function *func = builder.GetInsertBlock()->getParent();

  BasicBlock *cond_bb = BasicBlock::Create(context, "cond", func);
  BasicBlock *body_bb = BasicBlock::Create(context, "body", func);
  BasicBlock *incr_bb = BasicBlock::Create(context, "incr", func);
  BasicBlock *after_bb = BasicBlock::Create(context, "after", func);

  // init
  std::visit(*this, *cmd.init);
  builder.CreateBr(cond_bb);

  // condition block
  builder.SetInsertPoint(cond_bb);
  Value *cond = std::visit(ExprCompile{codegen}, cmd.cond);
  builder.CreateCondBr(cond, body_bb, after_bb);

  // body block
  BasicBlock *continue_tmp = continue_bb;
  BasicBlock *break_tmp = break_bb;
  continue_bb = incr_bb;
  break_bb = after_bb;

  builder.SetInsertPoint(body_bb);
  std::visit(*this, *cmd.body);
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateBr(incr_bb);
  }

  // increment
  builder.SetInsertPoint(incr_bb);
  std::visit(ExprCompile{codegen}, cmd.incr);
  builder.CreateBr(cond_bb);

  continue_bb = continue_tmp;
  break_bb = break_tmp;

  // after block
  builder.SetInsertPoint(after_bb);
}

void CmdCompile::operator()(WhileCmd &cmd) {
  Function *func = builder.GetInsertBlock()->getParent();

  BasicBlock *cond_bb = BasicBlock::Create(context, "cond", func);
  BasicBlock *body_bb = BasicBlock::Create(context, "body", func);
  BasicBlock *after_bb = BasicBlock::Create(context, "after", func);

  builder.CreateBr(cond_bb);

  // condition block
  builder.SetInsertPoint(cond_bb);
  Value *cond = std::visit(ExprCompile{codegen}, cmd.cond);
  builder.CreateCondBr(cond, body_bb, after_bb);

  // body block
  BasicBlock *continue_tmp = continue_bb;
  BasicBlock *break_tmp = break_bb;
  continue_bb = cond_bb;
  break_bb = after_bb;

  builder.SetInsertPoint(body_bb);
  std::visit(*this, *cmd.body);
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateBr(cond_bb);
  }

  continue_bb = continue_tmp;
  break_bb = break_tmp;

  // after block
  builder.SetInsertPoint(after_bb);
}

void CmdCompile::operator()(ContinueCmd &cmd) { builder.CreateBr(continue_bb); }

void CmdCompile::operator()(BreakCmd &cmd) { builder.CreateBr(break_bb); }

void CmdCompile::operator()(ReturnCmd &cmd) {
  if (cmd.ret) {
    Value *ret = std::visit(ExprCompile{codegen}, *cmd.ret);
    builder.CreateRet(ret);
  } else {
    builder.CreateRetVoid();
  }
}

/******************************************************************************/
// Declarations

void DeclCompile::operator()(ExternDecl &decl) {
  // create closure
  llvm::Type *closure_t = ClosureType::get(context);
  GlobalVariable *closure =
      module.getOrInsertGlobal(decl.id.name + "_closure", closure_t);
  closure->setLinkage(llvm::GlobalValue::InternalLinkage);
  closure->setInitializer(llvm::UndefValue::get(closure_t));
  table.back().emplace(decl.id, closure);

  llvm::Type *ft = ClosureFuncType::get(context, decl.id.type);
  Function *func = Function::Create(
      (FunctionType *)ft, Function::ExternalLinkage, decl.id.name, module);
  auto func_dest = builder.CreateGEP(closure_t, closure, {});
  builder.CreateStore(func, func_dest);
}

void DeclCompile::operator()(FuncDecl &decl) {
  Function *func;
  if (decl.id.name == "main") {
    // main function does not create closure
    FunctionType *ft = FunctionType::get(llvm::Type::getVoidTy(context), false);
    func = Function::Create((FunctionType *)ft, Function::ExternalLinkage,
                            decl.id.name, module);
  } else {
    // other functions create closures
    llvm::Type *closure_t = ClosureType::get(context);
    GlobalVariable *closure =
        module.getOrInsertGlobal(decl.id.name + "_closure", closure_t);
    closure->setLinkage(llvm::GlobalValue::InternalLinkage);
    closure->setInitializer(llvm::UndefValue::get(closure_t));
    table.back().emplace(decl.id, closure);

    llvm::Type *ft = ClosureFuncType::get(context, decl.id.type);
    func = Function::Create((FunctionType *)ft, Function::InternalLinkage,
                            decl.id.name, module);
    auto func_dest = builder.CreateGEP(closure_t, closure, {});
    builder.CreateStore(func, func_dest);
  }

  // function body
  BasicBlock *bb = BasicBlock::Create(context, "entry", func);
  builder.SetInsertPoint(bb);
  table.emplace_back(); // new scope
  for (unsigned i = 0; i < decl.args.size(); i++) {
    Id param = decl.args[i];
    table.back().emplace(param, func->getArg(i));
  }
  for (auto &c : decl.body) {
    std::visit(CmdCompile{codegen}, c);
  }
  // insert return if the function does not return anything
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
  table.pop_back(); // exit scope

  // verify the function
  verifyFunction(*func);
}

void DeclCompile::operator()(LetDecl &decl) {
  llvm::Type *t = TypeCompile::get(context, decl.id.type);
  GlobalVariable *global = module.getOrInsertGlobal(to_string(decl.id), t);
  global->setLinkage(llvm::GlobalValue::InternalLinkage);
  global->setInitializer(llvm::UndefValue::get(t));
  table.back().emplace(decl.id, global);
  Value *init = std::visit(ExprCompile{codegen}, decl.expr);
  builder.CreateStore(init, global);
}

void DeclCompile::operator()(VarDecl &decl) {
  llvm::Type *t = TypeCompile::get(context, decl.id.type);
  GlobalVariable *global = module.getOrInsertGlobal(to_string(decl.id), t);
  global->setLinkage(llvm::GlobalValue::InternalLinkage);
  global->setInitializer(llvm::UndefValue::get(t));
  table.back().emplace(decl.id, global);
  if (decl.expr) {
    Value *init = std::visit(ExprCompile{codegen}, *decl.expr);
    builder.CreateStore(init, global);
  }
}

/******************************************************************************/
// CodeGen

CodeGen::CodeGen(const std::string &module_name, int opt_level, Prog &prog)
    : context(LLVMContext()), module{module_name, context}, builder{context},
      table{}, FAM{}, LAM{}, CGAM{}, MAM{} {
  // initialize global constructor
  Function *global_init =
      Function::Create(FunctionType::get(llvm::Type::getVoidTy(context), false),
                       Function::InternalLinkage, "global_init", module);
  global_bb = BasicBlock::Create(context, "entry", global_init);
  llvm::appendToGlobalCtors(module, global_init, 0);

  // declare GC_malloc
  allocator = Function::Create(
      FunctionType::get(PointerType::get(context, 0),
                        {IntegerType::get(context, 64)}, false),
      Function::ExternalLinkage, "GC_malloc", module);

  // initialize all targets
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto CPU = "generic";
  auto Features = "";
  auto TargetTriple = sys::getDefaultTargetTriple();
  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  TargetOptions opt;
  TargetMachine = Target->createTargetMachine(Triple(TargetTriple), CPU,
                                              Features, opt, Reloc::PIC_);
  module.setDataLayout(TargetMachine->createDataLayout());

  // setup optimization passes
  PassBuilder PB;
  PB.registerLoopAnalyses(LAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerModuleAnalyses(MAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  // set optimization level
  OptimizationLevel llvm_opt_level;
  switch (opt_level) {
  case 0:
    llvm_opt_level = OptimizationLevel::O0;
    break;
  case 1:
    llvm_opt_level = OptimizationLevel::O1;
    break;
  case 2:
    llvm_opt_level = OptimizationLevel::O2;
    break;
  case 3:
    llvm_opt_level = OptimizationLevel::O3;
    break;
  }

  // build optimization pipeline
  MPM = PB.buildPerModuleDefaultPipeline(llvm_opt_level);

  // compile declarations
  DeclCompile compile(this);

  table.emplace_back(); // global scope
  for (auto &decl : prog.decls) {
    builder.SetInsertPoint(global_bb);
    std::visit(compile, decl);
  }
  builder.SetInsertPoint(global_bb); // finalize global init
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
  table.pop_back(); // exit global scope

  // verify and optimize the module
  verifyModule(module);
  MPM.run(module, MAM);
}

void CodeGen::emit(const std::string &output) {
  module.print(llvm::outs(), nullptr);

  // emit object file
  auto Filename = output;
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    exit(1);
  }

  legacy::PassManager pass;
  auto FileType = CodeGenFileType::ObjectFile;

  if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    exit(1);
  }

  pass.run(module);
}

} // namespace codegen
} // namespace implang